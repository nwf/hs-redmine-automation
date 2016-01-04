-- An example of processing Redmine issues in a programmatic way.
--
-- Dependencies can be installed by running
--
--   cabal install aeson lens lens-aeson conduit connection \
--                 http-client-tls parseargs
--
-- Then invoke this script as
--
--   runghc ./redmine-test.hs -u ${USERNAME} -p ${PASSWORD} \
--          -h clsp-redmine.cs.jhu.edu -Q [yield|stragglers] \
--          -P ${PROJECT}
--
-- The -h is optional and may be set to, e.g., "localhost:12345" if
-- you are engaged in port-forwarding to reach the web server.
--
-- The -P is optional, but should probably be given so that issues
-- from test projects and such do not show up.  That means
-- "-P clsp-admissions-2015", for example.
--
-- The queries built in thus far are for the expected yield of all
-- post-triage applicants ("yield") and for a list of stragglers who
-- have good triage marks but are not yet marked as having passed
-- triage ("stragglers").

---- Header material -------------------------------------------------- {{{

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Lens            as L
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.Aeson              as A
import qualified Data.Aeson.Lens         as A
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8   as B8
import qualified Data.Conduit            as C
import qualified Data.Conduit.List       as C
import qualified Data.Map                as M
import           Data.Maybe (catMaybes)
import           Data.Text(Text)
import           Data.Void
import           Network.Connection
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           System.Console.ParseArgs

------------------------------------------------------------------------ }}}
---- Argument parsing ------------------------------------------------- {{{

data ArgIx = AIHostname
           | AIProject
           | AIUsername
           | AIPassword
           | AIQuery
 deriving (Eq,Ord,Show)

argl :: [ Arg ArgIx ]
argl = [ Arg { argIndex = AIUsername,
               argName = Just "user",
               argAbbr = Just 'u',
               argData = argDataRequired "User" ArgtypeString,
               argDesc = "Redmine User Name" }
       , Arg { argIndex = AIPassword,
               argName = Just "password",
               argAbbr = Just 'p',
               argData = argDataRequired "Pass" ArgtypeString,
               argDesc = "Redmine Password" }
       , Arg { argIndex = AIHostname,
               argName = Just "host",
               argAbbr = Just 'h',
               argData = argDataDefaulted "host" ArgtypeString
                           "clsp-redmine.cs.jhu.edu",
               argDesc = "Redmine host name and port" }
       , Arg { argIndex = AIProject,
               argName = Just "proj",
               argAbbr = Just 'P',
               argData = argDataOptional "project" ArgtypeString,
               argDesc = "Redmine project identifier" }
       , Arg { argIndex = AIQuery,
               argName = Just "query",
               argAbbr = Just 'Q',
               argData = argDataDefaulted "query" ArgtypeString "yield",
               argDesc = "Which query pipeline to run" }
       ]

------------------------------------------------------------------------ }}}
---- Redmine query core ------------------------------------------------ {{{

doReq :: (Functor m, MonadIO m)
      => Args ArgIx
      -> Manager
      -> Integer
      -> m ByteString
doReq a m off = do
    initReq <- liftIO $ parseUrl
                      $ "https://"
                          ++ (getRequiredArg a AIHostname)
                          ++ "/issues.json"
    fmap responseBody $ liftIO $ flip httpLbs m
           $ applyBasicAuth
              (B8.pack $ getRequiredArg a AIUsername)
              (B8.pack $ getRequiredArg a AIPassword)
           $ setQueryString
               (catMaybes
               [ Just ("offset", Just $ B8.pack $ show off)
            -- ,  Just ("limit", Just "10") 
               , Just ("tracker_id", Just "4") -- i.e. "Applicant"
               , fmap (\x -> ("project_id", Just $ B8.pack x))
                      (getArg a AIProject)
               ])
           $ initReq

-- | Stream issues out of redmine, managing its cursor
issuesSource :: MonadIO m
             => (Integer -> IO ByteString)
             -> C.ConduitM i A.Value m ()
issuesSource fetch = go 0
 where
  go off = do
    -- liftIO $ putStrLn $ "Making request at offset " ++ show off
    resp <- liftIO $ fetch off
    let jresp = A.eitherDecode resp
    case jresp of
      Left s -> error $ "JSON error at offset " ++ show off ++ ": " ++ s
      Right (obj :: A.Value) ->
        case obj L.^? A.key "issues" of
          Nothing -> error "No issues returned?"
          Just issues -> do
            C.sourceList $ issues L.^.. L.traverseOf A.values
            maybe (error "JSON response missing position information")
                  (\(tc,o,l) -> if o + l < tc then go (o+l) else return ())
             $ do
              total_count <- obj L.^? A.key "total_count" . A._Integer
              offset      <- obj L.^? A.key "offset" . A._Integer
              limit       <- obj L.^? A.key "limit" . A._Integer
              return (total_count, offset, limit)

------------------------------------------------------------------------ }}}
---- HTTPS manager helper ---------------------------------------------- {{{

myWithManager :: forall a. (Manager -> IO a) -> IO a
myWithManager = withManager
                 (mkManagerSettings
                   (TLSSettingsSimple True True True)
                   Nothing)

------------------------------------------------------------------------ }}}
---- Utility queries for manipulating Redmine JSON---------------------- {{{

-- | Partial function & assumes "Int" big enough.
unsafeGetID :: A.Value -> Int
unsafeGetID v = maybe (error "Issue without id in stream!") id
                    $ v L.^? A.key "id" . A._Integral

unsafeGetSubject :: A.Value -> Text
unsafeGetSubject v = maybe (error "Issue without subject in stream!") id
                         $ v L.^? A.key "subject" . A._String

nameIs :: A.Value -> Text -> Bool
nameIs o x = o L.^? A.key "name" == Just (A.String x)

getCustomDouble :: A.Value -> Text -> Maybe Double
getCustomDouble o x = o L.^? A.key "custom_fields"
                         . L.traverseOf A.values
                         . L.filtered (`nameIs` x)
                         . A.key "value"
                         . A._String . A._Double

passedTriage :: A.Value -> Bool
passedTriage o = 
  o L.^? A.key "status" . A.key "name"
    `elem` map (Just . A.String) ["Passed Triage", "Shortlisted"]

inTriage :: A.Value -> Bool
inTriage o = o L.^? A.key "status" . A.key "name" == Just (A.String "In Triage")

------------------------------------------------------------------------ }}}
---- Code we care about : Expected yield pipeline ---------------------- {{{

data ExpectedYieldSummary = EYS
                              !Int     -- number of applicants
                              !Double  -- summed expected yield
                              ![Int]   -- list of applicant IDs without EYs

summarizeExpectedYield :: Monad m => C.Sink A.Value m ExpectedYieldSummary
summarizeExpectedYield = flip C.fold
     (EYS 0 0 [])           -- starting summary value
   $ \(EYS n ey bs) v ->    -- function to merge summary and new issue
      case extractExpectedYield v of
          -- Try to be nice and report the ID of any issues that
          -- do not have parsable expected yields, rather than
          -- just bailing or ignoring them.
        Nothing -> EYS (n+1) ey ((unsafeGetID v):bs)
        Just eyc -> EYS (n+1) (ey+eyc) bs
 where
  extractExpectedYield o = o `getCustomDouble` "Expected Yield" 

displayExpectedYieldSummary :: MonadIO m => ExpectedYieldSummary -> m ()
displayExpectedYieldSummary (EYS n_suitable ey no_eys) = liftIO $ do
  putStrLn $ "There are "
             ++ show n_suitable
             ++ " applicants post-triage, with combined expected yield "
             ++ show ey
             ++ "."
  when (no_eys /= []) $ do
    putStrLn $ "The following applicants had no parseable expected yield: "
    putStrLn $ show no_eys
    putStrLn $ "  The sum above was computed from the remaining "
               ++ show (n_suitable - length no_eys)
               ++ "."

expectedYieldPipeline :: String -> C.ConduitM A.Value Void IO ()
expectedYieldPipeline _ = C.filter passedTriage
                        C.=$= (summarizeExpectedYield >>=
                               displayExpectedYieldSummary)

------------------------------------------------------------------------ }}}
---- Code we care about : Good marks and not flagged as past triage ---- {{{

scoreOK :: Text -> A.Value -> Bool
scoreOK scf app = maybe False (>= 4) $ app `getCustomDouble` scf

stragglerPipeline :: String -> C.ConduitM A.Value Void IO ()
stragglerPipeline urlbase =
  C.filter inTriage
  C.=$= C.filter (\x -> and $ map ($ x)
                        [ scoreOK "Triage A Score"
                        , scoreOK "Triage B Score"])
  C.=$= printStragglers
 where
  printStragglers = do
    liftIO $ putStrLn "These applicants should be marked as post-triage:"
    C.mapM_ $ \app -> liftIO $ do
      putStrLn $ "  "
                  ++ (show $ unsafeGetSubject app)
                  ++ " ( https://" ++ urlbase ++ "/"
                  ++ (show $ unsafeGetID app)
                  ++ " )"

------------------------------------------------------------------------ }}}
---- Code we care about : Pipelines in a map --------------------------- {{{

pipelines :: M.Map String (String -> C.ConduitM A.Value Void IO ())
pipelines = M.fromList $
  [ ("yield",  expectedYieldPipeline)
  , ("stragglers", stragglerPipeline)
  ]

------------------------------------------------------------------------ }}}
---- Main -------------------------------------------------------------- {{{

main :: IO ()
main = do
  parsed_args <- parseArgsIO ArgsComplete argl
  let pl = maybe (error $ "Huh?  Try asking for one of these instead:\n "
                          ++ (show $ M.keys pipelines))
                 id
               $ M.lookup (getRequiredArg parsed_args AIQuery) pipelines
  myWithManager $ \manager ->
       C.runConduit $ issuesSource (doReq parsed_args manager)
                      C.=$= (pl (getRequiredArg parsed_args AIHostname))


------------------------------------------------------------------------ }}}

-- vim: foldmethod=marker:ts=2
