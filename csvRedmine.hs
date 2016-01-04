-- Headers ------------------------------------------------------------- {{{

{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

import           Control.Monad.Trans(MonadIO, liftIO)
import           Data.Monoid((<>))
import           Data.String(fromString)
import           Data.Text(Text)
-- import           Data.Vector(Vector)

import           Control.Applicative
import qualified Control.Lens               as L
import           Control.Lens.Operators
import qualified Control.Monad.Reader       as MR
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Lens            as A
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Char8      as BS8
import qualified Data.Conduit               as C
import qualified Data.Conduit.Binary        as CB
import qualified Data.Conduit.List          as CL
import qualified Data.Csv                   as CSV
import qualified Data.Csv.Conduit           as CSV
import qualified Data.Data                  as D
import qualified Data.HashMap.Strict        as DHS
import qualified Data.Maybe                 as M
import qualified Data.Text                  as T
-- import qualified Data.Vector              as V
import qualified Network.Connection         as N
import qualified Network.HTTP.Client        as N
import qualified Network.HTTP.Client.Internal as N
import qualified Network.HTTP.Client.TLS    as N
import qualified Network.Wreq               as W
import qualified Network.Wreq.Session       as WS
import qualified System.Console.CmdLib      as Cm
import qualified System.IO                  as IO
import qualified Text.Regex.Posix           as Re


-- import           Data.Map(Map)
-- import qualified Data.Map             as M
-- import qualified Debug.Trace              as DT

------------------------------------------------------------------------ }}}
-- Early Type Definitions----------------------------------------------- {{{
-- These are fronted due to TemplateHaskell ordering requirements.

-- | See `withRedmine`
data RedmineInfo = RI
  { _ri_projId      :: Integer
  , _ri_trackId     :: Integer
  , _ri_cf_areas    :: Integer
  , _ri_cf_citizen  :: Integer
  , _ri_cf_faculty  :: Integer
  , _ri_cf_gre      :: Integer
  , _ri_cf_insts    :: Integer
  , _ri_cf_jhuAppId :: Integer
  , _ri_cf_pdfURL   :: Integer
  , _ri_cf_toefl    :: Integer
  , _ri_cf_triageA  :: Integer
  , _ri_cf_triageB  :: Integer
  }
$(L.makeLenses ''RedmineInfo)

------------------------------------------------------------------------ }}}
-- CSV Data ------------------------------------------------------------ {{{

{- We are at present ignoring these fields:
 -   IELTS_BAND_SCORE, IELTS_LISTENING, IELTS_SPEAKING, IELTS_READING, IELTS_WRITING
 -}

--- CSV Applicant Data Definition -------------------------------------- {{{

-- These records approximate what we get out of the CSV file.  If its schema
-- changes over time, this is the right place to start changing this script.

data GREText = GREText
  { gre_quant  :: (Text,Text)
  , gre_verbal :: (Text,Text)
  , gre_awa    :: (Text,Text)
  }
 deriving (Eq,Ord,Show)

defGREText :: GREText
defGREText = GREText ("","") ("","") ("","")

data TOEFLText = TOEFLText
  { toefl_cbt_pbt :: Text
  , toefl_total   :: Text
  , toefl_listen  :: Text
  , toefl_read    :: Text
  , toefl_speak   :: Text
  , toefl_swrite  :: Text
  }
 deriving (Eq,Ord,Show)

defTOEFLText :: TOEFLText
defTOEFLText = TOEFLText "" "" "" "" "" ""

data CSVApplicant = CSVApplicant
  { _ca_jhuAppId   :: Text
  , _ca_pdfURL     :: Text
  , _ca_name_first :: Text
  , _ca_name_last  :: Text
  , _ca_gender     :: Text
  , _ca_email      :: Text
  , _ca_citizen    :: Text
  , _ca_area1      :: Text
  , _ca_area2      :: Text
  , _ca_faculty    :: Text
  , _ca_insts      :: [(Text,Text,Text)]
  , _ca_gre        :: [GREText]
  , _ca_toefl      :: TOEFLText
  , _ca_reviewer1  :: Text -- Email, not name
  , _ca_reviewer2  :: Text
  }
 deriving (Eq,Ord,Show)
$(L.makeLenses ''CSVApplicant)

defCSVApplicant :: CSVApplicant
defCSVApplicant = CSVApplicant "" "" "" "" "" "" "" "" "" "" [] []
                               defTOEFLText "" ""

------------------------------------------------------------------------ }}}
--- CSV Applicant Data Parser ------------------------------------------ {{{

-- The way this works is that we start off with the default applicant
-- structure, namely 'defCSVApplicant' and fill in pieces of it as we go.
-- Each piece is feched by a Lens (but don't worry about it too much),
-- which names the field and an action which actually gets the thing we
-- want.  'grab' here is an alias for the named field projection which
-- uses the column headings from the CSV file.  The actual data from which
-- we are projecting ('m') is closed over in 'grab'; don't be alarmed that
-- it's not overtly present in each line.
instance CSV.FromNamedRecord CSVApplicant where
  parseNamedRecord m = pure defCSVApplicant
     >>= (ca_jhuAppId   $ grab "Applicant Client ID")
     >>= (ca_pdfURL     $ grabDef "" "Box URL")
     >>= (ca_name_first $ grab "First Name")
     >>= (ca_name_last  $ grab "Last Name")
     >>= (ca_gender     $ grab "Gender")
     >>= (ca_email      $ grab "Email")
     >>= (ca_area1      $ grab "Area of Interest")
     >>= (ca_area2      $ grab "Secondary Area of Interest")
     >>= (ca_faculty    $ {- fmap (T.split (== ',')) . -} grab "Faculty of Interest")
     >>= (ca_citizen    $ grab "Citizenship")
     >>= (ca_reviewer1  $ grabDef "" "Reviewer A Email")
     >>= (ca_reviewer2  $ grabDef "" "Reviewer B Email")
     >>= (ca_insts      $ \_ ->
             (filter (/= ("","","")))
             <$> sequence
                 [ (,,) <$> grab' "Inst 1" <*> grab' "Deg 1" <*> grab' "GPA1"
                 , (,,) <$> grab' "Inst 2" <*> grab' "Deg 2" <*> grab' "GPA2"
                 , (,,) <$> grab' "Institution Name3" <*> grab' "Degree Awarded3" <*> grab' "GPA3"
                 ])
     >>= (ca_gre        $ \_ ->
             (filter (/= GREText ("","") ("","") ("","")))
             <$> sequence
                 [ GREText <$> ((,) <$> grab' "QUANTITATIVE_SCORE"   <*> grab' "Quantitative Percent")
                           <*> ((,) <$> grab' "VERBAL_SCORE"         <*> grab' "GRE Verbal Percent")
                           <*> ((,) <$> grab' "AWA_SCORE"            <*> grab' "Analytical Writing Percent")
                 , GREText <$> ((,) <$> grab' "QUANTITATIVE_SCORE 2" <*> grab' "Quantitative Percent 2")
                           <*> ((,) <$> grab' "VERBAL_SCORE 2"       <*> grab' "GRE Verbal Percent 2")
                           <*> ((,) <$> grab' "AWA_SCORE 2"          <*> grab' "Analytical Writing Percent 2")
                 ])
     >>= (ca_toefl      $ \_ ->
             TOEFLText <$> grab' "TOEFL_CBT_PBT"
                       <*> grab' "TOEFL_TOTAL_SCORE"
                       <*> grab' "TOEFL_LISTENING_SCORE"
                       <*> grab' "TOEFL_READING_SCORE"
                       <*> grab' "TOEFL_SPEAKING_SCORE"
                       <*> grab' "TOEFL_STRUCTURE_WRTG_SCORE")

    where
     grab'    f   = m CSV..: f
     grabOpt' f   = optional (grab' f)

     grab     f _ = grab' f
     -- grabOpt  f _ = grabOpt' f

     grabDef d f _ = fmap (maybe d id) (grabOpt' f)

showCSVError :: CSV.CsvParseError -> String
showCSVError (CSV.IncrementalError e) = "Incremental CSV parse error: " ++ e
showCSVError (CSV.CsvParseError bs e) = "CSV Parse error: " ++ (show bs) ++ ":" ++ e

------------------------------------------------------------------------ }}}
------------------------------------------------------------------------ }}}
-- Redmine Applicant Data ---------------------------------------------- {{{

-- This approximates the Redmine schema.  If that changes over time, this is
-- the right place to start changing this file.  Note that these are
-- parameterized on the type of reviewers (and, perhaps, should be
-- parameterized on other things too); reason being that we need to go to
-- Redmine and ask it for the identifiers of users.  So we move from email
-- text to numerics at some point (see 'lookupReviewers')

data RedmineApplicant uid = RedmineApplicant
  { _ra_jhuAppId   :: Text
  , _ra_pdfURL     :: Text
  , _ra_subject    :: Text  -- formed from name fields
  , _ra_citizen_us :: Bool
  , _ra_areas      :: [Text]  -- formed from area fields
  , _ra_faculty    :: Text
  , _ra_insts      :: Text  -- formed from inst field
  , _ra_gre        :: Text  -- formed from GRE data
  , _ra_toefl      :: Text  -- formed from TOEFL data
  , _ra_reviewer1  :: uid 
  , _ra_reviewer2  :: uid
  }
 deriving (Eq,Ord,Show)
$(L.makeLenses ''RedmineApplicant)

--- Redmine Applicant From CSV ----------------------------------------- {{{

-- Here's our schema conversion, mostly.

csvToRedmine :: CSVApplicant -> RedmineApplicant Text
csvToRedmine ca = RedmineApplicant
  { _ra_jhuAppId   = _ca_jhuAppId ca
  , _ra_pdfURL     = _ca_pdfURL ca
  , _ra_faculty    = _ca_faculty ca
  , _ra_reviewer1  = _ca_reviewer1 ca -- copy across emails
  , _ra_reviewer2  = _ca_reviewer2 ca

  , _ra_citizen_us = _ca_citizen ca == "U.S. Citizen"

  , _ra_subject    = T.concat [ _ca_name_last ca, ", ", _ca_name_first ca ]
  , _ra_areas      = [ _ca_area1 ca, _ca_area2 ca ]

  , _ra_insts      = T.intercalate "; "
                   $ flip map (_ca_insts ca)
                   $ \(i,d,g) -> T.concat [mkQ i, " (", mkQ d, ": ", mkQ g, ")"]

  , _ra_gre        = T.intercalate "; "
                   $ flip map (_ca_gre ca)
                   $ \(GREText (qs,qp) (vs,vp) (ws,wp)) -> 
                      T.concat [mkQ qs, "M " , mkQ vs, "V " , mkQ ws, "AW ("
                               ,mkQ qp, "%, ", mkQ vp, "%," , mkQ wp, "%)"]

  , _ra_toefl      = ($ _ca_toefl ca) $ \(TOEFLText cbt t l r s w) ->
                     T.concat [t, ": ", mkQ l, "(Listening), ", mkQ r, "(Reading), "
                                      , mkQ s, "(Speaking), " , mkQ w, "(Writing) "
                                      , toeflty cbt]
  }
 where
   mkQ x = if x == "" then "??" else x

   toeflty :: Text -> Text
   toeflty t = case (T.unpack t) Re.=~ ("^[^(]*(\\([^)]+\\))$" :: String) of
                 (_ :: String, _ :: String, _ :: String, [t']) -> T.pack t'
                 _                                             -> t


-- XXX
--- testCSV = IO.withFile "test.csv" IO.ReadMode $ \f -> do
---             fc <- BL.hGetContents f
---             let Right (_, d) = decodeCSV fc
---             print (V.map csvToRedmine d)

------------------------------------------------------------------------ }}}
------------------------------------------------------------------------ }}}
-- Redmine API Details ------------------------------------------------- {{{

data RestTD = RTD
  { _rtd_sess       :: WS.Session
  , _rtd_opts       :: W.Options
  , _rtd_base       :: String
  , _rtd_show_resps :: W.Response BL.ByteString -> IO ()
  }
$(L.makeLenses ''RestTD)

-- | Package up most things we need to make ReSTful queries
newtype RestT m a = RestT { runRestT :: MR.ReaderT RestTD m a }
 deriving (Applicative,Functor,Monad,MonadIO,MR.MonadReader RestTD)

--- Redmine API Query Core --------------------------------------------- {{{

-- Return the entire response body as well as a descended piece of it
redmineQuery :: MonadIO m
             => IO (W.Response BL.ByteString)
             -> Text
             -> m (A.Value, A.Value)
redmineQuery q d = do
  resp :: W.Response A.Value <- liftIO (W.asJSON =<< q)
  let rb = resp ^. W.responseBody
  case rb ^? A.key d of
      Nothing -> error $ "Invalid Redmine response? " ++ (show resp)
      Just value -> pure (rb, value)

-- Sometimes we need to make Redmine queries for lists of things.  Sometimes
-- those things have paged interfaces because there are lots of them; the
-- 'redmineListCursor' version here manages the offset transparently.
-- Hooray.
redmineList, redmineListCursor
  :: MonadIO m
  => String      -- ^ URL suffix
  -> Text        -- ^ Field to look for in Redmine response
  -> C.ConduitM i A.Value (RestT m) ()
redmineList u d = do
  RTD s o b vp <- MR.ask
  (_, value) <- redmineQuery (WS.getWith o s (b ++ u) >>= \resp -> vp resp >> pure resp) d
  CL.sourceList $ value ^.. L.traverseOf A.values
redmineListCursor u d = do
  RTD s o b vp <- MR.ask
  let fetch n = WS.getWith (o & W.param "offset" .~ [T.pack $ show n]) s (b ++ u) 
                >>= \resp -> vp resp >> pure resp
  go fetch 0
  where
   go fetch off = do
    (rb, value) <- redmineQuery (fetch off) d
    CL.sourceList $ value ^.. L.traverseOf A.values
    maybe (error $ "JSON response missing position information: " ++ show rb)
          (\(tc,ooff,olim) -> if ooff + olim < tc then go fetch (ooff+olim) else return ())
     $ do
      total_count <- rb L.^? A.key "total_count" . A._Integer
      offset      <- rb L.^? A.key "offset" . A._Integer
      limit       <- rb L.^? A.key "limit" . A._Integer
      pure (total_count, offset, limit)


------------------------------------------------------------------------ }}}
--- Redmine API Post-Query Filters ------------------------------------- {{{

-- Redmine likes to give us things with identifiers and fields whose
-- value we care about and then expects us to use the identifiers rather
-- than values we care about later.  That stinks.  This is a convenience
-- wrapper that maps us from the values we care about to the identifiers.
withRedmineIdThing :: (MonadIO m, A.AsValue s)
               => Text                     -- ^ Field carrying the name
               -> ([Integer] -> m a)       -- ^ Error callback
               -> C.Conduit () m s         -- ^ Source conduit
               -> Text                     -- ^ Name of thing to look for
               -> (Integer -> m a)
               -> m a
withRedmineIdThing k err cond name f = do
  ids <- C.sourceToList
       $ cond
       C.=$= CL.filter (\p -> Just name == p ^? A.key k . A._String)
       C.=$= CL.mapMaybe (\p -> p ^? A.key "id" . A._Integer)
  case ids of
    [i] -> f i
    _   -> err ids

-- A lot of Redmine things use "name" as the value we care about *and*
-- we don't necessarily care about all the fancy error handling that
-- the generic `withRedmineIdThing` would give us.  This is a simplified
-- interface.
redmineIdNamed :: (MonadIO m, A.AsValue s)
               => ([Integer] -> m Integer) -- ^ Error callback
               -> C.Conduit () m s         -- ^ Source conduit
               -> Text                     -- ^ Name of thing to look for
               -> m Integer
redmineIdNamed err cond name = withRedmineIdThing "name" err cond name pure


------------------------------------------------------------------------ }}}
--- Redmine API Queries ------------------------------------------------ {{{

-- Using the above functions, make some convenience enumerators ("source
-- conduits") for things in Redmine we care about.

redmineProjects, redmineUsers, redmineTrackers, redmineCustomFields, redmineIssues
  :: MonadIO m => C.ConduitM i A.Value (RestT m) ()
redmineProjects     = redmineListCursor "/projects.json"      "projects"
redmineUsers        = redmineListCursor "/users.json"         "users"
redmineIssues       = redmineListCursor "/issues.json"        "issues"
redmineTrackers     = redmineList       "/trackers.json"      "trackers"
redmineCustomFields = redmineList       "/custom_fields.json" "custom_fields"

------------------------------------------------------------------------ }}}
--- Redmine Applicant To JSON ------------------------------------------ {{{

-- Map reviewer emails to possible identifiers
lookupReviewers :: RedmineApplicant Text -> RestT IO (RedmineApplicant (Maybe Integer))
lookupReviewers ra = do
  let findUserMaybe un = if un == ""
                          then pure Nothing
                          else withRedmineIdThing "mail" (pure . M.listToMaybe) redmineUsers un (pure . Just)
  r1id <- findUserMaybe (_ra_reviewer1 ra)
  r2id <- findUserMaybe (_ra_reviewer2 ra)
  pure $ ra { _ra_reviewer1 = r1id, _ra_reviewer2 = r2id }

-- Convert a RemineApplicant, whose reviewers have already been mapped using
-- Redmine, to a JSON representation using the RedmineInfo to get
-- identifiers for custom fields
jsonifyRedmineApp :: RedmineInfo -> RedmineApplicant (Maybe Integer) -> A.Value
jsonifyRedmineApp ri ra = do
  A.object [ "custom_fields" A..= (A.toJSON
             $ addcfm ri_cf_triageA  ra_reviewer1
             $ addcfm ri_cf_triageB  ra_reviewer2
             $ [ mkcf ri_cf_jhuAppId ra_jhuAppId
               , mkcf ri_cf_pdfURL   ra_pdfURL
               , mkcf ri_cf_citizen  ra_citizen_us
               , mkcf ri_cf_areas    ra_areas
               , mkcf ri_cf_faculty  ra_faculty
               , mkcf ri_cf_insts    ra_insts
               , mkcf ri_cf_gre      ra_gre
               , mkcf ri_cf_toefl    ra_toefl
               ])
           , "subject" A..= (ra ^. ra_subject)
           ] 
 where
  addcfm f v  = maybe id (\vv -> ((A.object [ "id" A..= show (ri ^. f), "value" A..= vv ]) :)) (ra ^. v)
  mkcf   f v  = A.object [ "id" A..= show (ri ^. f), "value" A..= (ra ^. v) ]

------------------------------------------------------------------------ }}}
--- Redmine API Updates ------------------------------------------------ {{{

-- Construct a new applicant
redmineNewIssue :: RedmineInfo -> RedmineApplicant (Maybe Integer) -> RestT IO (W.Response BL.ByteString)
redmineNewIssue ri ra = do
  RTD s o u vp <- MR.ask
  let (A.Object fs) = jsonifyRedmineApp ri ra
  let fs' = (DHS.fromList [ "project_id" A..= (_ri_projId ri), "tracker_id" A..= (_ri_trackId ri) ]) <> fs
  liftIO $ WS.postWith o s (u ++ "/issues.json") (A.object ["issue" A..= fs'])
           >>= \resp -> vp resp >> pure resp

-- Update an existing applicant; sends all fields and Redmine sorts it out
redmineUpdateIssue :: RedmineInfo -> Integer -> RedmineApplicant (Maybe Integer) -> RestT IO (W.Response BL.ByteString)
redmineUpdateIssue ri n ra = do
  RTD s o u vp <- MR.ask
  liftIO $ WS.putWith o s (u ++ "/issues/" ++ show n ++ ".json") (A.object ["issue" A..= jsonifyRedmineApp ri ra])
           >>= \resp -> vp resp >> pure resp
------------------------------------------------------------------------ }}}
------------------------------------------------------------------------ }}}
-- withRedmine --------------------------------------------------------- {{{

-- | A wrapper which sets us up to make ReSTful queries against a Redmine
--   instance given the common arguments.
--
--   Makes a boatload of requests itself to figure out common parameters
--   of the Redmine setup used by CLSP.  Hooray.
withRedmine :: ArgCommon -> (RedmineInfo -> RestT IO ()) -> IO ()
withRedmine ac cb = do
    let rurl = ac_redmineURL ac
    let tlss0 = if ac_cert_check ac
                 then N.tlsManagerSettings
                 else N.mkManagerSettings (N.TLSSettingsSimple True False False) Nothing
    let tlss  = if ac_debug ac < 2
                 then tlss0
                 else tlss0 { N.managerTlsConnection = pure $ \ha h p -> do
                                mkTLSC <- N.managerTlsConnection tlss0
                                c <- mkTLSC ha h p
                                pure c { N.connectionWrite = \bs -> do
                                           mapM_ (\bsl -> do
                                                    IO.hPutStr IO.stderr "> "
                                                    BS8.hPutStrLn IO.stderr bsl)
                                                 (BS8.lines bs)
                                           N.connectionWrite c bs
                                       }
                            }
    let vp = if ac_debug ac < 2
              then \_ -> pure ()
              else print
    let wreqOpts = W.defaults & W.auth .~ Just (W.basicAuth (fromString $ ac_redmineAuth ac) "")
    WS.withSessionControl Nothing tlss $ \sess -> flip MR.runReaderT (RTD sess wreqOpts rurl vp) $ runRestT $ do

        -- We need to look up a whole lot of custom fields; since there aren't
        -- that many of them, grab them all into RAM now and scan locally rather
        -- than making repeated queries of the server.
        customFields <- C.sourceToList redmineCustomFields

        -- Build redmine configuration information to pass to callback
        ri <- RI
               -- project
           <$> redmineIdNamed rerr (redmineProjects) (fromString $ ac_redmineProjName ac)
               -- tracker
           <*> redmineIdNamed rerr (redmineTrackers) "Applicant"
               -- custom fields
           <*> redmineIdNamed rerr (CL.sourceList customFields) "Research Areas"
           <*> redmineIdNamed rerr (CL.sourceList customFields) "US Citizen"
           <*> redmineIdNamed rerr (CL.sourceList customFields) "Faculty of Interest"
           <*> redmineIdNamed rerr (CL.sourceList customFields) "GRE Scores"
           <*> redmineIdNamed rerr (CL.sourceList customFields) "Prior Institutions"
           <*> redmineIdNamed rerr (CL.sourceList customFields) "JHU Applicant ID"
           <*> redmineIdNamed rerr (CL.sourceList customFields) "PDF Application"
           <*> redmineIdNamed rerr (CL.sourceList customFields) "TOEFL Score"
           <*> redmineIdNamed rerr (CL.sourceList customFields) "Triage A"
           <*> redmineIdNamed rerr (CL.sourceList customFields) "Triage B"

        cb ri
 where
  rerr [] = error "Could not find redmine project by name."
  rerr _  = error "Redmine project name is not unique."

------------------------------------------------------------------------ }}}
-- CSV Upsert ---------------------------------------------------------- {{{

-- Insert or Update an applicant based on CSV data

data ArgUpsert = ArgUpsert { upsert_clobber :: Bool }
 deriving (D.Data,Eq)

instance Cm.Attributes ArgUpsert where
  attributes _ = Cm.group "Upsert Options" 
    [ upsert_clobber
        Cm.%> [ Cm.Short "x" , Cm.Long ["clobber"]
              , Cm.Help "Overwrite existing records" ]
    ]

data RedmineUpsert = RedmineUpsert ArgCommon
 deriving (D.Data,Eq)

instance Cm.Command RedmineUpsert (Cm.Record ArgUpsert) where
  cmdname _ = "upsert"
  run (RedmineUpsert ac) au _ = withRedmine ac $ \ri -> do
    -- A little ahead of the game, but... once we've got an applicant
    -- 'what', we should...
    let process what = do
          let appid = _ra_jhuAppId what
          -- ... see if they already exist in Redmine ...
          raids <- MR.local (rtd_opts %~ ( (W.param "project_id" .~ [T.pack $ show $ _ri_projId ri])
                                         . (W.param "tracker_id" .~ [T.pack $ show $ _ri_trackId ri])
                                         . (W.param (fromString ("cf_" ++ show (_ri_cf_jhuAppId ri)))
                                                                 .~ [appid])))
                 $ C.sourceToList
                 $ redmineIssues 
                 C.=$= CL.mapMaybe (^? A.key "id" . A._Integer)

          -- ... and respond appropriately by...
          case (raids, upsert_clobber au) of
                                   -- complaining if there's a problem
            (_:_:_, _)          -> liftIO $ IO.hPutStrLn IO.stdout
                                     ("Multiple applicants with the same ID? " ++ show appid)
                                   -- ... creating a new issue if they
                                   -- don't already exist
            ([], _)             -> redmineNewIssue ri what >> pure ()
                                   -- ... clobbering the existing record
                                   -- if they already do and we've been told
                                   -- to clobber ...
            ([applicant], True) -> redmineUpdateIssue ri applicant what >> pure ()
                                   -- ... or complaining otherwise.
            ([_], False)        -> liftIO $ IO.hPutStrLn IO.stdout
                                     ("Not clobbering existing applicant" ++ show appid)

    -- Alright, here's our pipeline; the heart of the upsert command.  We
    -- start by...
    C.runConduit $
            -- ... reading stdin as chunks of bytes ...
            CB.sourceHandle IO.stdin

            -- ... converting those chunks of bytes to CSVApplicants ...
      C.=$= CSV.fromNamedCsvStreamError CSV.defaultDecodeOptions

            -- ... filtering out any CSV errors and showing them ...
      C.=$= CL.mapMaybeM (either ((*> pure Nothing) . printErr) (pure . Just))

            -- ... purely mapping schemas ...
      C.=$= CL.map csvToRedmine

            -- ... using Redmine to convert reviewers ...
      C.=$= CL.mapM lookupReviewers

            -- ... and lastly using the above "process" function
      C.=$= CL.mapM_ process
   where
    printErr = liftIO . IO.hPutStrLn IO.stdout . showCSVError
    
------------------------------------------------------------------------ }}}
-- Argument Parsing and main function ---------------------------------- {{{

-- Common argument parsing.  Nothing terribly fancy.

redmine_commands :: ArgCommon -> [Cm.CommandWrap]
redmine_commands f = Cm.commandGroup "Redmine Command" (RedmineUpsert f)

data ArgCommon = ArgCommon
  { ac_redmineURL      :: String
  , ac_redmineAuth     :: String
  , ac_redmineProjName :: String
  , ac_cert_check      :: Bool
  , ac_debug           :: Int
  }
 deriving (D.Data,Eq,Show)

instance Cm.Attributes ArgCommon where
  attributes _ = Cm.group "Common Options"
    [ ac_redmineURL
        Cm.%> [ Cm.Short "h" , Cm.Long ["url", "host"]
              , Cm.Help "Base to redmine server", Cm.ArgHelp "URL" ]
    , ac_redmineAuth
        Cm.%> [ Cm.Short "au", Cm.Long ["auth", "user"]
              , Cm.Help "Authentication token"  , Cm.ArgHelp "STRING" ]
    , ac_redmineProjName
        Cm.%> [ Cm.Short "p" , Cm.Long ["project"]
              , Cm.Help "Project Name"          , Cm.ArgHelp "STRING" ]
    , ac_cert_check
        Cm.%> [ Cm.Short "C" , Cm.Long ["cert"], Cm.Default False
              , Cm.Help "certificate checking"  ]
    , ac_debug
        Cm.%> [ Cm.Short "v" , Cm.Long ["debug"], Cm.Default (0 :: Int)
              , Cm.Help "verbosity"             , Cm.ArgHelp "LEVEL" ]
    ]

data RedmineCommand = RedmineCommand deriving (D.Typeable,Eq)

instance Cm.Command RedmineCommand (Cm.Record ArgCommon) where
  summary      _ = "Interact with Redmine"
  help         _ = Cm.helpCommands (redmine_commands undefined)
  cmdname      _ = "redmine"
  supercommand _ = True
  synopsis     _ = "redmine [COMMON OPTIONS] [REDMINE COMMAND] ..."
  run _ f opts   = Cm.dispatch [] (redmine_commands f) opts

main :: IO ()
main = Cm.getArgs >>= Cm.execute (Cm.cmd :: RedmineCommand)

------------------------------------------------------------------------ }}}
-- vim: foldmethod=marker:ts=2
