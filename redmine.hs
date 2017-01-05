-- Headers ------------------------------------------------------------- {{{
{-# LANGUAGE DataKinds, DeriveDataTypeable, FlexibleContexts,
             FlexibleInstances, GADTs,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             RankNTypes, OverloadedStrings, ScopedTypeVariables,
             StandaloneDeriving, TemplateHaskell, TypeFamilies,
             TupleSections, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Main(main) where

import           Control.Exception(handle)
import           Control.Monad(join,when)
import           Control.Monad.Trans(MonadIO, liftIO, lift)
import           Data.Monoid((<>))
import           Data.String(fromString)
import           Data.Text(Text)
import           System.IO(stderr)
import           Text.Read(readMaybe)

import           Control.Applicative
import qualified Control.Monad.Reader         as MR
import qualified Control.Monad.Base           as MC
import qualified Control.Monad.Except         as ME
import qualified Control.Monad.Trans.Control  as MC
import qualified Data.Aeson                   as A
import qualified Data.Aeson.Lens              as A
import qualified Data.ByteString.Char8        as BS8
import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString.Lazy.Char8   as BL8
import qualified Data.Char                    as DC
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.List            as CL
import qualified Data.Csv                     as CSV
import qualified Data.Csv.Conduit             as CSV
import qualified Data.Default                 as Def
import qualified Data.HashMap.Strict          as DHS
import qualified Data.Maybe                   as M
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import qualified Data.Text.Lazy.Builder       as TLB
import qualified Data.Text.Lazy.IO            as TL
import qualified Data.Word                    as DW
import           Lens.Micro                   as L
import qualified Lens.Micro.TH                as L
import qualified Network.Connection           as N
import qualified Network.HTTP.Client          as N
import qualified Network.HTTP.Client.Internal as N
import qualified Network.HTTP.Client.TLS      as N
import qualified Network.HTTP.Req             as WR
import qualified Options.Applicative          as OA
import qualified System.IO                    as IO
import qualified Text.Regex.Posix             as Re

-- import qualified Debug.Trace                  as DT

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
  , _ra_email      :: Text
  , _ra_pdfURL     :: Text
  , _ra_subject    :: Text  -- formed from name fields
  , _ra_citizen_us :: Bool
  , _ra_areas      :: [Text]  -- formed from area fields
  , _ra_faculty    :: Text
  , _ra_insts      :: Text  -- formed from inst field
  , _ra_gre        :: Text  -- formed from GRE data
  , _ra_toefl      :: Text  -- formed from TOEFL data
  , _ra_reviewer1  :: uid
  , _ra_score1     :: Maybe Integer
  , _ra_reviewer2  :: uid
  , _ra_score2     :: Maybe Integer
  , _ra_assignee   :: uid   -- XXX not in CSV
  }
 deriving (Eq,Ord,Show)
$(L.makeLenses ''RedmineApplicant)

------------------------------------------------------------------------ }}}
-- Redmine Account Data ------------------------------------------------ {{{

data RedmineAccount uid uidtext = RedmineAccount
  { _rac_redmineID   :: uid
  , _rac_redmineName :: uidtext
  , _rac_email       :: Text
  , _rac_firstName   :: Text
  , _rac_lastName    :: Text
  }
 deriving (Eq,Ord,Show)
$(L.makeLenses ''RedmineAccount)

defRedmineAccount :: RedmineAccount () ()
defRedmineAccount = RedmineAccount () () "" "" ""

------------------------------------------------------------------------ }}}
-- CSV Data ------------------------------------------------------------ {{{
--- CSV Applicant Data Definition -------------------------------------- {{{

-- These records approximate what we get out of the CSV file.  If its schema
-- changes over time, this is the right place to start changing this script.

data GREText = GREText
  { _gre_quant  :: (Text,Text)
  , _gre_verbal :: (Text,Text)
  , _gre_awa    :: (Text,Text)
  }
 deriving (Eq,Ord,Show)

defGREText :: GREText
defGREText = GREText ("","") ("","") ("","")

data TOEFLText = TOEFLText
  { _toefl_cbt_pbt :: Text
  , _toefl_total   :: Text
  , _toefl_listen  :: Text
  , _toefl_read    :: Text
  , _toefl_speak   :: Text
  , _toefl_swrite  :: Text
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

{- We are at present ignoring these fields:
 -   IELTS_BAND_SCORE, IELTS_LISTENING, IELTS_SPEAKING, IELTS_READING, IELTS_WRITING
 -}

-- The way this works is that we start off with the default applicant
-- structure, namely 'defCSVApplicant' and fill in pieces of it as we go.
-- Each piece is feched by a Lens (but don't worry about it too much),
-- which names the field and an action which actually gets the thing we
-- want.  'grab' here is an alias for the named field projection which
-- uses the column headings from the CSV file.  The actual data from which
-- we are projecting ('m') is closed over in 'grab'; don't be alarmed that
-- it's not overtly present in each line.
--
-- Sometimes we get spreadsheets without the Reviewer {A,B} Email columns,
-- thus why these fields do not report an error if they're missing.  On the
-- down side, that also means we won't find out if the header has been
-- misspelled, but whatever.
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
                 [ (,,) <$> grab' "Institution 1" <*> grab' "Degree 1" <*> grab' "GPA1"
                 , (,,) <$> grab' "Institution 2" <*> grab' "Degree 2" <*> grab' "GPA2"
                 , (,,) <$> grab' "Institution 3" <*> grab' "Degree 3" <*> grab' "GPA3"
                 ])
     >>= (ca_gre        $ \_ ->
             (filter (/= defGREText))
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

------------------------------------------------------------------------ }}}
--- CSV Account Data Parser -------------------------------------------- {{{

instance CSV.FromNamedRecord (RedmineAccount () ()) where
  parseNamedRecord m = pure defRedmineAccount
    >>= (rac_email       $ grab "Email Address")
    >>= (rac_firstName   $ grab "First Name")
    >>= (rac_lastName    $ grab "Last Name")

   where grab f _ = m CSV..: f

------------------------------------------------------------------------ }}}
------------------------------------------------------------------------ }}}
-- Redmine Applicant From CSV ------------------------------------------ {{{

-- Here's our schema conversion, mostly.

csvToRedmine :: CSVApplicant -> RedmineApplicant Text
csvToRedmine ca = RedmineApplicant
  { _ra_jhuAppId   = _ca_jhuAppId ca
  , _ra_email      = _ca_email ca
  , _ra_pdfURL     = _ca_pdfURL ca
  , _ra_faculty    = _ca_faculty ca
  , _ra_reviewer1  = _ca_reviewer1 ca -- copy across emails
  , _ra_score1     = Nothing
  , _ra_reviewer2  = _ca_reviewer2 ca
  , _ra_score2     = Nothing
  , _ra_assignee   = error "ERR: Initial applicant assignee should not be accessed"

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
                               ,mkQ qp, "%, ", mkQ vp, "%, ", mkQ wp, "%)"]

  , _ra_toefl      = ($ _ca_toefl ca) $ \tt@(TOEFLText cbt t l r s w) ->
                     if tt == defTOEFLText
                      then "" -- if we don't have anything to report, don't!
                      else T.concat
                           [t, ": ", mkQ l, "(Listening), ", mkQ r, "(Reading), "
                                   , mkQ s, "(Speaking), " , mkQ w, "(Writing) "
                                   , toeflty cbt]
  }
 where
   mkQ x = if x == "" then "??" else x

   toeflty :: Text -> Text
   toeflty t = case (T.unpack t) Re.=~ ("^[^(]*(\\([^)]+\\))$" :: String) of
                 (_ :: String, _ :: String, _ :: String, [t']) -> T.pack t'
                 _                                             -> t


------------------------------------------------------------------------ }}}
-- withRedmine and friends --------------------------------------------- {{{
--- RestT Monad Transformer -------------------------------------------- {{{
data RestTD scheme e = RTD
  { _rtd_base        :: WR.Url scheme
  , _rtd_req_config  :: WR.HttpConfig
  , _rtd_req_opts    :: WR.Option scheme
  , _rtd_http_err    :: WR.HttpException -> e
  }

-- | Package up most things we need to make ReSTful queries
newtype RestT scheme e m a = RestT {
  runRestT :: ME.ExceptT e (MR.ReaderT (RestTD scheme e) m) a
 }
 deriving (Applicative,Functor,Monad,MonadIO,
           MR.MonadReader (RestTD scheme e))

$(L.makeLensesFor [("_rtd_req_opts", "rtd_req_opts")
                  ,("_rtd_base", "rtd_base")]
   ''RestTD)

deriving instance (Monad m) => ME.MonadError e (RestT scheme e m)

instance MR.MonadTrans (RestT scheme e) where
  lift x = RestT (ME.lift (MR.lift x))

-- Yikes... we need these to get exception handlers working with our
-- RestT transformer.  Thankfully it's "chant default a lot".
instance (MC.MonadBase b m) => MC.MonadBase b (RestT scheme e m) where
  liftBase = RestT . MC.liftBase

instance MC.MonadTransControl (RestT scheme e) where
  type StT (RestT scheme e) a = Either e a
  liftWith f =   RestT
                 $ ME.ExceptT $ ME.liftM pure
                   $ MR.ReaderT $ \r ->
                     f
                   $ flip MR.runReaderT r
                 . ME.runExceptT
               . runRestT
  restoreT = RestT . ME.ExceptT . MR.ReaderT . const

instance (MC.MonadBaseControl b m)
       => MC.MonadBaseControl b (RestT scheme e m) where
  type StM (RestT scheme e m) a = MC.ComposeSt (RestT scheme e) m a
  liftBaseWith = MC.defaultLiftBaseWith
  restoreM = MC.defaultRestoreM

instance (Monad m, MonadIO m) => WR.MonadHttp (RestT scheme e m) where
  handleHttpException e = MR.asks _rtd_http_err >>= ME.throwError . ($ e)
  getHttpConfig = MR.asks _rtd_req_config

------------------------------------------------------------------------ }}}
--- withRedmine -------------------------------------------------------- {{{

-- | There are a whole lot of identifiers that fly around.  Phfew.
data RedmineInfo = RI
  { _ri_projId      :: Integer
  , _ri_trackId     :: Integer
  , _ri_cf_areas    :: Integer
  , _ri_cf_citizen  :: Integer
  , _ri_cf_email    :: Integer
  , _ri_cf_faculty  :: Integer
  , _ri_cf_gre      :: Integer
  , _ri_cf_insts    :: Integer
  , _ri_cf_jhuAppId :: Integer
  , _ri_cf_pdfURL   :: Integer
  , _ri_cf_toefl    :: Integer
  , _ri_cf_triageA  :: Integer
  , _ri_cf_scoreA   :: Integer
  , _ri_cf_triageB  :: Integer
  , _ri_cf_scoreB   :: Integer
  }
$(L.makeLenses ''RedmineInfo)

-- | A wrapper which sets us up to make ReSTful queries against a Redmine
--   instance given the common arguments.
--
--   Makes a boatload of requests itself to figure out common parameters
--   of the Redmine setup used by CLSP.  Hooray.
withRedmine :: ArgCommon
            -> (WR.HttpException -> e)
            -> (forall scheme . RedmineInfo -> RestT scheme e IO a)
            -> IO (Either e a)
withRedmine ac ecb cb = do
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
                                           mapM_ (trace "> ") (BS8.lines bs)
                                           N.connectionWrite c bs
                                       -- , N.connectionRead = do
                                       --     res <- N.connectionRead c
                                       --     mapM_ (trace "< ") (BS8.lines res)
                                       --     pure res
                                       }
                            }
                        where trace pfx bsl = IO.hPutStr stderr pfx >> BS8.hPutStrLn stderr bsl
    let reqOpts = WR.port (ac_redminePort ac)
                <> WR.basicAuth (fromString $ ac_redmineAuth ac) ""

    N.newManager tlss >>= \manager ->
      flip MR.runReaderT (RTD rurl (Def.def { WR.httpConfigAltManager = Just manager }) reqOpts ecb)
      $ ME.runExceptT
      $ runRestT $ do

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
           <*> redmineIdNamed rerr (CL.sourceList customFields) "Applicant Email Address"
           <*> redmineIdNamed rerr (CL.sourceList customFields) "Faculty of Interest"
           <*> redmineIdNamed rerr (CL.sourceList customFields) "GRE Scores"
           <*> redmineIdNamed rerr (CL.sourceList customFields) "Prior Institutions"
           <*> redmineIdNamed rerr (CL.sourceList customFields) "JHU Applicant ID"
           <*> redmineIdNamed rerr (CL.sourceList customFields) "PDF Application"
           <*> redmineIdNamed rerr (CL.sourceList customFields) "TOEFL Score"
           <*> redmineIdNamed rerr (CL.sourceList customFields) "Triage A"
           <*> redmineIdNamed rerr (CL.sourceList customFields) "Triage A Score"
           <*> redmineIdNamed rerr (CL.sourceList customFields) "Triage B"
           <*> redmineIdNamed rerr (CL.sourceList customFields) "Triage B Score"

        cb ri
 where
  rerr n [] = error $ "ERR: Could not find redmine object by name: " ++ (T.unpack n)
  rerr n _  = error $ "ERR: Redmine object name is not unique: " ++ (T.unpack n)

------------------------------------------------------------------------ }}}
------------------------------------------------------------------------ }}}
-- Redmine API Details ------------------------------------------------- {{{
--- Redmine API Query Core --------------------------------------------- {{{

-- Return the entire response body as well as a descended piece of it
redmineQuery :: (MonadIO m, WR.MonadHttp m)
             => m (WR.JsonResponse A.Value)
             -> Text
             -> m (A.Value, A.Value)
redmineQuery q d = do
  resp <- q
  let rb = WR.responseBody resp
  case rb ^? A.key d of
      Nothing -> error $ "ERR: Invalid Redmine response? " ++ (show $ WR.toVanillaResponse resp)
      Just value -> pure (rb, value)

-- Sometimes we need to make Redmine queries for lists of things.  Sometimes
-- those things have paged interfaces because there are lots of them; the
-- 'redmineListCursor' version here manages the offset transparently.
-- Hooray.
redmineList, redmineListCursor
  :: MonadIO m
  => Text        -- ^ URL suffix
  -> Text        -- ^ Field to look for in Redmine response
  -> C.ConduitM i A.Value (RestT scheme e m) ()
redmineList u d = do
  RTD b _ o _ <- MR.ask
  (_, value) <- lift $ redmineQuery (WR.req WR.GET (b WR./~ u) WR.NoReqBody WR.jsonResponse o) d
  CL.sourceList $ value ^.. A.values
redmineListCursor u d = do
  RTD b _ o _ <- MR.ask
  let fetch n = WR.req WR.GET
                       (b WR./~ u)
                       WR.NoReqBody
                       WR.jsonResponse
                       (o <> "offset" WR.=: n)
  go fetch 0
  where
   go fetch off = do
    (rb, value) <- lift $ redmineQuery (fetch off) d
    CL.sourceList $ value ^.. A.values
    maybe (error $ "ERR: JSON response missing position information: " ++ show rb)
          (\(tc,ooff,olim) -> if ooff + olim < tc then go fetch (ooff+olim) else pure ())
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
               => Text                       -- ^ Field carrying the name
               -> (Text -> [Integer] -> m a) -- ^ Error callback
               -> C.Conduit () m s           -- ^ Source conduit
               -> Text                       -- ^ Name of thing to look for
               -> (Integer -> m a)
               -> m a
withRedmineIdThing k err cond name f = do
  ids <- C.sourceToList
       $ cond
       C.=$= CL.filter (\p -> Just name == p ^? A.key k . A._String)
       C.=$= CL.mapMaybe (\p -> p ^? A.key "id" . A._Integer)
  case ids of
    [i] -> f i
    _   -> err name ids

-- A lot of Redmine things use "name" as the value we care about *and*
-- we don't necessarily care about all the fancy error handling that
-- the generic `withRedmineIdThing` would give us.  This is a simplified
-- interface.
redmineIdNamed :: (MonadIO m, A.AsValue s)
               => (Text -> [Integer] -> m Integer) -- ^ Error callback
               -> C.Conduit () m s                 -- ^ Source conduit
               -> Text                             -- ^ Name of thing to look for
               -> m Integer
redmineIdNamed err cond name = withRedmineIdThing "name" err cond name pure


------------------------------------------------------------------------ }}}
--- Redmine API Queries ------------------------------------------------ {{{

-- Using the above functions, make some convenience enumerators ("source
-- conduits") for things in Redmine we care about.

redmineProjects, redmineUsers, redmineTrackers, redmineCustomFields,
 redmineIssues, redmineIssueStats, redmineRoles
  :: MonadIO m => C.ConduitM i A.Value (RestT scheme e m) ()
redmineProjects     = redmineListCursor "projects.json"       "projects"
redmineUsers        = redmineListCursor "users.json"          "users"
redmineIssues       = redmineListCursor "issues.json"         "issues"
redmineIssueStats   = redmineList       "issue_statuses.json" "issue_statuses"
redmineTrackers     = redmineList       "trackers.json"       "trackers"
redmineCustomFields = redmineList       "custom_fields.json"  "custom_fields"
redmineRoles        = redmineList       "roles.json"          "roles"

-- Ick!
redmineProjectMembers :: MonadIO m => Integer -> C.ConduitM i A.Value (RestT scheme e m) ()
redmineProjectMembers pid = MR.local (rtd_base %~ \b -> b WR./: "projects" WR./~ (show pid))
                          $ redmineListCursor "memberships.json" "memberships"

------------------------------------------------------------------------ }}}
--- Redmine API Updates ------------------------------------------------ {{{

-- Construct a new applicant; note that we have to add the project_id and
-- tracker_id from the RedmineInfo we have.
redmineNewIssue :: RedmineInfo -> RedmineApplicant (Maybe Integer) -> RestT scheme e IO WR.LbsResponse
redmineNewIssue ri ra = do
  RTD u _ o _ <- MR.ask
  let (A.Object fs) = jsonifyRedmineApp ri ra
  let fs' = (DHS.fromList [ "project_id" A..= (ri ^. ri_projId), "tracker_id" A..= (ri ^. ri_trackId) ]) <> fs
  WR.req WR.POST (u WR./: "issues.json") (WR.ReqBodyJson $ A.object ["issue" A..= fs']) WR.lbsResponse o

-- Create a new user
redmineNewUser :: RedmineAccount () Text -> RestT scheme e IO (WR.JsonResponse A.Value)
redmineNewUser rac = do
  RTD u _ o _ <- MR.ask
  let (A.Object fs) = jsonifyRedmineAccount rac
  WR.req WR.POST (u WR./: "users.json") (WR.ReqBodyJson $ A.object ["user" A..= fs]) WR.jsonResponse o

-- Install a user into the current project as the given roles
redmineSetUserRole :: RedmineInfo -> RedmineAccount Integer a -> [Integer] -> RestT scheme e IO WR.LbsResponse
redmineSetUserRole ri rac roleids = do
  RTD u _ o _ <- MR.ask
  WR.req WR.POST (u WR./: "projects" WR./: (T.pack $ show $ ri ^. ri_projId) WR./: "memberships.json")
                 (WR.ReqBodyJson $ A.object ["membership" A..=
                                               A.object [ "user_id" A..= (rac ^. rac_redmineID)
                                                        , "role_ids" A..= roleids
                                                        ]
                                            ])
                 WR.lbsResponse
                 o

-- Update an existing applicant; sends all fields and Redmine sorts it out
redmineUpdateIssue :: RedmineInfo -> Integer -> RedmineApplicant (Maybe Integer) -> RestT scheme e IO WR.LbsResponse
redmineUpdateIssue ri n ra = do
  RTD u _ o _ <- MR.ask
  WR.req WR.PUT
         (u WR./: "issues" WR./: (T.pack (show n ++ ".json")))
         (WR.ReqBodyJson $ A.object ["issue" A..= jsonifyRedmineApp ri ra])
         WR.lbsResponse
         o

-- Update the status of an issue; status information is not captured in
-- RedmineApplicant for the moment (but perhaps it should be)
redmineUpdateIssueStatus :: Integer -> Integer -> RestT scheme e IO WR.LbsResponse
redmineUpdateIssueStatus which what = do
  RTD u _ o _ <- MR.ask
  WR.req WR.PUT
         (u WR./: "issues" WR./: (T.pack (show which ++ ".json")))
         (WR.ReqBodyJson $ A.object ["issue" A..= A.object [ "status_id" A..= what ]])
         WR.lbsResponse
         o

------------------------------------------------------------------------ }}}
------------------------------------------------------------------------ }}}
-- Redmine Applicant To JSON ------------------------------------------- {{{

-- Map reviewer emails to possible identifiers
lookupReviewers :: RedmineApplicant Text -> RestT scheme e IO (RedmineApplicant (Maybe Integer))
lookupReviewers ra = do
  r1id <- findUserMaybe (_ra_reviewer1 ra)
  r2id <- findUserMaybe (_ra_reviewer2 ra)
  let assignee = r2id <|> r1id
  pure $ ra { _ra_reviewer1 = r1id, _ra_reviewer2 = r2id, _ra_assignee = assignee }
 where
  findUserMaybe :: Text -> RestT scheme e IO (Maybe Integer)
  findUserMaybe ue =
   -- Sometimes we don't have a reviewer assigned.  This gets given to us in
   -- a variety of ad-hoc ways; filter them out here.
   if ue `elem` ["", "<na>"]
    then pure Nothing
    else -- Sometimes emails come to us surrounded in angle brackets.  Strip those off.
         let ue' = case (T.unpack ue) Re.=~ ("^<(.*)>$" :: String) of
                     (_ :: String, _ :: String, _ :: String, [t']) -> T.pack t'
                     _                                             -> ue
         -- if there's more than one, just pick the first; that's gross, but...
         in withRedmineIdThing "mail" (\_ -> pure . M.listToMaybe) redmineUsers ue' (pure . Just)
            >>= maybe (warn ue *> pure Nothing) (pure . Just)

  warn ue = liftIO $ IO.hPutStrLn IO.stderr $ "ERR: Unable to find user ID for email: " ++ show ue


-- Convert a RemineApplicant, whose reviewers have already been mapped using
-- Redmine, to a JSON representation using the RedmineInfo to get
-- identifiers for custom fields
--
-- XXX These types are a little too monomorphic for my taste, but they work.
jsonifyRedmineApp :: RedmineInfo -> RedmineApplicant (Maybe Integer) -> A.Value
jsonifyRedmineApp ri ra = do
  A.object $ addfm "assigned_to_id" ra_assignee
           $ [ "custom_fields" A..= (A.toJSON
               $ addcfm ri_cf_triageA  ra_reviewer1
               $ addcfm ri_cf_scoreA   ra_score1
               $ addcfm ri_cf_triageB  ra_reviewer2
               $ addcfm ri_cf_scoreB   ra_score2
               $ [ mkcf ri_cf_jhuAppId ra_jhuAppId
                 , mkcf ri_cf_email    ra_email
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
  addfm :: (A.ToJSON v, A.KeyValue a)
        => Text -> L.Lens' (RedmineApplicant (Maybe Integer)) (Maybe v) -> [a] -> [a]
  addfm  f v  = maybe id (\vv -> ((f A..= vv) :)) (ra ^. v)

  addcfm :: (A.ToJSON v)
         => L.Lens' RedmineInfo Integer
         -> L.Lens' (RedmineApplicant (Maybe Integer)) (Maybe v)
         -> [A.Value] -> [A.Value]
  addcfm f v  = maybe id (\vv -> ((A.object [ "id" A..= show (ri ^. f), "value" A..= vv ]) :)) (ra ^. v)

  mkcf :: (A.ToJSON v)
       => L.Lens' RedmineInfo Integer
       -> L.Lens' (RedmineApplicant (Maybe Integer)) v
       -> A.Value
  mkcf   f v  = A.object [ "id" A..= show (ri ^. f), "value" A..= (ra ^. v) ]

------------------------------------------------------------------------ }}}
-- Redmine Account To JSON --------------------------------------------- {{{

-- Convert a RemineAccount to a JSON representation; note that this ignores
-- the redmineID field's value.
jsonifyRedmineAccount :: RedmineAccount a Text -> A.Value
jsonifyRedmineAccount rac = do
  A.object [ "login"     A..= (rac ^. rac_redmineName)
           , "firstname" A..= (rac ^. rac_firstName)
           , "lastname"  A..= (rac ^. rac_lastName)
           , "mail"      A..= (rac ^. rac_email)
           ]
------------------------------------------------------------------------ }}}
-- Redmine Applicant From Redmine JSON --------------------------------- {{{

-- Parse a Redmine JSON response into a RedmineApplicant structure
jsonToRedmineApplicant :: RedmineInfo -> A.Value -> Maybe (Integer, RedmineApplicant (Maybe Integer))
jsonToRedmineApplicant ri va = do
  iid <- va ^? A.key "id" . A._Integer
  subj <- va ^? A.key "subject" . A._String
  cfs <- va ^? A.key "custom_fields"
  let
   lcf :: L.Lens' RedmineInfo Integer -> Maybe (A.Value)
   lcf f = cfs ^? L.traverseOf A.values . L.filtered (`idIs` (ri ^. f)) . A.key "value"
  email  <- lcf ri_cf_email    >>= (^? A._String)
  pdf    <- lcf ri_cf_pdfURL   >>= (^? A._String)
  appId  <- lcf ri_cf_jhuAppId >>= (^? A._String)
  gre    <- lcf ri_cf_gre      >>= (^? A._String)
  toefl  <- lcf ri_cf_toefl    >>= (^? A._String)
  facul  <- lcf ri_cf_faculty  >>= (^? A._String)
  insts  <- lcf ri_cf_insts    >>= (^? A._String)
  usCit  <- lcf ri_cf_citizen  >>= (^? A._String) >>= pure . (== "1")
  areas  <- lcf ri_cf_areas    >>= (^? A._JSON)
  let tA  = lcf ri_cf_triageA  >>= (^? A._String) >>= (readMaybe . T.unpack)
  let tAs = lcf ri_cf_scoreA   >>= (^? A._String) >>= (readMaybe . T.unpack)
  let tB  = lcf ri_cf_triageB  >>= (^? A._String) >>= (readMaybe . T.unpack)
  let tBs = lcf ri_cf_scoreB   >>= (^? A._String) >>= (readMaybe . T.unpack)
  let ae  = va ^? A.key "assigned_to" . A.key "id" . A._Integer
  pure $ (iid,) $ RedmineApplicant
    { _ra_subject     = subj
    , _ra_email       = email
    , _ra_pdfURL      = pdf
    , _ra_jhuAppId    = appId
    , _ra_gre         = gre
    , _ra_toefl       = toefl
    , _ra_faculty     = facul
    , _ra_insts       = insts
    , _ra_citizen_us  = usCit
    , _ra_areas       = areas
    , _ra_reviewer1   = tA
    , _ra_score1      = tAs
    , _ra_reviewer2   = tB
    , _ra_score2      = tBs
    , _ra_assignee    = ae
    }
 where
  idIs :: A.Value -> Integer -> Bool
  idIs o x = o L.^? A.key "id" == Just (A.toJSON x)

------------------------------------------------------------------------ }}}
-- Command: Ping ------------------------------------------------------- {{{

doPing :: ArgCommon -> IO ()
doPing ac = wrap =<< withRedmine ac show (\_ -> pure ())
 where
  wrap :: Either String () -> IO ()
  wrap = either IO.putStrLn pure

------------------------------------------------------------------------ }}}
-- Command: CSV Upsert ------------------------------------------------- {{{

data CsvUpsertParams = CUP
  { _csvUpsertClobber    :: Bool
  , _csvUpsertDryRun     :: Bool
  , _csvUpsertWarnTriage :: Bool
  , _csvUpsertSepChar    :: DW.Word8
  }

doCsvUpsert :: CsvUpsertParams -> ArgCommon -> IO ()
doCsvUpsert (CUP pClobber pDryRun pWarnTriage pSepChar) ac = wrap =<< withRedmine ac (T.pack . show) go
   where
    wrap :: Either Text () -> IO ()
    wrap = either T.putStrLn pure

    -- Cassava decode options
    csvdec = CSV.defaultDecodeOptions { CSV.decDelimiter = pSepChar }

    -- Alright, here's our pipeline; the heart of the upsert command.  We
    -- start by...
    go :: RedmineInfo -> RestT scheme Text IO ()
    go ri = C.runConduit $
            -- ... reading stdin as chunks of bytes ...
            CB.sourceHandle IO.stdin
            -- ... converting those chunks of bytes to CSVApplicants ...
            C.=$= CSV.fromNamedCsvStreamError csvdec raiseCsvStreamErr
            -- ... filtering out any CSV errors and showing them ...
            C.=$= CL.mapMaybeM (either ((*> pure Nothing) . printCsvErr) (pure . Just))
            -- ... purely mapping schemas ...
            C.=$= CL.map csvToRedmine
            -- ... using Redmine to convert reviewers ...
            C.=$= CL.mapM lookupReviewers
            -- ... and lastly using the above "process" function
            C.=$= CL.mapM_ process
     where
       -- Once we've got an applicant...
      process :: RedmineApplicant (Maybe Integer) -> RestT scheme e IO ()
                     -- This cryptic little number traps any HTTP errors on
                     -- a per-candidate basis so that we don't bail the
                     -- first time there's an error.  It matters a little
                     -- less than it might otherwise (you can always rerun
                     -- the script, but this is nicer).
      process what = MC.control $ \run -> handle printHTTPErr $ run $ do
        let appid = _ra_jhuAppId what
        -- ... see if they already exist in Redmine ...
        raids <- MR.local (rtd_req_opts %~ (
                   \x -> x <> ("project_id" WR.=: ri ^. ri_projId)
                           <> ("tracker_id" WR.=: ri ^. ri_trackId)
                           <> ((fromString ("cf_" ++ show (_ri_cf_jhuAppId ri))) WR.=: appid)))
               $ C.sourceToList
               $ redmineIssues
               C.=$= CL.mapMaybe (^? A.key "id" . A._Integer)

        -- ... and respond appropriately by...
        case (raids, pClobber) of
                                 -- complaining if there's a problem
          (_:_:_, _)          -> liftIO $ IO.putStrLn
                                   ("Multiple applicants with the same ID? " ++ show appid)
                                 -- ... creating a new issue if they
                                 -- don't already exist
          ([], _)             -> new what >> progress what
                                 -- ... clobbering the existing record
                                 -- if they already do and we've been told
                                 -- to clobber ...
          ([applicant], True) -> upd applicant what >> progress what
                                 -- ... or complaining otherwise.
          ([_], False)        -> liftIO $ IO.putStrLn
                                   ("Not clobbering existing applicant ID " ++ show appid)


      new :: RedmineApplicant (Maybe Integer) -> RestT scheme e IO ()
      new what = if pDryRun
                  then when (ac_debug ac > 0) $ liftIO $ do
                        IO.putStrLn "Would insert new candidate"
                        BL8.putStrLn (A.encode $ jsonifyRedmineApp ri what)
                  else redmineNewIssue ri what >> pure ()

      upd :: Integer -> RedmineApplicant (Maybe Integer) -> RestT scheme e IO ()
      upd applicant what = if pDryRun
                            then liftIO $ do
                                  IO.putStrLn ("Would update existing candidate issue " ++ (show applicant))
                                  when (ac_debug ac > 1) $ BL8.putStrLn (A.encode $ jsonifyRedmineApp ri what)
                            else redmineUpdateIssue ri applicant what >> pure ()

      printCsvErr (CSV.CsvStreamRecordParseError e) =
        liftIO $ IO.hPutStrLn IO.stderr
               $ "CSV ERR (skipping record): " ++ (T.unpack e)

      raiseCsvStreamErr :: CSV.CsvStreamHaltParseError -> Text
      raiseCsvStreamErr (CSV.HaltingCsvParseError _ e) = e

      printHTTPErr :: N.HttpException -> IO (Either e ())
      printHTTPErr e = do
        liftIO $ IO.hPutStrLn IO.stderr
               $ "ERR: Bad interaction; response is: " ++ (show e)
        pure (Right ())

      progress :: RedmineApplicant (Maybe a) -> RestT scheme e IO ()
      progress what = do
        liftIO $ IO.putStrLn $ "Processed applicant ID " ++ (T.unpack $ _ra_jhuAppId what)
        when (pWarnTriage
              && (all M.isNothing $ map ($ what) [_ra_assignee, _ra_reviewer1, _ra_reviewer2]))
             $ liftIO $ IO.hPutStrLn IO.stderr
                      $ "ERR: No reviewers or assignee for applicant " ++ (T.unpack $ _ra_jhuAppId what)

parseUpsert :: OA.Parser CsvUpsertParams
parseUpsert = CUP
   <$> OA.flag False True (OA.long "clobber"                     <> OA.short 'x' <> OA.help "Overwrite existing records")
   <*> OA.flag False True (OA.long "dry-run" <> OA.long "no-act" <> OA.short 'n' <> OA.help "Do not actually run imports")
   <*> OA.flag False True (OA.long "warn-no-triage"                              <> OA.help "Complain if no triagers assigned")
   <*> sepCharOption

------------------------------------------------------------------------ }}}
-- Command: Account Creation ------------------------------------------- {{{

data NewAccountParams = NAP
  { _napDryRun :: Bool
  , _napSepChar :: DW.Word8
  }

parseNewAccounts :: OA.Parser NewAccountParams
parseNewAccounts = NAP
   <$> OA.flag False True (OA.long "dry-run" <> OA.long "no-act" <> OA.short 'n'
                           <> OA.help "Do not actually create accounts")
   <*> sepCharOption

doNewAccounts :: NewAccountParams -> ArgCommon -> IO ()
doNewAccounts (NAP pDryRun pSepChar) ac = wrap =<< withRedmine ac (T.pack . show) go
 where
  wrap :: Either Text () -> IO ()
  wrap = either T.putStrLn pure

  chat :: MonadIO m => IO () -> m ()
  chat = when (ac_debug ac > 0 || pDryRun) . liftIO

  go :: RedmineInfo -> RestT scheme Text IO ()
  go ri = do
          reviewerRoleId <- redmineIdNamed rerr redmineRoles "Applicant Reviewer"
          existingMembers <- C.sourceToList $ redmineProjectMembers (ri ^. ri_projId)

          let findUserInMembers uid = filter (\t -> Just uid == t ^? A.key "user" . A.key "id" . A._Integer)
                                             existingMembers

          let process :: RedmineAccount () () -> RestT scheme e IO ()
              process rac = do
                 existsEmail <- searchUsers (rac ^. rac_email) 

                 let debugpfx =  TLB.fromText (_rac_firstName rac)
                              <> TLB.singleton ' '
                              <> TLB.fromText (_rac_lastName rac)
                              <> TLB.fromText " (email="
                              <> TLB.fromText (_rac_email rac)
                              <> TLB.fromText " redmineid="
                              <> TLB.fromString (show existsEmail)
                              <> TLB.fromText ")"

                 case existsEmail of
                   []  -> let login = case (T.unpack $ rac ^. rac_email) Re.=~ ("^([^@]*)@.*$" :: String) of
                                        (_ :: String, _ :: String, _ :: String, [t']) -> T.pack t'
                                        _                                             -> (rac ^. rac_firstName) <> (rac ^. rac_lastName)
                          in do
                              existsLogin <- searchUsers login
                              case existsLogin of
                                [] -> if pDryRun
                                       then  chat $ tptlt $ debugpfx
                                                          <> TLB.fromText ": User does not exist; would create with login "
                                                          <> TLB.fromText login
                                       else do
                                             chat $ tptlt $ debugpfx
                                                          <> TLB.fromText ": User does not exist; creating with login "
                                                          <> TLB.fromText login
                                             newUser <- redmineNewUser (rac & rac_redmineName .~ login)
                                             let muid = (WR.responseBody newUser) ^? A.key "user" . A.key "id" . A._Integer
                                             case muid of
                                               Nothing -> error "ERR: Something has gone terribly wrong: no user ID in response"
                                               Just x -> redmineSetUserRole ri (rac & rac_redmineID .~ x) [reviewerRoleId] >> pure ()
                                _  -> liftIO $ tptlt
                                             $ debugpfx
                                             <> TLB.fromText ": Login collision ("
                                             <> TLB.fromString (show existsLogin)
                                             <> TLB.fromText "); cannot create: "
                                             <> TLB.fromText login
                   x:_ -> do
                           chat $ tptlt $ debugpfx <> TLB.fromText ": User exists; adding to project."
                           if not . null $ findUserInMembers x
                            then chat $ IO.putStrLn " And already has membership; skipping."
                            else if pDryRun
                                  then chat $ IO.putStrLn " Is not a member; would insert."
                                  else redmineSetUserRole ri (rac & rac_redmineID .~ x) [reviewerRoleId] >> pure ()

          C.runConduit $
            -- ... reading stdin as chunks of bytes ...
            CB.sourceHandle IO.stdin
            -- ... converting those chunks of bytes to CSVApplicants ...
            C.=$= CSV.fromNamedCsvStreamError csvdec raiseCsvStreamErr
            -- ... filtering out any CSV errors and showing them ...
            C.=$= CL.mapMaybeM (either ((*> pure Nothing) . printCsvErr) (pure . Just))
            -- ... and processing
            C.=$= CL.mapM_ process

  -- Note that Redmine doesn't let us be terribly picky about what we're
  -- searching *on*.  So this is at best heuristic.
  searchUsers x = MR.local (rtd_req_opts %~ (<> ("name" WR.=: x)))
                $ C.sourceToList
                $ redmineUsers
                C.=$= CL.mapMaybe (^? A.key "id" . A._Integer)

  tptlt :: TLB.Builder -> IO ()
  tptlt = TL.putStrLn . TLB.toLazyText

  rerr n [] = error $ "ERR: Could not find redmine object by name: " ++ (T.unpack n)
  rerr n _  = error $ "ERR: Redmine object name is not unique: " ++ (T.unpack n)

  csvdec = CSV.defaultDecodeOptions { CSV.decDelimiter = pSepChar }

  printCsvErr (CSV.CsvStreamRecordParseError e) =
    liftIO $ IO.hPutStrLn IO.stderr
           $ "CSV ERR (skipping record): " ++ (T.unpack e)

  raiseCsvStreamErr :: CSV.CsvStreamHaltParseError -> Text
  raiseCsvStreamErr (CSV.HaltingCsvParseError _ e) = e


------------------------------------------------------------------------ }}}
-- Command: Applicants in Triage --------------------------------------- {{{

data TriageParams = TP
  { _triageDryRun :: Bool
  }

parseTriage :: OA.Parser TriageParams
parseTriage = TP
   <$> OA.flag False True (OA.long "dry-run" <> OA.long "no-act" <> OA.short 'n' <> OA.help "Do not promote")

doApplicantsInTriage :: TriageParams -> ArgCommon -> IO ()
doApplicantsInTriage (TP pDryRun) ac = wrap =<< withRedmine ac show go
 where
  wrap :: Either String () -> IO ()
  wrap = either IO.putStrLn pure

  go :: RedmineInfo -> RestT scheme e IO ()
  go ri = do
     isstats <- C.sourceToList redmineIssueStats

     insid  <- redmineIdNamed rerr (CL.sourceList isstats) "In Triage"
     outsid <- redmineIdNamed rerr (CL.sourceList isstats) "Passed Triage"

     let applicants =
          MR.local (rtd_req_opts %~ (
                     \x -> x <> ("project_id" WR.=: ri ^. ri_projId)
                             <> ("tracker_id" WR.=: ri ^. ri_trackId)
                             <> ("status_id" WR.=: insid)
                   ))
          redmineIssues

     let process (raid, ra) = do
          let debugpfx =  TLB.fromText (_ra_subject ra)
                       <> TLB.fromText " (jhuid="
                       <> TLB.fromText (_ra_jhuAppId ra)
                       <> TLB.fromText " redmineid="
                       <> TLB.fromString (show raid)
                       <> TLB.fromText "):"
          let scores = M.catMaybes [_ra_score1 ra, _ra_score2 ra]
          let promote = case scores of
                         []                                  -> False
                         xs | all (>= 4) xs || any (>= 5) xs -> True
                         _                                   -> False
          rtx <- if promote
                  then if pDryRun
                        then pure "Would promote"
                        else do
                              _ <- redmineUpdateIssueStatus raid outsid
                              pure "Promoted"
                  else pure "Not promoting"
          liftIO $ TL.putStrLn $ TLB.toLazyText
                 $ TLB.fromText rtx
                 <> TLB.singleton ' '
                 <> debugpfx
                 <> TLB.fromText " scores="
                 <> TLB.fromString (show scores)

     C.runConduit
        $     applicants
        C.=$= CL.map (jsonToRedmineApplicant ri)
        C.=$= CL.map (maybe (error "ERR: Failed to decode applicant") id)
        C.=$= CL.mapM_ process
    where
     rerr n [] = error $ "Could not find redmine object by name: " ++ (T.unpack n)
     rerr n _  = error $ "Redmine object name is not unique: " ++ (T.unpack n)

------------------------------------------------------------------------ }}}
-- Command: Test Various Parsers --------------------------------------- {{{

-- Crossref 'doCsvUpsert'
doTestParseCSVApplicant :: DW.Word8 -> ArgCommon -> IO ()
doTestParseCSVApplicant sc _ = wrap =<< ME.runExceptT go
 where
  wrap :: Either Text () -> IO ()
  wrap = either T.putStrLn pure

  go :: ME.ExceptT Text IO ()
  go = C.runConduit
     $ CB.sourceHandle IO.stdin
     C.=$= CSV.fromNamedCsvStreamError csvdec raiseCsvStreamErr
     C.=$= CL.mapMaybeM (either ((*> pure Nothing) . printCsvErr) (pure . Just))
     C.=$= CL.map csvToRedmine
     C.=$= CL.mapM_ (liftIO . IO.print)

  csvdec = CSV.defaultDecodeOptions { CSV.decDelimiter = sc }

  printCsvErr (CSV.CsvStreamRecordParseError e) =
    liftIO $ IO.hPutStrLn IO.stderr
           $ "CSV ERR (skipping record): " ++ (T.unpack e)

  raiseCsvStreamErr :: CSV.CsvStreamHaltParseError -> Text
  raiseCsvStreamErr (CSV.HaltingCsvParseError _ e) = e

doTestParseJsonApplicant :: ArgCommon -> IO ()
doTestParseJsonApplicant ac = wrap =<< withRedmine ac show go
 where
  wrap :: Either String () -> IO ()
  wrap = either IO.putStrLn pure

  go :: RedmineInfo -> RestT scheme e IO ()
  go ri = do
    x <- liftIO $ BL.hGetContents IO.stdin
    case A.decode x of
      Just (v :: A.Value) -> liftIO $ print (jsonToRedmineApplicant ri v)
      Nothing -> liftIO $ putStrLn "JSON decode failure"

testSubcommand :: OA.Parser (ArgCommon -> IO ())
testSubcommand = OA.subparser
               $  testParseCSVApplicantCommand
               <> testParseJsonApplicantCommand

 where
  testParseCSVApplicantCommand :: OA.Mod OA.CommandFields (ArgCommon -> IO ())
  testParseCSVApplicantCommand = OA.command "parse-csv-applicant"
    $ infoh (doTestParseCSVApplicant <$> sepCharOption)
            (OA.progDesc "Test parsing CSV Applicant data, as from dept.")

  testParseJsonApplicantCommand :: OA.Mod OA.CommandFields (ArgCommon -> IO ())
  testParseJsonApplicantCommand = OA.command "parse-json-applicant"
    $ infoh (pure doTestParseJsonApplicant)
            (OA.progDesc "Test parsing JSON Applicant data, as from Redmine")

------------------------------------------------------------------------ }}}
-- Command: Expected Yield --------------------------------------------- {{{

-- XXX not yet

-- data ExpectedYieldSummary = EYS
--                           !Int     -- number of applicants
--                           !Double  -- summed expected yield
--                           ![Int]   -- list of applicant IDs without EYs
--
-- summarizeExpectedYield :: Monad m => C.Sink A.Value m ExpectedYieldSummary
-- summarizeExpectedYield = flip C.fold
--      (EYS 0 0 [])           -- starting summary value
--    $ \(EYS n ey bs) v ->    -- function to merge summary and new issue
--       case extractExpectedYield v of
--           -- Try to be nice and report the ID of any issues that
--           -- do not have parsable expected yields, rather than
--           -- just bailing or ignoring them.
--         Nothing -> EYS (n+1) ey ((unsafeGetID v):bs)
--         Just eyc -> EYS (n+1) (ey+eyc) bs
--  where
--   extractExpectedYield o = o `getCustomDouble` "Expected Yield"

------------------------------------------------------------------------ }}}
-- Argument Parsing and main function ---------------------------------- {{{

-- Common argument parsing.  Nothing terribly fancy.

argWord8AsChar :: OA.ReadM DW.Word8
argWord8AsChar = do
  c :: Char <- OA.auto
  let i = DC.ord c
  if i > (fromIntegral (maxBound :: DW.Word8))
   then OA.readerError "Selected character value does not fit in 8 bits!"
   else pure (fromIntegral i)

-- this is used by several commands at this point
sepCharOption :: OA.Parser DW.Word8
sepCharOption = OA.option argWord8AsChar
                          (  OA.long "sep"
                          <> OA.short 's'
                          <> OA.value (fromIntegral $ DC.ord ',')
                          <> OA.help "Set the separator value (defaults to ',')")

infoh :: forall a. OA.Parser a -> OA.InfoMod a -> OA.ParserInfo a
infoh p = OA.info (p <**> OA.helper)

data ArgCommon = ArgCommon
  { ac_redmineURL      :: WR.Url 'WR.Https
  , ac_redminePort     :: Int
  , ac_redmineAuth     :: String
  , ac_redmineProjName :: String
  , ac_cert_check      :: Bool
  , ac_debug           :: Int
  }

oat :: OA.ReadM a  -- ^ Reader
    -> [String]    -- ^ Long opt spellings
    -> [Char]      -- ^ Short opt spellings
    -> String      -- ^ Metavariable text for help message
    -> String      -- ^ Help description
    -> OA.Mod OA.OptionFields a -- ^ Other options
    -> OA.Parser a
oat t ls ss m h x = OA.option t
  $  OA.help h
  <> OA.metavar m
  <> mconcat (map OA.long ls)
  <> mconcat (map OA.short ss)
  <> x

parseArgCommon :: OA.Parser ArgCommon
parseArgCommon = ArgCommon
  <$> oat host    ["host"]        ['h']     "HOST"     "Redmine server host"
          (OA.value $ WR.https "clsp-redmine.cs.jhu.edu")
  <*> oat OA.auto ["port"]        ['P']     "PORT"     "Redmine server port"    (OA.value 443)
  <*> oat OA.str  ["auth","user"] ['a','u'] "STRING"   "Authentication token"   mempty
  <*> oat OA.str  ["project"]     ['p']     "ProjName" "Project name"           mempty
  <*> OA.flag False True (OA.long "cert" <> OA.short 'C')
  <*> oat OA.auto ["debug"]       ['v']     "LEVEL"    "Verbosity"              (OA.value 0)
 where
  host = (WR.https . T.pack) <$> OA.str

redmine_commands :: OA.Parser (ArgCommon -> IO ())
redmine_commands = OA.subparser
                 $  pingCommand
                 <> newAccountCommand
                 <> upsertCommand
                 -- <> expectedYieldCommand
                 <> inTriageCommand
                 <> testCommand
 where
  pingCommand :: OA.Mod OA.CommandFields (ArgCommon -> IO ())
  pingCommand = OA.command "ping"
              $ infoh (pure doPing) (OA.progDesc "Ping the Redmine server")

  testCommand :: OA.Mod OA.CommandFields (ArgCommon -> IO ())
  testCommand = OA.command "test"
              $ infoh testSubcommand (OA.progDesc "Self-tests of various forms")

  newAccountCommand :: OA.Mod OA.CommandFields (ArgCommon -> IO ())
  newAccountCommand = OA.command "new-accounts"
    $ infoh (doNewAccounts <$> parseNewAccounts)
            (OA.progDesc "Create new accounts from CSV on stdin"
            <> OA.footer "Note that input *must* be UTF-8.  Try 'iconv --from latin1 --to utf8'")


  upsertCommand :: OA.Mod OA.CommandFields (ArgCommon -> IO ())
  upsertCommand = OA.command "upsert"
    $ infoh (doCsvUpsert <$> parseUpsert)
            (OA.progDesc "Upsert from CSV on stdin"
            <> OA.footer "Note that input *must* be UTF-8.  Try 'iconv --from latin1 --to utf8'")

  inTriageCommand :: OA.Mod OA.CommandFields (ArgCommon -> IO ())
  inTriageCommand = OA.command "in-triage"
    $ infoh (doApplicantsInTriage <$> parseTriage)
            (OA.progDesc "In Triage (in progress)")

  
main :: IO ()
main = do
  putStrLn "Redmine client starting..."
  join $ OA.execParser
       $ infoh ((flip ($)) <$> parseArgCommon <*> redmine_commands)
               (OA.progDesc "Interact with clsp-redmine.cs.jhu.edu")

------------------------------------------------------------------------ }}}
-- vim: set foldmethod=marker ts=2 nu
