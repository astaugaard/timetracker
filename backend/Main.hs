{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Network.Wai.Handler.Warp (run)
import Servant
import Data.Map (Map)
import qualified Data.Map as Map
import Database.Esqueleto.Experimental hiding (Union)
import Database.Persist.TH
       ( mkMigrate
       , mkPersist
       , persistLowerCase
       , share
       , sqlSettings
       )
import Data.Time.Clock
import Data.Time.Calendar
import Control.Monad.Reader
import Database.Persist.Postgresql (withPostgresqlConn)
-- import Data.Pool
import Control.Monad.Logger
import Data.Aeson
import GHC.Generics (Generic)
import Control.Monad.Except (ExceptT (..), MonadError (..), MonadTrans (..), runExceptT)

-- uverbT taken from servant documenation
newtype UVerbT xs m a = UVerbT { unUVerbT :: ExceptT (Union xs) m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO )

-- | Deliberately hide 'ExceptT's 'MonadError' instance to be able to use
-- underlying monad's instance.
instance MonadError e m => MonadError e (UVerbT xs m) where
  throwError = lift . throwError
  catchError (UVerbT act) h = UVerbT $ ExceptT $
    runExceptT act `catchError` (runExceptT . unUVerbT . h)

-- | This combinator runs 'UVerbT'. It applies 'respond' internally, so the handler
-- may use the usual 'return'.
runUVerbT :: (Monad m, HasStatus x, IsMember x xs) => UVerbT xs m x -> m (Union xs)
runUVerbT (UVerbT act) = either id id <$> runExceptT (act >>= respond)

-- | Short-circuit 'UVerbT' computation returning one of the response types.
throwUVerb :: (Monad m, HasStatus x, IsMember x xs) => x -> UVerbT xs m a
throwUVerb = UVerbT . ExceptT . fmap Left . respond


data PeriodOfTime = PeriodOfTime {start :: Int, lengthOfPeriod :: Int} deriving (Generic)

data PeriodOfTimePlusDay = PeriodOfTimePlusDay {start :: Int, lengthOfPeriod :: Int, day :: Int} deriving (Generic)

instance ToJSON PeriodOfTime
instance ToJSON PeriodOfTimePlusDay

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Person
    username String
    deriving Eq Show
  TimeSlot
    day Int
    person PersonId
    startTime Int
    lengthOfTime Int
    deriving Eq Show
|] -- Times in seconds since start of day



type MainApi = "login" :> ReqBody '[PlainText] String :> UVerb 'POST '[JSON] '[WithStatus 200 (), WithStatus 400 (), WithStatus 500 ()]
            :<|> "logout" :> ReqBody '[PlainText] String :> UVerb 'POST '[JSON] '[WithStatus 200 (), WithStatus 400 ()]
            :<|> "gettodaydata" :> Get '[JSON] (Map String [PeriodOfTime])
            :<|> "getalldata" :> Get '[JSON] (Map String [PeriodOfTimePlusDay])

type Api = "api" :> "v1" :> MainApi
      :<|> "resource" :> Raw
      :<|> Raw

day2Int :: Day -> Int
day2Int = fromEnum . toModifiedJulianDay

currentSeconds :: DiffTime -> Int
currentSeconds time = (fromEnum (diffTimeToPicoseconds time `div` (10^12)))

getPersonNamed :: MonadIO m => String -> ReaderT SqlBackend m (Maybe PersonId)
getPersonNamed name = do peopleNamed <- select $ do
                                          person <- from $ table @Person
                                          where_ (person ^. PersonUsername ==. val name)
                                          return person
                         case peopleNamed of
                            [] -> pure Nothing
                            (x:_) -> pure $ Just $ entityKey x

createUserIfNotExists :: MonadIO m => String -> ReaderT SqlBackend m PersonId
createUserIfNotExists name = do p <- getPersonNamed name
                                case p of
                                    Just pid -> pure pid
                                    Nothing -> do insert $ Person name


newTimeSlotUser :: (MonadIO m, IsMember (WithStatus 500 ()) xs) => String -> Day -> DiffTime -> UVerbT xs (ReaderT SqlBackend m) ()
newTimeSlotUser name cday time = do p <- lift $ getPersonNamed name
                                    case p of
                                       Nothing -> throwUVerb $ WithStatus @500 ()
                                       Just pid -> do
                                           slots <- lift $ select $ do
                                             timeSlot <- from $ table @TimeSlot
                                             where_ (timeSlot ^. TimeSlotPerson ==. val pid &&. timeSlot ^. TimeSlotDay ==. val (day2Int cday) &&. timeSlot ^. TimeSlotLengthOfTime ==. val 0)
                                             pure timeSlot
                                           case slots of
                                               [] -> do -- p <- ask
                                                        -- liftIO $ withResource p $ \r -> runReaderT
                                                        --      r
                                                        _ <- lift $ insert $ TimeSlot (day2Int cday) pid (currentSeconds time) 0
                                                        pure ()

                                               (_:_) -> pure ()

updateEndTimeForUser :: (MonadIO m, IsMember (WithStatus 400 ()) xs) => String -> Day -> DiffTime -> UVerbT xs (ReaderT SqlBackend m) ()
updateEndTimeForUser s cday time =
    do n <- lift $ getPersonNamed s
       case n of
         Just pid -> lift $ update $ \timeSlot -> do
                             where_ (timeSlot ^. TimeSlotPerson ==. val pid &&. timeSlot ^. TimeSlotDay ==. val (day2Int cday) &&. timeSlot ^. TimeSlotLengthOfTime ==. val 0)
                             set timeSlot [TimeSlotLengthOfTime =. val (currentSeconds time) -. timeSlot ^. TimeSlotStartTime ]
         Nothing -> throwUVerb $ WithStatus @400 ()


getUserName :: String -> Maybe String
getUserName s = if take (length prefix) s == prefix then
                  Just $ drop (length prefix) s
                else
                  Nothing
  where prefix = "email:"

api :: SqlBackend -> Server MainApi
api cons = login
 :<|> logout
 :<|> gettodaydata
 :<|> getalldata
 where login :: String -> Handler (Union '[WithStatus 200 (), WithStatus 400 (), WithStatus 500 ()])
       login dat = do (runReaderT
                        (runUVerbT $ case getUserName dat of
                              Just username -> do
                                lift $ createUserIfNotExists username
                                (UTCTime cday startTime) <- liftIO $ getCurrentTime
                                newTimeSlotUser username cday startTime
                                pure $ WithStatus @200 ()
                              Nothing -> throwUVerb $ WithStatus @400 ()) cons ) -- :: UVerbT '[WithStatus 200 (), WithStatus 400 (), WithStatus 500 ()] (ReaderT SqlBackend Handler) ())
       logout dat = runReaderT (
              runUVerbT $ case getUserName dat of
                  Just username -> do
                      (UTCTime cday currentTime) <- liftIO $ getCurrentTime
                      updateEndTimeForUser username cday currentTime
                      pure $ WithStatus @200 ()
                  Nothing -> throwUVerb $ WithStatus @400 ()
           ) cons
       gettodaydata =
            runReaderT (
          do dat <- select $ do
                 (t:&p) <- from $ table @TimeSlot
                                  `InnerJoin`
                                  table @Person
                                     `on` \(timeSlot :& p) ->
                                        timeSlot ^. TimeSlotPerson ==. p ^. PersonId
                 pure (p ^. PersonUsername, (t ^. TimeSlotStartTime, t ^. TimeSlotLengthOfTime))
             pure $ Map.fromListWith (++) $ map (\(a,(b,c)) -> (unValue a, [PeriodOfTime (unValue b) (unValue c)])) dat
           ) cons
       getalldata = runReaderT (
            do dat <- select $ do (t:&p) <- from $ table @TimeSlot
                                                   `InnerJoin`
                                                   table @Person
                                                      `on` \(timeSlot :& p) ->
                                                         timeSlot ^. TimeSlotPerson ==. p ^. PersonId
                                  pure (p ^. PersonUsername, (t ^. TimeSlotStartTime, t ^.TimeSlotLengthOfTime , t ^. TimeSlotDay))
               pure $ Map.fromListWith (++) $ map (\(n,(s,l,d)) -> (unValue n,[PeriodOfTimePlusDay (unValue s) (unValue l) (unValue d)])) dat

           ) cons

server :: SqlBackend -> Server Api
server cons = api cons
    :<|> resources
    :<|> html
  where resources = serveDirectoryWebApp "resources"
        html = serveDirectoryWebApp "frontend/html"
            -- should change this to serve to add the .html tag... later



main :: IO ()
main = runNoLoggingT $ withPostgresqlConn "host=localhost port=5432 dbname=postgres" $ \conn ->
        let app = serve (Proxy :: Proxy Api) (server conn)
        in do runReaderT (runMigration migrateAll) conn
              liftIO $ run 8000 app

