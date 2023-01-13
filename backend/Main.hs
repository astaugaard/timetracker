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
-- import Servant.API
-- import Servant.Server
import Data.Map (Map)
import qualified Data.Map as Map
import Database.Esqueleto.Experimental
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



type MainApi = "login" :> ReqBody '[PlainText] String :> Post '[JSON] ()
            :<|> "logout" :> ReqBody '[PlainText] String :> Post '[JSON] ()
            :<|> "gettodaydata" :> Get '[JSON] (Map String [PeriodOfTime]) -- StartTime Length of time
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
                                    Nothing -> do -- pool <- ask
                                                  -- liftIO $ withResource pool $ \r -> runReaderT  r
                                                  insert $ Person name


newTimeSlotUser :: MonadIO m => String -> Day -> DiffTime -> ReaderT SqlBackend m ()
newTimeSlotUser name cday time = do p <- getPersonNamed name
                                    case p of
                                       Nothing -> liftIO $ putStrLn "error person doesn't exist"
                                       Just pid -> do
                                           slots <- select $ do
                                             timeSlot <- from $ table @TimeSlot
                                             where_ (timeSlot ^. TimeSlotPerson ==. val pid &&. timeSlot ^. TimeSlotDay ==. val (day2Int cday) &&. timeSlot ^. TimeSlotLengthOfTime ==. val 0)
                                             pure timeSlot
                                           case slots of
                                               [] -> do -- p <- ask
                                                        -- liftIO $ withResource p $ \r -> runReaderT
                                                        --      r
                                                        liftIO $ putStrLn "inserting time slot"
                                                        _ <- insert $ TimeSlot (day2Int cday) pid (currentSeconds time) 0
                                                        pure ()

                                               (_:_) -> liftIO $ putStrLn "no inserting time slot" >> pure ()

updateEndTimeForUser :: MonadIO m => String -> Day -> DiffTime -> ReaderT SqlBackend m ()
updateEndTimeForUser s cday time =
    do n <- getPersonNamed s
       case n of
         Just pid -> update $ \timeSlot -> do
                             where_ (timeSlot ^. TimeSlotPerson ==. val pid &&. timeSlot ^. TimeSlotDay ==. val (day2Int cday) &&. timeSlot ^. TimeSlotLengthOfTime ==. val 0)
                             set timeSlot [TimeSlotLengthOfTime =. val (currentSeconds time) -. timeSlot ^. TimeSlotStartTime ]
         Nothing -> liftIO $ putStrLn "can't find user of that name"


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
 where login dat = do liftIO $ putStrLn "at top level"
                      runReaderT
                        (case getUserName dat of
                              Just username -> do
                                liftIO $ putStrLn "in login"
                                _ <- createUserIfNotExists username
                                (UTCTime cday startTime) <- liftIO $ getCurrentTime
                                newTimeSlotUser username cday startTime
                                pure ()
                              Nothing -> liftIO $ putStrLn "error: invalid format") cons
       logout dat = runReaderT (
              case getUserName dat of
                  Just username -> do
                      (UTCTime cday currentTime) <- liftIO $ getCurrentTime
                      updateEndTimeForUser username cday currentTime
                  Nothing -> liftIO $ putStrLn "error: invalid format"
           ) cons
       gettodaydata =
            runReaderT (
          do liftIO $ putStrLn "hello world"
             dat <- select $ do
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

