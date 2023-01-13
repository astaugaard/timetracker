{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Api (PeriodOfTime(..),PeriodOfTimePlusDay(..),MainApi,Api,UserEmail(..)) where
    
import Servant
import Data.Aeson
import GHC.Generics
import Data.Map (Map)
import Data.ByteString.Lazy.Char8 (unpack,pack)

data PeriodOfTime = PeriodOfTime {start :: Int, lengthOfPeriod :: Int} deriving (Generic)

data PeriodOfTimePlusDay = PeriodOfTimePlusDay {start :: Int, lengthOfPeriod :: Int, day :: Int} deriving (Generic)

instance ToJSON PeriodOfTime
instance ToJSON PeriodOfTimePlusDay

newtype UserEmail = UserEmail String

instance Show UserEmail where
    show (UserEmail e) = "email:" ++ e


getUserName :: String -> Maybe String
getUserName s = if take (length prefix) s == prefix then
                  Just $ drop (length prefix) s
                else
                  Nothing

  where prefix = "email:"

instance MimeUnrender PlainText UserEmail where
    mimeUnrender _ bs = case getUserName $ unpack bs of
        Just e -> Right $ UserEmail e
        Nothing -> Left "expect email input to be of form email:<email>"

instance MimeRender PlainText UserEmail where
    mimeRender _ (UserEmail em) = "email:" <> pack em

type MainApi = "login" :> ReqBody '[PlainText] UserEmail :> UVerb 'POST '[JSON] '[WithStatus 200 (), WithStatus 400 (), WithStatus 500 ()]
            :<|> "logout" :> ReqBody '[PlainText] UserEmail :> UVerb 'POST '[JSON] '[WithStatus 200 (), WithStatus 400 ()]
            :<|> "gettodaydata" :> Get '[JSON] (Map String [PeriodOfTime])
            :<|> "getalldata" :> Get '[JSON] (Map String [PeriodOfTimePlusDay])

type Api = "api" :> "v1" :> MainApi
      :<|> "resource" :> Raw
      :<|> Raw
