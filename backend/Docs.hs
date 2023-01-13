{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Docs (getDocs) where

import Api
import Data.Proxy
import Servant.Docs
import Servant
import Data.Map (Map)
import qualified Data.Map as Map

type LoginEndpoint = (UVerb 'POST '[JSON]
                            '[WithStatus 200 (),
                              WithStatus 400 (),
                              WithStatus 500 ()])

type LogoutEndpoint = (UVerb 'POST '[JSON]
                            '[WithStatus 200 (),
                              WithStatus 400 ()])

endpointProxy :: Proxy LoginEndpoint
endpointProxy = Proxy

logoutEndpoint :: Proxy logoutEndpoint
logoutEndpoint = Proxy

emptyPostProxy :: Proxy (Post '[JSON] ())
emptyPostProxy = Proxy

instance HasDocs LoginEndpoint where
    docsFor api = docsFor emptyPostProxy

instance HasDocs LogoutEndpoint where
    docsFor api = docsFor emptyPostProxy

instance ToSample () where
    toSamples _ = noSamples

instance ToSample (Map String [PeriodOfTime]) where
    toSamples _ = [
        ("when there isn't any users", Map.empty),
        ("normal case (note that users that don't have any timeframes loged don't show up)", Map.fromList [("johnSmith", [PeriodOfTime 5033 50]),
                                                                                                           ("janeSmith", [PeriodOfTime 5033 50, PeriodOfTime 5070 70])])]
instance ToSample (Map String [PeriodOfTimePlusDay]) where
    toSamples _ = [
        ("when there isn't any users", Map.empty),
        ("normal case (note that users that don't have any timeframes loged don't show up)", Map.fromList [("johnSmith", [PeriodOfTimePlusDay 5033 50 60]),
                                                                                                          ("janeSmith", [PeriodOfTimePlusDay 5033 50 60, PeriodOfTimePlusDay 5070 70 61])])]

instance ToSample UserEmail where
    toSamples _ = [("the general format for passing in the users email is", UserEmail "<email>")]

instance ToCapture(Capture "email" String) where
    toCapture _ = DocCapture "email"
                             "name/email of person to check if logged in"

api :: Proxy Api
api = Proxy

getDocs :: String
getDocs = markdown $ docs api
