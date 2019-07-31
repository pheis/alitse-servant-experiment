{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric         #-}

module Identity where

import qualified Data.Aeson                    as Aeson
import           GHC.Generics                   ( Generic )
import qualified Data.Text                     as T

data Role =  Mentee | Mentor | Admin deriving (Eq, Show, Read, Generic)
instance Aeson.FromJSON Role
instance Aeson.ToJSON Role

storeName :: T.Text
storeName = "Identity"

data Identity = Identity
    { login :: T.Text
    , password :: T.Text
    , role  :: Identity.Role
    , accountId :: T.Text
    , userId :: T.Text
    , mentorId :: Maybe T.Text
    } deriving (Eq, Show, Read, Generic)
instance Aeson.FromJSON Identity
instance Aeson.ToJSON Identity
