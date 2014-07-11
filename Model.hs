{-# LANGUAGE FlexibleInstances #-}

module Model where

import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Typeable (Typeable)

import Prelude (Show, ($))
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON (Entity Module) where
    toJSON (Entity mid m) = object
        [ "id" .= (String $ toPathPiece mid)
        , "code" .= moduleCode m
        , "name" .= moduleName m
        ]

instance FromJSON Module where
    parseJSON (Object o) = Module
        <$> o .: "code"
        <*> o .: "name"
    parseJSON _ = mzero

instance ToJSON (Entity Synthax) where
    toJSON (Entity sid s) = object
        [ "id" .= (String $ toPathPiece sid)
        , "code" .= synthaxCode s
        , "name" .= synthaxName s
        ]

instance FromJSON Synthax where
    parseJSON (Object o) = Synthax
        <$> o .: "code"
        <*> o .: "name"
    parseJSON _ = mzero

