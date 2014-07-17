{-# LANGUAGE FlexibleInstances #-}

module Model where

import Yesod
import Data.Text (Text)
import Data.Time
import Database.Persist.Quasi
import Data.Typeable (Typeable)

import Prelude (Show, ($), Maybe(..), (==))
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

discardEmptyString :: Maybe Text -> Maybe Text
discardEmptyString Nothing = Nothing
discardEmptyString (Just s) = if s == "" then Nothing else Just s

instance ToJSON (Entity Module) where
    toJSON (Entity mid m) = object
        [ "id" .= (String $ toPathPiece mid)
        , "code" .= moduleCode m
        , "name" .= moduleName m
        ]

instance FromJSON Module where
    parseJSON (Object o) = (\c n -> Module c $ discardEmptyString n)
        <$> o .: "code"
        <*> o .: "name"
    parseJSON _ = mzero

instance ToJSON (Entity Synthax) where
    toJSON (Entity sid s) = object
        [ "id" .= (String $ toPathPiece sid)
        , "code" .= synthaxCode s
        , "name" .= synthaxName s
        ]

data SynthaxResponse = SynthaxResponse Text (Maybe Text)

instance FromJSON SynthaxResponse where
    parseJSON (Object o) = (\c n -> SynthaxResponse c $ discardEmptyString n)
        <$> o .: "code"
        <*> o .: "name"
    parseJSON _ = mzero

