{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Butler.Schema where

import Data.Text (Text)
import GHC.Generics (Generic, Rep)
import Butler.TypeInformation
import qualified Data.Text as Text

data Relationship r where
    HasOne :: Relationship r
    HasMany :: Relationship r
    BelongsTo :: Relationship r
    BelongsToMany :: Relationship r

data Schema a = Schema { schemaName :: Text, schemaFields :: [Field] } deriving (Show)

class Entity a where
    schema :: Schema a

    default schema :: (Generic a, Selectors (Rep a), TypeName (Rep a)) => Schema a
    schema = Schema constr fields
        where
            fields = selectors @(Rep a)
            constr = Text.toLower $ typename @(Rep a)