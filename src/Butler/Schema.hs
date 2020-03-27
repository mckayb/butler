{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}

module Butler.Schema where

import Data.Text (Text)
import Data.Proxy (Proxy(..))
import GHC.Generics (Rep)
import Type.Reflection (SomeTypeRep)
import Butler.Test
import qualified Data.Text as Text

data Relationship r where
    HasOne :: Proxy a -> Relationship r
    HasMany :: Proxy a -> Relationship r
    BelongsTo :: Proxy a -> Relationship r
    BelongsToMany :: Proxy a -> Relationship r

data FieldType = Varchar Int deriving (Show)
type Field = (Text, FieldType)

data Schema a = Schema { schemaName :: Text, schemaFields :: [Field] } deriving (Show)

selectorToField :: (String, SomeTypeRep) -> Field
selectorToField (name, _) = (Text.toLower . Text.pack $ name, Varchar 255)

class EntitySchema a where
    schema :: Schema a

    default schema :: (Selectors (Rep a), TypeName (Rep a)) => Schema a
    schema = Schema constr (fmap selectorToField fields)
        where
            fields = selectors @(Rep a)
            constr = Text.toLower . Text.pack $ typename @(Rep a)