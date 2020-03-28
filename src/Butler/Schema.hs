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
import GHC.Generics (Generic, Rep)
import Type.Reflection (SomeTypeRep)
import Butler.TypeInformation
import qualified Data.Text as Text
import Debug.Trace

data Relationship r where
    HasOne :: Relationship r
    HasMany :: Relationship r
    BelongsTo :: Relationship r
    BelongsToMany :: Relationship r

data FieldType = Number Int Int | Varchar Int deriving (Show)
type Field = (Text, FieldType)

data Schema a = Schema { schemaName :: Text, schemaFields :: [Field] } deriving (Show)

selectorToField :: (String, SomeTypeRep) -> Field
selectorToField (name, x) = 
    case show x of
        "Int" -> (col, Number 32 2)
        "Text" -> (col, Varchar 255)
        _ -> error "Unknown type!"
    where
        col = Text.toLower . Text.pack $ name

class Entity a where
    schema :: Schema a

    default schema :: (Generic a, Selectors (Rep a), TypeName (Rep a)) => Schema a
    schema = Schema constr (fmap selectorToField fields)
        where
            fields = selectors @(Rep a)
            constr = Text.toLower . Text.pack $ typename @(Rep a)


data Client = Client { clientId :: Int, clientName :: Text } deriving (Eq, Show, Generic, Entity)

test :: SomeTypeRep -> Text
test a = traceShow a "foo"