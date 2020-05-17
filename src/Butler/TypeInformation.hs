{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Butler.TypeInformation (ModelSelectors(modelSelectors, schema), TableSchema) where

import Knit
import Data.Text (Text)
import Data.Proxy (Proxy(Proxy))
import GHC.Generics (Rep, U1, M1, D, C, Selector(selName), S, K1, R, (:*:), (:+:))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap


data FieldType = Number Int Int | Varchar Int | Foreign Text Text FieldType deriving (Eq, Show)

class HasFieldType a where
  toFieldType :: FieldType

instance HasFieldType Text where
  toFieldType = Varchar 255

instance HasFieldType Int where
  toFieldType = Number 11 0

instance HasFieldType a => HasFieldType (Proxy a) where
  toFieldType = toFieldType @a

instance HasFieldType a => HasFieldType (Lazy model a) where
  toFieldType = toFieldType @a

instance HasFieldType a => HasFieldType [Lazy model a] where
  toFieldType = toFieldType @a

instance HasFieldType a => HasFieldType (RecordId a) where
  toFieldType = toFieldType @a

instance (KnownSymbol table, KnownSymbol field, HasFieldType a) => HasFieldType (ForeignRecordId table field a) where
  toFieldType = Foreign (Text.pack . symbolVal $ Proxy @table) (Text.pack . symbolVal $ Proxy @field) (toFieldType @a)

instance (KnownSymbol table, KnownSymbol field, HasFieldType a) => HasFieldType [ForeignRecordId table field a] where
  toFieldType = Foreign (Text.pack . symbolVal $ Proxy @table) (Text.pack . symbolVal $ Proxy @field) (toFieldType @a)


type Field = (Text, FieldType)

class Selectors rep where
  selectors :: [Field]

-- | Unit - Constructor without arguments
instance Selectors U1 where
  selectors = []

-- | Meta-information (data types, etc.)
instance Selectors f => Selectors (M1 D x f) where
  selectors = selectors @f

-- | Meta-information (constructor names, etc.)
instance Selectors f => Selectors (M1 C x f) where
  selectors = selectors @f

-- | Meta-information (record field names, etc.)
instance (Selector s, HasFieldType t) => Selectors (M1 S s (K1 R t)) where
  selectors =
    [(Text.pack $ selName (undefined :: M1 S s (K1 R t) ()), toFieldType @t)]

instance (Selectors a, Selectors b) => Selectors (a :*: b) where
  selectors = selectors @a ++ selectors @b

instance (Selectors a, Selectors b) => Selectors (a :+: b) where
  selectors = selectors @a ++ selectors @b

instance Selectors (Rep a) => Selectors a where
  selectors = selectors @(Rep a)

instance Selectors (Rep a) => Selectors [a] where
  selectors = selectors @(Rep a)

instance (KnitRecord (tables :: Mode -> *) t, Selectors (Rep (t tables 'Unresolved))) => Selectors (t tables) where
  selectors = selectors @(Rep (t tables 'Unresolved))


type TableSchema = [(Text, [Field])]

class ModelSelectors rep where
  modelSelectors :: TableSchema

  schema :: TableSchema
  default schema :: TableSchema
  schema = foldr f [] initialSchema
    where
      kv :: HashMap Text [Field]
      kv = HashMap.fromList initialSchema

      f :: (Text, [Field]) -> TableSchema -> TableSchema
      f (table1, fields1) acc =
        let
          containsMatchingForeignField :: [Field] -> Maybe Field
          containsMatchingForeignField =
            List.find (\(_, fieldType) -> case fieldType of
              Foreign table2 _ _ -> table1 == table2
              _ -> False)

          attachManyToMany :: Field -> ([Field], TableSchema) -> ([Field], TableSchema)
          attachManyToMany field@(_, fieldType) (fields, schemas) =
            case fieldType of
              Foreign table2 _ _ ->
                case containsMatchingForeignField (kv ! table2) of
                  Just (_, fieldType2) -> (fields, schemas <> [(table1 <> "_" <> table2, [("id", Number 11 0), (table1 <> "_id", fieldType2), (table2 <> "_id", fieldType)])])
                  Nothing -> (fields <> [field], schemas)
              _ -> (fields <> [field], schemas)

          additionalInfo :: ([Field], TableSchema)
          additionalInfo = foldr attachManyToMany ([], []) fields1

        in acc <> [(table1, fst additionalInfo)] <> snd additionalInfo

      initialSchema :: TableSchema
      initialSchema = modelSelectors @rep



instance ModelSelectors U1 where
  modelSelectors = []

-- | Meta-information (data types, etc.)
instance ModelSelectors f => ModelSelectors (M1 D x f) where
  modelSelectors = modelSelectors @f

-- | Meta-information (constructor names, etc.)
instance ModelSelectors f => ModelSelectors (M1 C x f) where
  modelSelectors = modelSelectors @f

-- instance (Selector s, Selectors (Rep t)) => ModelSelectors (M1 S s (K1 R t)) where
  -- modelSelectors =
    -- [(Text.pack $ selName (undefined :: M1 S s (K1 R t) ()), selectors @(Rep t))]

instance (Selector s, KnitRecord (tables :: Mode -> *) t, Selectors (Rep (t tables mode))) => ModelSelectors (M1 S s (K1 R [t tables mode])) where
  modelSelectors =
    [(Text.pack $ selName (undefined :: M1 S s (K1 R (t tables mode)) ()), selectors @(Rep (t tables mode)))]

instance (Selector s, KnitRecord (tables :: Mode -> *) t, Selectors (Rep (t tables mode))) => ModelSelectors (M1 S s (K1 R (t tables mode))) where
  modelSelectors =
    [(Text.pack $ selName (undefined :: M1 S s (K1 R (t tables mode)) ()), selectors @(Rep (t tables mode)))]

instance (ModelSelectors a, ModelSelectors b) => ModelSelectors (a :*: b) where
  modelSelectors = modelSelectors @a ++ modelSelectors @b

instance ModelSelectors (model 'Unresolved) => ModelSelectors model where
  modelSelectors = modelSelectors @(model 'Unresolved)

instance ModelSelectors (Rep a) => ModelSelectors a where
  modelSelectors = modelSelectors @(Rep a)

-- class TypeName rep where
  -- typename :: Text

-- instance (Datatype f) => TypeName (M1 D f x) where
  -- typename = Text.pack $ datatypeName (undefined :: M1 D f x a)