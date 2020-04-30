{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}

module Butler.TypeInformation where

import Data.Text (Text)
import Data.Proxy (Proxy)
import GHC.Generics (U1, M1, D, C, Selector(selName), Datatype(datatypeName), S, K1, R, (:*:))
import Type.Reflection (Typeable)
import Data.Kind (Type)
import qualified Data.Text as Text


data FieldType = Number Int Int | Varchar Int deriving (Show)

class HasFieldType a where
    toFieldType :: FieldType

instance HasFieldType Text where
    toFieldType = Varchar 255

instance HasFieldType String where
    toFieldType = Varchar 255

instance HasFieldType Int where
    toFieldType = Number 11 0

instance HasFieldType a => HasFieldType (Proxy a) where
    toFieldType = toFieldType @a

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
  
class TypeName rep where
  typename :: Text

instance (Datatype f) => TypeName (M1 D f x) where
  typename = Text.pack $ datatypeName (undefined :: M1 D f x a)


-- data Relationship = HasOne Type
--     | BelongsTo Type
--     | HasMany Type
--     | BelongsToMany Type
--     deriving (Show)

-- class RelationshipSelectors rep where
--   relationshipSelectors :: [Relationship]

-- instance RelationshipSelectors U1 where
  -- relationshipSelectors = []

-- instance RelationshipSelectors

