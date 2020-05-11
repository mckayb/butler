{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Butler.TypeInformation where

import Knit
import Data.Text (Text)
import Data.Proxy (Proxy)
import GHC.Generics (Rep, U1, M1, D, C, Selector(selName), Datatype(datatypeName), S, K1, R, (:*:), (:+:))
import qualified Data.Text as Text

data FieldType = Number Int Int | Varchar Int deriving (Show)

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

instance HasFieldType a => HasFieldType (ForeignRecordId c b a) where
  toFieldType = toFieldType @a

instance HasFieldType a => HasFieldType [ForeignRecordId c b a] where
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

instance (Selectors a, Selectors b) => Selectors (a :+: b) where
  selectors = selectors @a ++ selectors @b

instance Selectors (Rep a) => Selectors [a] where
  selectors = selectors @(Rep a)


type TableSchema = [(Text, [Field])]

-- TODO: How can modelSelectors be smart enough to detect if a relationship is many to many
-- and therefore needs a bridge table?
class ModelSelectors rep where
  modelSelectors :: TableSchema

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

instance ModelSelectors (Rep a) => ModelSelectors a where
  modelSelectors = modelSelectors @(Rep a)

  
class TypeName rep where
  typename :: Text

instance (Datatype f) => TypeName (M1 D f x) where
  typename = Text.pack $ datatypeName (undefined :: M1 D f x a)