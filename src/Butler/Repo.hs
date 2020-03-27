{-# LANGUAGE
  AllowAmbiguousTypes,
  DeriveGeneric,
  FlexibleContexts,
  FlexibleInstances,
  RankNTypes,
  TypeApplications,
  TypeInType,
  TypeOperators,
  ScopedTypeVariables,
  OverloadedStrings
  #-}

module Butler.Repo where

import Data.Text (Text)
-- import Butler.Schema
-- import Data.Proxy
-- import Type.Reflection (Typeable, SomeTypeRep(..), typeRep)
-- import GHC.Generics
-- import Data.Foldable

-- A Query Binding needs to hold a value
data QueryBinding = String | Number

data Query a = Query { querySql :: Text, queryBindings :: [QueryBinding] }

-- runQuery :: Query a -> a
-- runQuery = _

-- all :: Schema a -> Query [a]
-- all (Schema table p rs) = Query ("SELECT * FROM " <> table) []

-- test :: Schema a -> IO ()
-- test (Schema table p rs) = do
    -- let fields = selectors p
    -- print fields

-- class Selectors rep where
--   selectors :: [(String, SomeTypeRep)]

-- instance Selectors f => Selectors (M1 D x f) where
--   selectors = selectors @f

-- instance Selectors f => Selectors (M1 C x f) where
--   selectors = selectors @f

-- instance (Selector s, Typeable t) => Selectors (M1 S s (K1 R t)) where
--   selectors =
--     [(selName (undefined :: M1 S s (K1 R t) ()) , SomeTypeRep (typeRep @t))]

-- instance (Selectors a, Selectors b) => Selectors (a :*: b) where
--   selectors = selectors @a ++ selectors @b

-- instance Selectors U1 where
--   selectors = []