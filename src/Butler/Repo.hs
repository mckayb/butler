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

-- A Query Binding needs to hold a value
data QueryBinding = String | Number

data Query a = Query { querySql :: Text, queryBindings :: [QueryBinding] }

-- runQuery :: Query a -> a
-- runQuery = _

-- all :: Schema a -> Query [a]
-- all (Schema table p rs) = Query ("SELECT * FROM " <> table) []
