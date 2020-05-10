{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Butler.Repo where

import Data.Text (Text)

-- A Query Binding needs to hold a value
data QueryBinding = String | Number

data Query a = Query { querySql :: Text, queryBindings :: [QueryBinding] }

-- runQuery :: Query a -> a
-- runQuery = _

-- all :: Schema a -> Query [a]
-- all (Schema table p rs) = Query ("SELECT * FROM " <> table) []
