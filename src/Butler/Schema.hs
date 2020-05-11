{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Butler.Schema
    ( module Knit
    , Schema(..)
    , Entity(..)
    -- , GenSchemas(genModel)
    ) where

import GHC.Generics (Generic, Rep)
import Butler.TypeInformation
import Knit

-- testShit :: (Show (model m), KnitTables model) => model m -> IO ()
-- testShit = print

data Schema a = Schema { schemaName :: String, schemaFields :: [Field] } deriving (Show)

class Entity a where
    schema :: Schema a

    default schema :: (Generic a, Selectors (Rep a)) => Schema a
    schema = Schema "foo" fields
        where fields = selectors @(Rep a)

-- data Schema a = Schema { schemaName :: Text, schemaFields :: [Field] }

{-
class Entity rs a where
    schema :: Schema a
    default schema :: (Generic a, Selectors (Rep a), TypeName (Rep a)) => Schema a
    schema = Schema constr fields []
        where
            -- TODO: How do I parse the relationships from the type?
            -- rels =  show (Proxy :: Proxy rs)
            rels = "foo"
            fields = selectors @(Rep a)
            constr = Text.toLower $ typename @(Rep a)
data Comment = Comment { commentContent :: Text }
    deriving (Eq, Show, Generic, Entity '[BelongsTo Post])
data Post = Post { postTitle :: Text }
    deriving (Eq, Show, Generic, Entity '[BelongsTo User, HasMany Comment])
data User = User { userName :: Text }
    deriving (Eq, Show, Generic, Entity '[HasMany Post])
-}