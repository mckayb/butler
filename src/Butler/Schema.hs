{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module Butler.Schema where

import Data.Proxy (Proxy(..))
import Data.Text (Text)
import GHC.Generics (Generic, Rep)
import Butler.TypeInformation
import qualified Data.Text as Text
import qualified Type.Reflection as Reflection

-- data Schema a = Schema { schemaName :: Text, schemaFields :: [Field], schemaRelationships :: [Relationship] }

{- class Entity rs a where
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

data Relationship where
    HasOne :: (Show a, Reflection.Typeable a, Entity2 a) => a -> Relationship
deriving instance Show (Relationship)

data Schema a = Schema { schemaName :: Text, schemaFields :: [Field], schemaRelationships :: [Relationship] } deriving (Show)
class Entity2 a where
    schema2 :: Schema a

    default schema2 :: (Generic a, Selectors (Rep a), TypeName (Rep a)) => Schema a
    schema2 = Schema constr fields []
        where
            fields = selectors @(Rep a)
            constr = Text.toLower $ typename @(Rep a)

data Key a = Key a

-- data Relationship2 a b where
    -- HasOne' :: (Entity2 a, Entity2 b) => a -> b -> Relationship2 a b

data Comment = Comment { commentContent :: Text }
    deriving (Eq, Show, Generic, Entity2)
