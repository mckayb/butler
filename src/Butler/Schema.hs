{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module Butler.Schema where

import Data.Text (Text)
import GHC.Generics (Generic, Rep)
import Butler.TypeInformation
import qualified Data.Text as Text

-- data Relationship where
--     HasOne :: (b :: *) -> Relationship
--     HasMany :: (b :: *) -> Relationship
--     BelongsTo :: (b :: *) -> Relationship
--     BelongsToMany :: (b :: *) -> Relationship

data Relationship = HasMany * | BelongsToMany *
--type HasMany (a :: k) = a

-- data Relationship = HasOne (Entity)

-- data Relationship r where
    -- HasOne :: (b :: *) -> Relationship r

data Schema a = Schema { schemaName :: Text, schemaFields :: [Field], schemaRelationships :: [Int] } deriving (Show)

-- class Entity a where
--     schema :: Schema rs a

--     default schema :: (Generic a, Selectors (Rep a), TypeName (Rep a)) => Schema rs a
--     schema = Schema constr fields []
--         where
--             fields = selectors @(Rep a)
--             constr = Text.toLower $ typename @(Rep a)

class Entity (rs :: [Relationship]) a where
    schema :: Schema a

    default schema :: (Generic a, Selectors (Rep a), TypeName (Rep a)) => Schema a
    schema = Schema constr fields []
        where
            fields = selectors @(Rep a)
            constr = Text.toLower $ typename @(Rep a)


-- data User = User { userId :: Int, userName :: Text } deriving (Eq, Show, Generic, Entity)
data Foo = Foo { fooId :: Int } deriving (Eq, Show, Generic, Entity '[BelongsToMany User])
data User = User { userId :: Int, userName :: Text } deriving (Eq, Show, Generic, Entity '[HasMany Foo])