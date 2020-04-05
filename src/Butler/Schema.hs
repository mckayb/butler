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

data Schema a = Schema { schemaName :: Text, schemaFields :: [Field], schemaRelationships :: [Relationship] }

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


data Comment = Comment { commentContent :: Text }
    deriving (Eq, Show, Generic, Entity '[BelongsTo Post])
data Post = Post { postTitle :: Text }
    deriving (Eq, Show, Generic, Entity '[BelongsTo User, HasMany Comment])
data User = User { userName :: Text }
    deriving (Eq, Show, Generic, Entity '[HasMany Post])