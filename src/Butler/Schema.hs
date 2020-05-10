{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module Butler.Schema where

-- import Data.Proxy (Proxy(..))
import Data.Text (Text)
import GHC.Generics (Generic, Rep)
import Butler.TypeInformation
import qualified Data.Text as Text

data Schema a = Schema { schemaName :: Text, schemaFields :: [Field] }

class Entity rs a where
    schema :: Schema a

    default schema :: (Generic a, Selectors (Rep a), TypeName (Rep a)) => Schema a
    schema = Schema constr fields
        where
            -- TODO: How do I parse the relationships from the type?
            -- rels =  show (Proxy :: Proxy rs)
            fields = selectors @(Rep a)
            constr = Text.toLower $ typename @(Rep a)

{-
data Comment = Comment { commentContent :: Text }
    deriving (Eq, Show, Generic, Entity '[BelongsTo Post])
data Post = Post { postTitle :: Text }
    deriving (Eq, Show, Generic, Entity '[BelongsTo User, HasMany Comment])
data User = User { userName :: Text }
    deriving (Eq, Show, Generic, Entity '[HasMany Post])
-}
