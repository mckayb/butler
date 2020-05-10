{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances  #-}

module Butler.KnitTest where

import GHC.Generics (Generic)
import Data.Text (Text)
import Knit


data User model m = User
    { userId :: Id model m Int
    , userName :: Text
    , userPosts :: [ForeignId model m "posts" "postId"]
    , userComments :: [ForeignId model m "comments" "commentId"]
    } deriving (Generic, KnitRecord Model)

deriving instance Show (User Model 'Unresolved)
deriving instance Show (User Model 'Resolved)

data Post model m = Post
    { postId :: Id model m Int
    , postTitle :: Text
    , postUser :: ForeignId model m "users" "userId"
    , postComments :: [ForeignId model m "comments" "commentId"]
    } deriving (Generic, KnitRecord Model)

deriving instance Show (Post Model 'Unresolved)
deriving instance Show (Post Model 'Resolved)

data Comment model m = Comment
    { commentId :: Id model m Int
    , commentText :: Text
    , commentUser :: ForeignId model m "users" "userId"
    , commentPost :: ForeignId model m "posts" "postId"
    } deriving (Generic, KnitRecord Model)

deriving instance Show (Comment Model 'Unresolved)
deriving instance Show (Comment Model 'Resolved)

data A model m = A
    { aId :: Id model m Int
    , aName :: Text
    , aBs :: [ForeignId model m "bs" "bId"]
    } deriving (Generic, KnitRecord Model)

deriving instance Show (A Model 'Unresolved)
deriving instance Show (A Model 'Resolved)

data B model m = B
    { bId :: Id model m Int 
    , bName :: Text
    , bAs :: [ForeignId model m "as" "aId"]
    } deriving (Generic, KnitRecord Model)

deriving instance Show (B Model 'Unresolved)
deriving instance Show (B Model 'Resolved)

data Model m = Model
    { posts :: Table Model m Post
    , users :: Table Model m User
    , comments :: Table Model m Comment
    , as :: Table Model m A
    , bs :: Table Model m B
    } deriving (Generic, KnitTables)

deriving instance Show (Model 'Resolved)

-- One to Many User to Post
-- One to Many User to Post
user :: User Model 'Unresolved
user = User
    { userId = Id 1
    , userName = "Foo"
    , userPosts = []
    , userComments = []
    }

-- One to Many Post to Comment
-- Many to One Post to User
post :: Post Model 'Unresolved
post = Post
    { postId = Id 1
    , postTitle = "This is a test"
    , postComments = [ForeignId 1, ForeignId 2]
    , postUser = ForeignId 1
    }

-- Many to One Comment to Post
-- Many to One Comment to User
comment :: Comment Model 'Unresolved
comment = Comment
    { commentId = Id 1
    , commentText = "One more time"
    , commentUser = ForeignId 1
    , commentPost = ForeignId 1
    }

comment2 :: Comment Model 'Unresolved
comment2 = Comment
    { commentId = Id 2
    , commentText = "One more time"
    , commentUser = ForeignId 1
    , commentPost = ForeignId 1
    }

-- Many to Many A to B
a :: A Model 'Unresolved
a = A
    { aId = Id 1
    , aName = "Foo"
    , aBs = [ForeignId 1]
    }

b :: B Model 'Unresolved
b = B
    { bId = Id 1
    , bName = "Bar"
    , bAs = [ForeignId 1]
    }

model :: Model 'Unresolved
model = Model
    { posts = [post]
    , users = [user]
    , comments = [comment, comment2]
    , as = [a]
    , bs = [b]
    }

test :: Model 'Resolved
test = case knit model of
    Right resolved -> resolved
    Left _ -> error "Nope"

showModel :: IO ()
showModel = print $ show test