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

{-
data Person model m = Person
  { name        :: Id model m String
  , loves       :: ForeignId model m "persons" "name" --
  , isPresident :: Bool                               --
  } deriving (Generic, KnitRecord Model)              --
                                                      -- 
data Model m = Model                                  --
  { persons :: Table Model m Person -- <----------------
  } deriving (Generic, KnitTables)

alice :: Person Model 'Unresolved
alice = Person
  { name        = Id "Alice"
  , loves       = ForeignId "Bob"  -- this must be a String, since Model.persons.name is a String!
  , isPresident = False
  }

bob :: Person Model 'Unresolved
bob = Person
  { name        = Id "Bob"
  , loves       = ForeignId "Alice"
  , isPresident = False
  }

model :: Model 'Unresolved
model = Model
  { persons = [alice, bob]  -- `Table` is just a regular list
  }
-}


data User model m = User
    { userId :: Id model m Int
    , userName :: Text
    , userPosts :: [ForeignId model m "postsTable" "postId"]
    , userComments :: [ForeignId model m "commentsTable" "commentId"]
    } deriving (Generic, KnitRecord Model)

deriving instance Show (User Model 'Unresolved)
deriving instance Show (User Model 'Resolved)

data Post model m = Post
    { postId :: Id model m Int
    , postTitle :: Text
    , postUser :: ForeignId model m "usersTable" "userId"
    , postComments :: [ForeignId model m "commentsTable" "commentId"]
    } deriving (Generic, KnitRecord Model)

deriving instance Show (Post Model 'Unresolved)
deriving instance Show (Post Model 'Resolved)

data Comment model m = Comment
    { commentId :: Id model m Int
    , commentText :: Text
    , commentUser :: ForeignId model m "usersTable" "userId"
    , commentPost :: ForeignId model m "postsTable" "postId"
    } deriving (Generic, KnitRecord Model)

deriving instance Show (Comment Model 'Unresolved)
deriving instance Show (Comment Model 'Resolved)

data Model m = Model
    { postsTable :: Table Model m Post
    , usersTable :: Table Model m User
    , commentsTable :: Table Model m Comment
    } deriving (Generic, KnitTables)

deriving instance Show (Model 'Resolved)

user :: User Model 'Unresolved
user = User
    { userId = Id 1
    , userName = "Foo"
    , userPosts = []
    , userComments = []
    }

post :: Post Model 'Unresolved
post = Post
    { postId = Id 1
    , postTitle = "This is a test"
    , postComments = [ForeignId 1, ForeignId 2]
    , postUser = ForeignId 1
    }

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

model :: Model 'Unresolved
model = Model
    { postsTable = [post]
    , usersTable = [user]
    , commentsTable = [comment, comment2]
    }

test :: Model 'Resolved
test = case knit model of
    Right resolved -> resolved
    Left _ -> error "Nope, Bitch"

showUser :: IO ()
showUser = print $ show test

{- model :: Model Unresolved
model = _

test :: Int -> Model Resolved
test = case knit model of
    Right resolved -> resolved
    Left e -> error "Nope, Bitch" -}