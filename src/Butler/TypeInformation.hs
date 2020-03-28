{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}

module Butler.TypeInformation where

import GHC.Generics (U1, M1, D, C, Selector(selName), Datatype(datatypeName), S, K1, R, (:*:))
import Type.Reflection (SomeTypeRep(SomeTypeRep), Typeable)
import qualified Type.Reflection as Reflection

class Selectors rep where
  selectors :: [(String, SomeTypeRep)]

-- | Unit - Constructor without arguments
instance Selectors U1 where
  selectors = []

-- | Meta-information (data types, etc.)
instance Selectors f => Selectors (M1 D x f) where
  selectors = selectors @f

-- | Meta-information (constructor names, etc.)
instance Selectors f => Selectors (M1 C x f) where
  selectors = selectors @f

-- | Meta-information (record field names, etc.)
instance (Selector s, Typeable t) => Selectors (M1 S s (K1 R t)) where
  selectors =
    [(selName (undefined :: M1 S s (K1 R t) ()) , SomeTypeRep (Reflection.typeRep @t))]

instance (Selectors a, Selectors b) => Selectors (a :*: b) where
  selectors = selectors @a ++ selectors @b
  
class TypeName rep where
  typename :: String

instance (Datatype f) => TypeName (M1 D f x) where
  typename = datatypeName (undefined :: M1 D f x a)