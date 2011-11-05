-- Copyright 2011 Jared Hance

{-# LANGUAGE FlexibleContexts, TypeOperators #-}
module Data.Binary.Derive
    (
    derivePut,
    deriveGet
    )
where

import Control.Applicative
import Data.Binary
import GHC.Generics

data ConsChoice = L | R

instance Binary ConsChoice where
    put L = put True
    put R = put False
    get = do b <- get
             case b of
                True -> return L
                False -> return R

-- | Derives a `put` function for an instance of Binary. Normally you won't
-- call this from anywhere except that `put` function in your instance
-- declaration.
derivePut :: (Generic t, GBinary (Rep t)) => t -> Put
derivePut = gput . from

-- | Derives a `get` value for an instance of Binary. Normally you won't use
-- this from anywhere except that `get` value in your instance declaration.
deriveGet :: (Generic t, GBinary (Rep t)) => Get t
deriveGet = gget >>= return . to

class GBinary f where
    gput :: f t -> Put
    gget :: Get (f t)

instance GBinary U1 where
    gput U1 = return ()
    gget = return U1

instance Binary t => GBinary (K1 i t) where
    gput (K1 x) = put x
    gget = do x <- get
              return $ K1 x

instance GBinary t => GBinary (M1 i c t) where
    gput (M1 x) = gput x
    gget = do x <- gget
              return $ M1 x

instance (GBinary a, GBinary b) => GBinary (a :+: b) where
    gput (L1 x) = put L >> gput x
    gput (R1 x) = put R >> gput x
    gget = do t <- get
              case t of
                L -> do x <- gget
                        return $ L1 x
                R -> do x <- gget
                        return $ R1 x

instance (GBinary a, GBinary b) => GBinary (a :*: b) where
    gput (x :*: y) = do gput x
                        gput y
    gget = do (:*:) <$> gget <*> gget
