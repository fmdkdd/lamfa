{-# LANGUAGE FlexibleInstances,
             UndecidableInstances,
             ScopedTypeVariables
  #-}

module AOP.Internal.Typeable1Monad (
 Typeable1Monad (..),
 module Data.Typeable,
 module AOP.Internal.PolyTypeable,
 module Control.Monad,
 module Control.Monad.Identity,
 module Control.Monad.Trans,
 module Control.Monad.State,
 module Control.Monad.Writer,
 module Control.Monad.Reader,
 module Control.Monad.Cont,
 module Control.Monad.Error,
 -- module Control.Monad.Views,
) where

import Data.Typeable
import AOP.Internal.PolyTypeable
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.Error
-- import Control.Monad.Views
-- import Control.Monad.Mask

{- | Support for PolyTypeable when using monads and monad transformers -}

class (Typeable1 m, Monad m) => Typeable1Monad m

instance (Typeable1 m, Monad m) => Typeable1Monad m

instance Typeable1 Identity where
         typeOf1 _ = mkTyConApp (mkTyCon3 "Control" "Monad" "Identity") []

-- instance Typeable1Monad m => Typeable1 (IdentityT m) where
--          typeOf1 _ = mkTyConApp (mkTyCon3 "Control" "Monad" "IdentityT") []

instance (Typeable1Monad m, Typeable s) => Typeable1 (StateT s m) where
         typeOf1 _ = mkTyConApp (mkTyCon3 "Control" "Monad" "StateT") [typeOf (undefined :: s), typeOf1 (undefined :: m ())]

instance (Typeable1Monad m, Typeable s) => Typeable1 (WriterT s m) where
         typeOf1 _ = mkTyConApp (mkTyCon3 "Control" "Monad" "WriterT") [typeOf (undefined :: s), typeOf1 (undefined :: m ())]

instance (Typeable1Monad m, Typeable s) => Typeable1 (ErrorT s m) where
        typeOf1 _ = mkTyConApp (mkTyCon3 "Control" "Monad" "ErrorT")
                               [typeOf (undefined :: s), typeOf1 (undefined :: m ())]

-- instance (Typeable1Monad m, Typeable s) => Typeable1 (MonadStateReaderT s m) where
--          typeOf1 _ = mkTyConApp (mkTyCon3 "Control" "Monad" "MonadStateReaderT") [typeOf (undefined :: s), typeOf1 (undefined :: m ())]

-- instance MonadIO m => MonadIO (IdentityT m) where
--          liftIO = lift . liftIO

-- instance (Typeable1Monad m, Typeable s) => Typeable1 (TStateT t s m) where
--          typeOf1 _ = mkTyConApp (mkTyCon3 "Control" "Monad" "TStateT")
--                                 [typeOf (undefined :: s), typeOf1 (undefined :: m ())]

-- instance (Typeable1Monad m, Typeable s) => Typeable1 (TErrorT t s m) where
--         typeOf1 _ = mkTyConApp (mkTyCon3 "Control" "Monad" "TErrorT")
--                                [typeOf (undefined :: s), typeOf1 (undefined :: m ())]