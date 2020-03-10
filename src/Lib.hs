{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import qualified GHC.Generics as GHC
import qualified Env as Env
import qualified Env.Generic as Env
import Generics.Eot
import System.Environment
import Text.Casing

instance (Env.Field e a, Env.AsUnset e, Env.AsUnread e) => Env.Field e (Maybe a) where
  field name help = Env.optional (Env.field name help)

data Person = Person
  { name :: String Env.? "This is the name"
  , age :: Maybe Int
  , heightBig :: Double
  } deriving (GHC.Generic, Show, Env.Record Env.Error)

class GParseEnv a where
  gParseEnv :: Proxy a -> [String] -> IO a

instance GParseEnv () where
  gParseEnv _ [] = pure ()

instance GParseEnv a => GParseEnv (Either a Void) where
  gParseEnv _ fs = Left <$> gParseEnv (Proxy :: Proxy a) fs

instance (Read a, GParseEnv as) => GParseEnv (a, as) where
  gParseEnv p (f:fs) = do
    a  <- getEnv (toScreamingSnake $ fromAny f)
    as <- gParseEnv (Proxy :: Proxy as) fs
    pure (read a, as)

instance {-# OVERLAPPING #-} (GParseEnv as) => GParseEnv (String, as) where
  gParseEnv p (f:fs) = do
    a  <- getEnv (toScreamingSnake $ fromAny f)
    as <- gParseEnv (Proxy :: Proxy as) fs
    pure (a, as)

-- | Parse
class ParseEnv a where
  parseEnv :: Proxy a -> IO a
  default parseEnv :: HasEot a => GParseEnv (Eot a) => Proxy a -> IO a
  parseEnv _ = fromEot <$> (gParseEnv (Proxy :: Proxy (Eot a))) fs
    where
      (Datatype _ [Constructor _ (Selectors fs)])  = datatype (Proxy :: Proxy a)
      (Datatype _ [Constructor _ (NoSelectors _)]) = error "Selectors must be named"
      _ = error "envparse-generic internal error"

someFunc :: IO ()
someFunc = do
  -- p <- parseEnv (Proxy :: Proxy Person)
  p <- Env.parse (Env.header "Example") Env.record :: IO Person
  print p
