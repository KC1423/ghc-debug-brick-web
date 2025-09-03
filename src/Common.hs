{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Common where

import Lens.Micro
import Namespace

data ProfileLevel = OneLevel | TwoLevel deriving Show

