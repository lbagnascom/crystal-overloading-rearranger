{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeResolution.Fix where

newtype Fix t = Fix (t (Fix t))

deriving instance (Show (t (Fix t))) => (Show (Fix t))

deriving instance (Eq (t (Fix t))) => (Eq (Fix t))

fromFix :: Fix t -> t (Fix t)
fromFix (Fix t) = t
