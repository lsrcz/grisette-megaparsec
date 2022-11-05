{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Grisette.Lib.Text.Megaparsec where

import Control.Monad.Trans
import Grisette.Core
import Text.Megaparsec
import Text.Megaparsec.Internal

instance (Stream s, MonadGenSymFresh m) => MonadGenSymFresh (ParsecT e s m) where
  nextGenSymIndex = lift nextGenSymIndex
  getGenSymIdent = lift getGenSymIdent

instance (SymBoolOp bool, GUnionLike bool m) => GMergeable bool (ParsecT e s m a) where
  gmergingStrategy = SimpleStrategy unionIf

instance (SymBoolOp bool, GUnionLike bool m) => GMergeable1 bool (ParsecT e s m) where
  liftGMergingStrategy _ = SimpleStrategy unionIf

instance (SymBoolOp bool, GUnionLike bool m) => GSimpleMergeable bool (ParsecT e s m a) where
  gmrgIte = unionIf

instance (SymBoolOp bool, GUnionLike bool m) => GSimpleMergeable1 bool (ParsecT e s m) where
  liftGMrgIte _ = unionIf

instance (SymBoolOp bool, GUnionLike bool m) => GUnionLike bool (ParsecT e s m) where
  single x = ParsecT $ \s _ _ eok _ -> eok x s mempty
  unionIf cond (ParsecT l) (ParsecT r) =
    ParsecT $ \s cok cerr eok eerr -> unionIf cond (l s cok cerr eok eerr) (r s cok cerr eok eerr)
  mergeWithStrategy _ = id
