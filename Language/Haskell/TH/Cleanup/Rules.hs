module Language.Haskell.TH.Cleanup.Rules (
  emptyForallT
, filterModName
, removeModName
, removeAllModNames
, simplifyDec
, simplifyClause
) where

import Control.Lens
import Language.Haskell.TH
import Language.Haskell.TH.Cleanup.Lens
import Language.Haskell.TH.Syntax

emptyForallT :: Type -> Maybe Type
emptyForallT (ForallT [] _ a) =
  Just a
emptyForallT _ =
  Nothing

filterModName :: (ModName -> Bool) -> Name -> Name
filterModName f =
  _Name . _2 %~ f'
  where
    f' (NameG _ _ c) | not (f c)=
      NameS
    f' n =
      n

removeModName :: Name -> Name
removeModName =
  filterModName (const False)

removeAllModNames :: Dec -> Dec
removeAllModNames =
  decName %~ removeModName

simplifyDec :: Dec -> Dec
simplifyDec =
  (rewriteOn (_SigD . _2) emptyForallT) .
  (_FunD . _2 . single %~ simplifyClause)

single :: Traversal' [a] a
single f [a] =
  (:[]) <$> f a
single _ xs =
  pure xs

simplifyClause :: Clause -> Clause
simplifyClause (Clause [] (NormalB (LamE pats b)) []) =
  Clause pats (NormalB b) []
simplifyClause c =
  c
