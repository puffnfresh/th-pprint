module Language.Haskell.TH.Cleanup.Rules (
  emptyForallT
, removeEmptyForall
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

removeEmptyForall :: Type -> Type
removeEmptyForall =
  rewriteOf typeChildren emptyForallT

filterModName :: (ModName -> Bool) -> Name -> Name
filterModName f =
  _Name . _2 %~ f'
  where f' n@(NameG _ _ c) =
          if f c then n else NameS
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
  (_SigD . _2 %~ removeEmptyForall) .
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
