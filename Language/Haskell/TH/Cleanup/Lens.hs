module Language.Haskell.TH.Cleanup.Lens (
  _FunD
, _SigD
, _Name
, typeName
, clauseName
, decName
, matchName
, expName
, patName
, bodyExp
) where

import Control.Lens
import Language.Haskell.TH
import Language.Haskell.TH.Lens
import Language.Haskell.TH.Syntax

_Name :: Iso' Name (OccName, NameFlavour)
_Name = iso (\(Name x1_0 x2_1) -> (x1_0, x2_1)) (\(x1_2, x2_3) -> Name x1_2 x2_3)

typeName :: Traversal' Type Name
typeName f (VarT a) =
  VarT <$> f a
typeName f (ConT a) =
  ConT <$> f a
typeName f (PromotedT a) =
  PromotedT <$> f a
typeName f (InfixT a b c) =
  flip (InfixT a) c <$> f b
typeName f (UInfixT a b c) =
  flip (UInfixT a) c <$> f b
typeName f a =
  (plate . typeName) f a

clauseName :: Traversal' Clause Name
clauseName f (Clause a b c) =
  Clause <$> (traverse . patName) f a <*> (bodyExp . expName) f b <*> (traverse . decName) f c

decName :: Traversal' Dec Name
decName f (FunD a b) =
  FunD <$> f a <*> (traverse . clauseName) f b
decName f (ValD a b c) =
  ValD <$> patName f a <*> (bodyExp . expName) f b <*> (traverse . decName) f c
decName f (ClassD a b c d e) =
  ClassD <$> (traverse . typeName) f a <*> f b <*> pure c <*> pure d <*> (traverse . decName) f e
decName f (InstanceD a b c d) =
  InstanceD a <$> (traverse . typeName) f b <*> typeName f c <*> (traverse . decName) f d
decName f (SigD a b) =
  SigD <$> f a <*> typeName f b
decName f (DefaultSigD a b) =
  DefaultSigD <$> f a <*> typeName f b
decName f (TySynInstD a (TySynEqn b c)) =
  TySynInstD <$> f a <*> (TySynEqn <$> (traverse . typeName) f b <*> typeName f c)
decName _ a =
  pure a

matchName :: Traversal' Match Name
matchName f (Match a b c) =
  Match <$> patName f a <*> (bodyExp . expName) f b <*> (traverse . decName) f c

expName :: Traversal' Exp Name
expName f (VarE a) =
  VarE <$> f a
expName f (ConE a) =
  ConE <$> f a
expName f (LamE a b) =
  LamE <$> (traverse . patName) f a <*> expName f b
expName f (LetE a b) =
  LetE <$> (traverse . decName) f a <*> expName f b
expName f (CaseE a b) =
  CaseE <$> expName f a <*> (traverse . matchName) f b
expName f a =
  (plate . expName) f a

patName :: Traversal' Pat Name
patName f (VarP a) =
  VarP <$> f a
patName f (ConP a b) =
  ConP <$> f a <*> (traverse . patName) f b
patName _ a =
  pure a

bodyExp :: Traversal' Body Exp
bodyExp =
  failing _NormalB (_GuardedB . traverse . _2)
