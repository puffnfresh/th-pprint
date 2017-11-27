module Language.Haskell.TH.Cleanup.Lens (
  _FunD
, _SigD
, _Name
, _Match
, decChildren
, typeChildren
, expChildren
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
import Language.Haskell.TH.Syntax

_FunD :: Prism' Dec (Name, [Clause])
_FunD = prism (\(x1_0, x2_1) -> FunD x1_0 x2_1) (\x_2 -> case x_2 of
                                                             FunD y1_3 y2_4 -> Right (y1_3, y2_4)
                                                             _ -> Left x_2)

_SigD :: Prism' Dec (Name, Type)
_SigD = prism (\(a, b) -> SigD a b) (\a -> case a of
                                        SigD b c  -> Right (b, c)
                                        _ -> Left a)

_Name :: Iso' Name (OccName, NameFlavour)
_Name = iso (\(Name x1_0 x2_1) -> (x1_0, x2_1)) (\(x1_2, x2_3) -> Name x1_2 x2_3)

_Match :: Iso' Match (Pat, Body, [Dec])
_Match = iso (\(Match x1_0 x2_1 x3_2) -> (x1_0, x2_1, x3_2)) (\(x1_3, x2_4, x3_5) -> Match x1_3 x2_4 x3_5)

decChildren :: Traversal' Dec Dec
decChildren f (ClassD a b c d e) =
  ClassD a b c d <$> traverse f e
decChildren f (InstanceD a b c d) =
  InstanceD a b c <$> traverse f d
decChildren _ a =
  pure a

typeChildren :: Traversal' Type Type
typeChildren f (ForallT a b c) =
  ForallT a b <$> f c
typeChildren f (AppT a b) =
  AppT <$> f a <*> f b
typeChildren f (SigT a b) =
  flip SigT b <$> f a
typeChildren f (InfixT a b c) =
  InfixT <$> f a <*> pure b <*> f c
typeChildren f (UInfixT a b c) =
  UInfixT <$> f a <*> pure b <*> f c
typeChildren f (ParensT a) =
  ParensT <$> f a
typeChildren _ a =
  pure a

expChildren :: Traversal' Exp Exp
expChildren f (AppE a b) =
  AppE <$> f a <*> f b
expChildren f (InfixE a b c) =
  InfixE <$> traverse f a <*> f b <*> traverse f c
expChildren f (UInfixE a b c) =
  UInfixE <$> f a <*> f b <*> f c
expChildren f (ParensE a) =
  ParensE <$> f a
expChildren f (LamE a b) =
  LamE a <$> f b
expChildren f (TupE a) =
  TupE <$> traverse f a
expChildren f (UnboxedTupE a) =
  UnboxedTupE <$> traverse f a
expChildren f (CondE a b c) =
  CondE <$> f a <*> f b <*> f c
expChildren f (MultiIfE a) =
  MultiIfE <$> (traverse . traverse) f a
expChildren f (LetE a b) =
  LetE a <$> f b
expChildren f (CaseE a b) =
  CaseE <$> f a <*> (traverse . _Match . _2 . bodyExp) f b
expChildren f (ListE a) =
  ListE <$> traverse f a
expChildren f (SigE a b) =
  flip SigE b <$> f a
expChildren f (RecConE a b) =
  RecConE a <$> (traverse . traverse) f b
expChildren f (RecUpdE a b) =
  RecUpdE <$> f a <*> (traverse . traverse) f b
expChildren f (StaticE a) =
  StaticE <$> f a
expChildren _ a =
  pure a

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
  (typeChildren . typeName) f a

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
  (expChildren . expName) f a

patName :: Traversal' Pat Name
patName f (VarP a) =
  VarP <$> f a
patName f (ConP a b) =
  ConP <$> f a <*> (traverse . patName) f b
patName _ a =
  pure a

bodyExp :: Traversal' Body Exp
bodyExp f (GuardedB a) =
  GuardedB <$> (traverse . traverse) f a
bodyExp f (NormalB a) =
  NormalB <$> f a
