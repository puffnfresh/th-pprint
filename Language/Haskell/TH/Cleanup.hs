module Language.Haskell.TH.Cleanup (
  renderNoLength
) where

import Control.Lens
import Language.Haskell.TH
import Language.Haskell.TH.Cleanup.Rules
import Language.Haskell.TH.Lens
import Language.Haskell.TH.PprLib
import qualified Text.PrettyPrint as HPJ

renderNoLength :: Ppr a => a -> String
renderNoLength =
  HPJ.renderStyle (HPJ.style { HPJ.lineLength = maxBound }) . to_HPJ_Doc . ppr

simplifiedTH :: [Dec] -> ExpQ
simplifiedTH =
  stringE . renderNoLength . fmap (transformOf decChildren simplifyDec . removeAllModNames)
