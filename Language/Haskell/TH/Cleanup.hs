module Language.Haskell.TH.Cleanup (
  renderNoLength
, simplifiedTH
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

-- | Simplifies and pretty-prints declarations. Will give back a quoted string.
--
-- Can be used from GHCi like so:
--
-- > putStrLn $(simplifiedTH =<< makePrisms ''Either)
simplifiedTH :: [Dec] -> ExpQ
simplifiedTH =
  stringE . renderNoLength . fmap (transformOf decChildren simplifyDec . removeAllModNames)
