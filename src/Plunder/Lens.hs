module Plunder.Lens
  ( makePostfixLenses
  ) where

import Control.Lens.TH
import Language.Haskell.TH.Syntax
import Control.Lens


-- | like 'makeLenses', but postfixes on lens
makePostfixLenses :: Name -> Q [Dec]
makePostfixLenses = makeLensesWith (set lensField postfixLens lensRules)

postfixLens :: FieldNamer
postfixLens = mappingNamer $ pure . flip mappend "Lens"
