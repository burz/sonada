module Synthax.Quoter
( synthax
) where

import Synthax.Parser

import Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Quote

synthax :: QuasiQuoter
synthax = QuasiQuoter
    { quoteExp = \s -> appE (varE 'parseString) (litE $ stringL s)
    }

