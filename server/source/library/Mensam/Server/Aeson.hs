module Mensam.Server.Aeson where

import Data.Kind
import Deriving.Aeson qualified as A
import GHC.TypeLits

type JSONSettings :: Symbol -> Symbol -> [Type]
type JSONSettings constructorPrefix fieldPrefix =
  '[ A.ConstructorTagModifier
      '[ A.StripPrefix constructorPrefix
       , A.CamelToKebab
       ]
   , A.SumTaggedObject "tag" "value"
   , A.FieldLabelModifier
      '[ A.StripPrefix fieldPrefix
       , A.CamelToKebab
       ]
   , A.RejectUnknownFields
   ]
