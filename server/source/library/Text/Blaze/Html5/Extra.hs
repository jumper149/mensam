module Text.Blaze.Html5.Extra where

import Text.Blaze.Html5
import Text.Blaze.Internal

-- | This should be exported from `Text.Blaze.Html5`, but isn't.
s :: Html -> Html
s = Parent "s" "<s" "</s>"
