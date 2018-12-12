module App.Common where

import Data.Generic.Rep (class Generic)
import Data.String.Extra (snakeCase)
import Foreign (Foreign, F)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Generic.Class (class GenericDecode, class GenericEncode)
import Foreign.Generic.Types (Options)

foreignGenericOptions :: Options
foreignGenericOptions = defaultOptions { unwrapSingleConstructors = true, fieldTransform = snakeCase }

foreignGenericDecode :: ∀ a rep. Generic a rep => GenericDecode rep => Foreign -> F a
foreignGenericDecode = genericDecode foreignGenericOptions

foreignGenericEncode :: ∀ a rep. Generic a rep => GenericEncode rep => a -> Foreign
foreignGenericEncode = genericEncode foreignGenericOptions
