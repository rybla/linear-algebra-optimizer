module FileSystem where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)

foreign import data File :: Type

foreign import read :: String -> File

foreign import text_ :: File -> Effect (Promise String)

text :: File -> Aff String
text file = toAffE (text_ file)

foreign import write_ :: String -> String -> Effect (Promise Unit)

write :: String -> String -> Aff Unit
write filename content = toAffE (write_ filename content)
