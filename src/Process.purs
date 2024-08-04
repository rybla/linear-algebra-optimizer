module Process where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)

foreign import spawn_
  :: Array String
  -> Effect (Promise { stderr :: String, stdout :: String })

spawn :: Array String -> Aff { stderr :: String, stdout :: String }
spawn args = toAffE (spawn_ args)
