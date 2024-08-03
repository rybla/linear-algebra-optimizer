module Utility where

import Prelude
import Partial.Unsafe (unsafeCrashWith)

unreachable msg = unsafeCrashWith $ "[unreachable] " <> msg

todo _a = unsafeCrashWith "TODO"

