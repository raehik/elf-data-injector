module Common
  ( module Common
  , module Binrep.Type.Common
  ) where

import Binrep.Type.Int
import Binrep.Type.Common

type W8             = I 'U 'I1 'LE
type W16        end = I 'U 'I2 end
type W32        end = I 'U 'I4 end
type Vaddr size end = I 'U size end
type Paddr size end = I 'U size end -- ^ paddrs are also sized?
