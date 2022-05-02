-- Just type definitions.

module Replacement where

import Common

import GHC.Generics ( Generic )
import Data.Aeson qualified as Aeson
import Data.Aeson
import Data.Text ( Text )
import Data.Word ( Word32 )

type Repl' end = Repl (W32 end) Text Text

data Repl w p a = Repl
  { replOriginals :: [Original w a]
  , replData :: a
  , replPatches :: [Patch w p]
  } deriving (Generic, Show, Eq)

jcRepl :: Aeson.Options
jcRepl = jcCamelDrop $ length "repl"

instance (FromJSON w, FromJSON a, FromJSON p) => FromJSON (Repl w p a) where
    parseJSON  = genericParseJSON  jcRepl
instance (ToJSON   w, ToJSON   a, ToJSON   p) => ToJSON   (Repl w p a) where
    toEncoding = genericToEncoding jcRepl
    toJSON     = genericToJSON     jcRepl

data Original w a = Original
  { originalVaddr :: w
  , originalData  :: a
  } deriving (Generic, Show, Eq)

jcOriginal :: Aeson.Options
jcOriginal = jcCamelDrop $ length "original"

instance (FromJSON w, FromJSON a) => FromJSON (Original w a) where
    parseJSON  = genericParseJSON  jcOriginal
instance (ToJSON   w, ToJSON   a) => ToJSON   (Original w a) where
    toEncoding = genericToEncoding jcOriginal
    toJSON     = genericToJSON     jcOriginal

-- Used for original and "patch" (asm etc. patching) patches.
data Patch w p = Patch
  { patchVaddr   :: w
  , patchData    :: p
  , patchCompare :: p
  } deriving (Generic, Show, Eq)

jcPatch :: Aeson.Options
jcPatch = jcCamelDrop $ length "patch"

instance (FromJSON w, FromJSON p) => FromJSON (Patch w p) where
    parseJSON  = genericParseJSON  jcPatch
instance (ToJSON   w, ToJSON   p) => ToJSON   (Patch w p) where
    toEncoding = genericToEncoding jcPatch
    toJSON     = genericToJSON     jcPatch

--------------------------------------------------------------------------------

jcCamelDrop :: Int -> Aeson.Options
jcCamelDrop x = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop x
  , Aeson.rejectUnknownFields = True }
