{-# LANGUAGE OverloadedStrings #-}

module Replacement.Patch where

import Replacement

import Binrep.Type.Int

import GHC.Generics ( Generic )

import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as B.Builder
import Data.ByteString.Builder qualified as B ( Builder )
import Numeric.Natural ( Natural )
import Numeric qualified as Numeric
import Data.Word ( Word32, Word16 )
import Data.Bits ( shiftR, (.&.) )
import Data.String ( IsString, fromString )
import Data.Text qualified as Text
import Data.Text ( Text )

type ReplList w p a = [ReplListEntry w p a]
data ReplListEntry w p a = ReplListEntry
  { replListEntryRepl :: Repl w p a
  , replListEntryData :: B.ByteString
  , replListEntryPad  :: Natural
  } deriving (Generic, Show, Eq)

-- We do the null termination!
-- TODO this is where we decide aligning. not hard if we need to add
layoutReplList :: (a -> B.ByteString) -> [Repl w p a] -> ReplList w p a
layoutReplList f = map layout1
  where
    layout1 r = ReplListEntry
      { replListEntryRepl = r
      , replListEntryData = nullTerm $ f $ replData r
      , replListEntryPad  = 0
      }

serializeSegment :: ReplList w p a -> B.ByteString
serializeSegment =
    B.toStrict . B.Builder.toLazyByteString . mconcat . map serializeEntry

serializeEntry :: ReplListEntry w p a -> B.Builder
serializeEntry e =
       bs (replListEntryData e)
    <> bs (B.replicate (fromIntegral $ replListEntryPad e) 0x00)
  where bs = B.Builder.byteString

nullTerm :: B.ByteString -> B.ByteString
nullTerm = flip B.snoc 0x00

genPatches :: forall end w. I 'U 'I4 end -> ReplList w Text Text -> [Patch w Text]
genPatches w = \case
  [] -> []
  (r:rs) ->
    let ps = genReplPatches w $ replListEntryRepl r
        w' = w + fromIntegral (B.length (replListEntryData r)) + fromIntegral (replListEntryPad r)
    in  ps <> genPatches w' rs

-- Takes the vaddr of the given replacement and runs text replacements.
-- TODO disabled orig patches, because they need more complex data ferrying
genReplPatches :: forall end w. I 'U 'I4 end -> Repl w Text Text -> [Patch w Text]
genReplPatches vaddr repl = refPatches -- origPatches <> refPatches
  where
    origPatches = map handleOrig $ replOriginals repl
    refPatches  = map handleRef $ replPatches repl
    handleRef r = Patch
      { patchVaddr   = patchVaddr r
      , patchData    = interpolatePatch Text.replace (getI vaddr) $ patchData r
      , patchCompare = patchCompare r
      }
    handleOrig o = Patch
      { patchVaddr   = originalVaddr o
      -- TODO default value somehow. also, need to separate these!! they're text
      -- patches, not asm!!!
      , patchData    = Text.empty
      , patchCompare = originalData o
      }

-- Polymorphic on replacer. Call like @interpolatePatch Text.replace@
interpolatePatch :: IsString a => (a -> a -> a -> a) -> Word32 -> a -> a
interpolatePatch f w32 = replaces f
  [ ("{vaddr}"       , asHexString w32)
  , ("{vaddr_upper}" , asHexString $ w32beupper16 w32)
  , ("{vaddr_lower}" , asHexString $ w32belower16 w32)
  ] where asHexString a = fromString $ "0x" <> Numeric.showHex a ""

-- In big endian, upper = leftmost, lower = rightmost
w32beupper16, w32belower16 :: Word32 -> Word16
w32beupper16 = fromIntegral . flip shiftR 16
w32belower16 = fromIntegral . (.&. 0x0000ffff)

replaces :: (a -> a -> a -> a) -> [(a, a)] -> a -> a
replaces f = go
  where go []                     haystack = haystack
        go ((needle, replace):rs) haystack = go rs $ f needle replace haystack
