module Testing where

import Binrep
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.Int

import Data.Word
import Data.Bits
import Numeric.Natural
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Text ( Text )

import Data.Map qualified as Map

-- [(BS.ByteString, Natural, [Source], [Patch])]

{-
data Replacement = Replacement
  { rData :: BS.ByteString
  , rPad :: Natural
  , rSources :: [Source]
  , rPatches :: [Patch]
  }
-}

data ReplaceData = ReplaceData
  { rdData :: BS.ByteString
  , rdPad  :: Natural
  }

-- Add empty bin metas after to form '[Const Bin.MetaPrep, Compare.Meta 'ViaEq, Bin.Meta]
{-
f
    :: (Text -> a)
    -> [(Text, [Patch])]
    -> [Patch 'AbsSeek [Compare.Meta 'ViaEq] a]
-}

---

-- | Partial ELF file header.
data ELFFileHeaderPart (s :: ISize) (e :: Endianness) = ELFFileHeader
  { e_phnum     :: I 'U 'I2 e
  , e_entry     :: I 'U s   e
  , e_phoff     :: I 'U s   e
  , e_phentsize :: I 'U 'I2 e
  }

-- | Prepare a patch for the given ELF file header that increments @e_phnum@,
--   and return some data parsed from the header (which will be useful for the
--   patching).
--
-- This does all the work required on the ELF file header.
--
-- TODO actually need to thread errors due to decode, but cba
{-
elfHandleFileHeader
    :: Natural
    -> BS.ByteString
    -> (Patch 'AbsSeek '[Compare.Meta ('ViaEq 'Exact)] BS.ByteString, ELFFileHeaderPart 'I4 'LE)
elfHandleFileHeader newSegs bs = (patch, addedPhLen)
  where
    patch      = Patch e_phnum' 0x2c meta
    e_phnum'   = e_phnum + fromIntegral newSegs
    Right e_phnum = binDecodeAt @(I 'U 'I2 'LE) 0x2c bs
    meta       = BP.metaWrap1 $ Compare.Meta $ Just $ Bin.binEncode e_phnum
    addedPhLen = fromIntegral e_phentsize * newSegs
    Right e_phentsize = binDecodeAt @(I 'U 'I2 'LE) 0x2a bs

binDecodeAt :: BinaryCodec a => Int -> BS.ByteString -> Either String a
binDecodeAt offset = Bin.binDecode . BS.drop offset
-}

---

data Replace a = Replace
  { replaceAddress :: Word32
  -- ^ We need to know which segment this address belongs to, to allow us to
  --   find the physical offset. We could just use the physical offset, but
  --   we're better than that. We could make the user write it, but "guessing"
  --   by comparing it to segment ranges seems awfully safe.

  , replaceData    :: a
  , replaceCompare :: Maybe a
  }

{-
f
    :: Word32 -> [Replace Text]
    -> ([Patch 'AbsSeek '[Compare.Meta ('ViaEq 'Exact)] Text], BS.ByteString)
f p_offset = foldr go ([], BS.empty)
  where
    go (Replace a d mc) (patches, bs) = undefined
-}

-- | Calculate a vaddr's associated paddr by guessing its containing segment.
--
-- We can guess the containing segment by finding the closest/largest smaller
-- seg_vaddr_start = @Map.lookupLE@. This isn't foolproof, but it's good enough,
-- because we offload patching to bytepatch with safety checks.
--
-- Errors if you give a vaddr not in any segments.
calculate_paddr_via_closest_segment
    :: (Num (IRep 'U s), Ord (IRep 'U s))
    => [ElfProgHeadEntryPart s e] -> I 'U s e -> I 'U s e
calculate_paddr_via_closest_segment pheps vaddr = paddr
  where
    paddr = vaddr - p_vaddr seg + seg_p_offset
    Just (seg_p_offset, seg) = Map.lookupLE vaddr phepMap
    phepMap = Map.fromList $ map (\phep -> (p_vaddr phep, phep)) pheps

data ElfProgHeadEntryPart (s :: ISize) (e :: Endianness) = ElfProgHeadEntryPart
  { p_offset :: I 'U s e
  , p_vaddr  :: I 'U s e
  , p_extra_name :: Text
  }
