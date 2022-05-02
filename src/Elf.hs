module Elf where

import Common

import Binrep
import Binrep.Type.Int
import Binrep.Type.Magic
import Binrep.Generic qualified as BR
import Binrep.Generic
import Data.ByteString qualified as B
import Data.Serialize.Get qualified as Cereal
import GHC.Generics ( Generic )
import Control.Monad ( replicateM )
import Data.Word ( Word16, Word32 )

-- Magic, 32/64, endianness, version
data TopHeader = TopHeader
  { topHeaderMagic :: Magic '[0x7F, 0x45, 0x4C, 0x46]
  }

data Header (size :: ISize) (end :: Endianness) = Header
  { headerAbi :: W8
  , headerAbiVerAndPadding :: W8 -- ^ because idc and 7-byte padding is annoying
  , headerType :: W16 end
  , headerMachine :: W16 end
  , headerVersion :: W16 end -- ^ ignoring
  , headerEntry :: Vaddr size end
  , headerPhoff :: Paddr size end
  , headerShoff :: Paddr size end
  , headerFlags :: W32 end
  , headerEhsize :: W16 end
  , headerPhentsize :: W16 end
  , headerPhnum :: W16 end
  , headerShentsize :: W16 end
  , headerShnum :: W16 end
  , headerShstrndx :: W16 end
  }

-- 64-bit and 32-bit has different layout (according to Wikipedia). So no
-- polymorphism possible.
--
-- 0x20 bytes long
data ProgramHeaderEntry (end :: Endianness) = ProgramHeaderEntry
  { phentType :: W32 end
  , phentOffset :: W32 end
  , phentVaddr :: W32 end
  , phentPaddr :: W32 end
  , phentFilesz :: W32 end
  , phentMemsz :: W32 end
  , phentFlags :: W32 end
  , phentAlign :: W32 end
  } deriving (Generic, Show, Eq)

instance                  BLen (ProgramHeaderEntry end) where blen = blenGeneric BR.cDef
instance Put (W32 end) => Put  (ProgramHeaderEntry end) where put  = putGeneric  BR.cDef
instance Get (W32 end) => Get  (ProgramHeaderEntry end) where get  = getGeneric  BR.cDef

type ProgramHeaderTable end = [ProgramHeaderEntry end]

readProgramHeaderTable
    :: forall end. (Get (W32 end), Get (W16 end)) => B.ByteString -> [ProgramHeaderEntry end]
readProgramHeaderTable bs =
    case Cereal.runGet (getPhtInfo @end) bs of
      Left e -> error e
      Right (e_phoff, e_phentsize, e_phnum) -> do
        let parsePht = replicateM (fI e_phnum) $ get @(ProgramHeaderEntry end)
            phtBs  = B.take (fromIntegral (e_phentsize * e_phnum)) $ B.drop (fI e_phoff) bs
        case Cereal.runGet parsePht phtBs of
          Left e -> error e
          Right pht -> pht
  where
    fI :: (Integral a, Num b) => a -> b
    fI = fromIntegral

getPhtInfo :: (Get (W16 end), Get (W32 end)) => Cereal.Get (W32 end, W16 end, W16 end)
getPhtInfo = do
    Cereal.skip 0x1C
    e_phoff <- get
    Cereal.skip 0x0A
    e_phentsize <- get
    e_phnum <- get
    return (e_phoff, e_phentsize, e_phnum)

{-
e_phentsize should always be 0x20 but use given.
* read e_phoff@0x1C, e_phentsize@0x2A, e_phnum@0x2C
* let ph_size_old = e_phentsize * e_phnum
      ph_size_new = e_phentsize * List.length entries
* patch: e_entry = e_entry + (ph_size_new - ph_size_old)
* patch: e_phnum++
* jump to e_phoff
* drop (e_phentsize * e_phnum) bytes
* serialize program entries and insert
* join it all up and serve
-}

replaceProgramHeaderTable
    :: forall end. (Get (W16 end), Get (W32 end), Put (W16 end), Put (W32 end))
    => B.ByteString -> ProgramHeaderTable end -> B.ByteString
replaceProgramHeaderTable bs pht = do
    case Cereal.runGet getPhtInfo bs of
      Left e -> error e
      Right (e_phoff, e_phentsize, e_phnum) ->
        let e_phnum' = fI (length pht) :: W16 end
            e_phoff' = e_phoff + fI (e_phnum' - e_phnum) * (fI e_phentsize)
            phtOldLen = e_phentsize * e_phnum

            (elfTop1, elfTop2) = B.splitAt 0x2C bs
            elfTop2Patched = runPut e_phnum' <> B.drop 2 elfTop2
            elfPatched = elfTop1 <> elfTop2Patched

            (elfTop, elfPhtAndLater) = B.splitAt 0x34 elfPatched
            elfLaterNoPht = B.drop (fI phtOldLen) elfPhtAndLater
            elfPht'AndLater = runPut pht <> elfLaterNoPht
            elf = elfTop <> elfPht'AndLater

        in  elf

  where
    fI :: (Integral a, Num b) => a -> b
    fI = fromIntegral

--------------------------------------------------------------------------------

-- | nice simple segment data type that stores metadata + contents
data Segment (size :: ISize) (end :: Endianness) = Segment
  { segmentType  :: W32 end
  , segmentVaddr :: Vaddr size end
  , segmentData  :: B.ByteString
  }
