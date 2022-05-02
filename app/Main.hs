{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common
import Replacement
import Replacement.Patch
import Elf

import GHC.Generics ( Generic )

import Data.ByteString qualified as B
import Control.Monad.IO.Class
import Data.Yaml qualified as Yaml
import Data.Yaml.Pretty qualified as Yaml.Pretty
import Data.Aeson ( FromJSON, ToJSON )
import Data.Map qualified as Map
import Data.Map ( Map )
import Data.List.NonEmpty ( NonEmpty( (:|) ) )
import Data.Text.Encoding qualified as Text
import Data.Text ( Text )
import Data.Foldable qualified as Foldable

import StreamPatch.Simple qualified as SP
import StreamPatch.Patch ( SeekKind(..) )
import StreamPatch.Patch.Compare ( Via(..), EqualityCheck(..) )

main :: IO ()
main = tmpParsePatch

tmpParsePatch :: MonadIO m => m ()
tmpParsePatch = do
    -- parse replacements file
    replacements <- decodeYamlFile @[Repl' 'LE] "tmp/example-instructions.yaml"

    -- lay out replacements, then serialize to get new segment data
    let replacements' = layoutReplList Text.encodeUtf8 replacements
        segData       = serializeSegment replacements'

    -- load ELF file into memory. we'll keep coming back to it
    elfBs <- liftIO $ B.readFile "tmp/eboot-orig.elf"

    -- read the program header table. we'll be amending it, and we need it for
    -- locating the segment of vaddr patches
    let pht = readProgramHeaderTable @'LE elfBs

    -- unsafely calculate the new segment vaddr based on game's ELF layout
    let segVaddr = calculateAppendSegVaddr pht

    -- fixup pht: add an entry pointing to our new segment. but that requires
    -- that we shift the phent_offset for every other segment. this is all
    -- highly specific and unsafe
    let pht' = fixupPht 0x20 segVaddr (fromIntegral (B.length segData)) pht

    -- generate a patchscript using the replacements and the initial vaddr
    let patchscript = genPatches segVaddr replacements'

    -- patches use plain vaddr (because it's clear), which needs to be aligned
    -- depending on segment, so naively locate patches via (new!) pht info
    let patchscript' = locatePatches (phtSegVaddrPaddrMap pht') patchscript

    -- format patchscript for bytepatch :)
    let patchscript'' = outputPatches patchscript'
        patchscriptYamlBs = encodeYamlPretty patchscript''

    -- we have the patches, the data and the new pht. time to finish up: patch
    -- the top ELF header to fix file offsets changed by new pht, and replace
    -- the pht (all in one). SUPER unsafe
    let elfBs' = replaceProgramHeaderTable elfBs pht'

    -- finally, append the data to the end of the file. if all the maths lined
    -- up, it will be at the same file offset as specified in the new pht
    let elfBs'WithNewSeg = elfBs' <> segData

    liftIO $ B.writeFile "tmp/gen-patches.yaml" patchscriptYamlBs
    liftIO $ B.writeFile "tmp/eboot-patched-gen.elf" elfBs'WithNewSeg

-- TODO Align? Can I go straight after, or do I need some padding?
-- TODO 2022-05-02T13:53:57+0100 raehik: Looks like going straight after gives
-- me a nice failure (simply can't load). I think I need to manually align to
-- the segment's align. But I cba to calculate that, so I'm just using the value
-- that worked for me before when I did this manually.
calculateAppendSegVaddr :: ProgramHeaderTable end -> W32 end
calculateAppendSegVaddr (_:p:_) = 0x811fa000 -- phentVaddr p + phentMemsz p

fixupPht :: W32 end -> W32 end -> W32 end -> ProgramHeaderTable end -> ProgramHeaderTable end
fixupPht w vaddr size (seg1:seg2:seg3:[]) =
    let seg1' = fixupPhent w seg1
        seg2' = fixupPhent w seg2
        seg3' = fixupPhent w seg3
        segNewOffset = phentOffset seg3' + phentFilesz seg3' -- TODO align?
        segNew = genPhent segNewOffset vaddr size
    in  [seg1', seg2', segNew, seg3']

-- Shift phent offset by given amount (phent_size * x).
fixupPhent :: W32 end -> ProgramHeaderEntry end -> ProgramHeaderEntry end
fixupPhent w p = p
  { phentOffset = phentOffset p + w }

genPhent :: W32 end -> W32 end -> W32 end -> ProgramHeaderEntry end
genPhent o vaddr size = ProgramHeaderEntry
  { phentType = 1 -- = LOAD
  , phentOffset = o
  , phentVaddr = vaddr
  , phentPaddr = 0 -- same as others
  , phentFilesz = size
  , phentMemsz = size
  , phentFlags = 4 -- = R (read-only, no write, no execute)
  , phentAlign = 0x10 -- copied from others
  }

-- Paddr is real. Vaddr can be aligned to paddr using segment offsets (via
-- enclosing map).
data LocatedPatch w a = LocatedPatch
  { locatedPatchVaddr   :: w
  , locatedPatchPaddr   :: w
  , locatedPatchData    :: a
  , locatedPatchCompare :: a
  } deriving (Generic, Show, Eq)

phtSegVaddrPaddrMap :: ProgramHeaderTable end -> Map (W32 end) (W32 end)
phtSegVaddrPaddrMap = Map.fromList . map go
  where go p = (phentVaddr p, phentOffset p)

-- Takes data from program header table: start vaddr -> paddr.
locatePatches
    :: forall w a. (Ord w, Num w)
    => Map w w -> [Patch w a] -> Map w (NonEmpty (LocatedPatch w a))
locatePatches pht = foldr insertPatchInMap Map.empty . map go
  where
    insertPatchInMap
        :: (w, LocatedPatch w a)
        -> Map w (NonEmpty (LocatedPatch w a))
        -> Map w (NonEmpty (LocatedPatch w a))
    insertPatchInMap (w, p) m = Map.insertWith (<>) w (p :| []) m
    go :: Patch w a -> (w, LocatedPatch w a)
    go r =
        case Map.lookupLE (patchVaddr r) pht of
          Nothing -> error "vaddr comes before any segments"
          -- TODO cleanup algebra, explain. segv - segp means we can use
          -- unsigned and negate later (stupid but that's how my code is)
          Just (segVaddr, segPaddr) ->
              let paddr = patchVaddr r + segPaddr - segVaddr
                  locp = locatePatch r paddr
               in (segVaddr - segPaddr, locp)

locatePatch :: Patch w a -> w -> LocatedPatch w a
locatePatch p paddr = LocatedPatch
  { locatedPatchVaddr   = patchVaddr p
  , locatedPatchPaddr   = paddr
  , locatedPatchData    = patchData p
  , locatedPatchCompare = patchCompare p
  }

outputPatches
    :: Integral w
    => Map w (NonEmpty (LocatedPatch w Text))
    -> [SP.Aligned (SP.MultiPatch 'RelSeek ('ViaEq 'Exact) Text)]
outputPatches = map (uncurry fmtPatchGroup) . Map.toList

fmtPatchGroup
    :: Integral w
    => w
    -> NonEmpty (LocatedPatch w a)
    -> SP.Aligned (SP.MultiPatch 'RelSeek ('ViaEq 'Exact) a)
fmtPatchGroup w lps = SP.Aligned
  { SP.alignedAlign   = negate $ fromIntegral w
  , SP.alignedPatches = Foldable.toList $ fmap fmtPatch lps
  }

fmtPatch
    :: Integral w
    => LocatedPatch w a
    -> SP.MultiPatch 'RelSeek ('ViaEq 'Exact) a
fmtPatch lp = SP.MultiPatch
  { SP.mpData = locatedPatchData lp
  , SP.mpAt   = [seek]
  } where seek = SP.Seek
            { SP.sSeek = fromIntegral $ locatedPatchVaddr lp
            , SP.sCompare = Just $ locatedPatchCompare lp
            , SP.sNullTerminates = Nothing
            , SP.sMaxBytes = Nothing
            , SP.sAligned = Just $ fromIntegral $ locatedPatchPaddr lp
            }

decodeYamlFile :: forall a m. (FromJSON a, MonadIO m) => FilePath -> m a
decodeYamlFile = liftIO . Yaml.decodeFileThrow

encodeYamlPretty :: ToJSON a => a -> B.ByteString
encodeYamlPretty = Yaml.Pretty.encodePretty cfg
  where
    cfg = Yaml.Pretty.setConfCompare f $ Yaml.Pretty.setConfDropNull True Yaml.Pretty.defConfig
    f "data" _ = LT
    f _ "data" = GT
    f "seek" _ = LT
    f _ "seek" = GT
    f "compare" _ = LT
    f _ "compare" = GT
    f x y = compare x y
