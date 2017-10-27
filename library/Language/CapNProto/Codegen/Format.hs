{-|
Module: Language.CapNProto.Codegen.Format
Description: Generate Haskell source code

This module converts the types in 'Language.CapNProto.Codegen.Types' into
textual Haskell source code.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Language.CapNProto.Codegen.Format (fmtModule) where

import Language.CapNProto.Codegen.Types

import Data.Monoid (mconcat)
import Data.Word   (Word64)
import Text.Printf (printf)

import qualified Data.Text.Lazy.Builder as TB

-- | Convert a 'Module' to a 'TB.Builder', containing the text of a
-- corresponding Haskell source file.
fmtModule :: Module -> TB.Builder
fmtModule Module{..} = mconcat
    [ "module Schema.CapNProto.Reader.", fmtId modId, " where\n"
    , "\n"
    , "import qualified Data.Word\n"
    , "import qualified Data.Int\n"
    , "import qualified Data.CapNProto.Untyped\n"
    , mconcat $ map fmtImport imports
    , "\n"
    , mconcat $ map fmtStructWrapper structWrappers
    ]

fmtImport :: Word64 -> TB.Builder
fmtImport id = mconcat
    ["import qualified Schema.CapNProto.Reader.", idB, " as ", idB, "\n"]
  where
    idB = fmtId id

fmtStructWrapper :: TypeName -> TB.Builder
fmtStructWrapper name = mconcat
    ["newtype ", nameB, " b = ", nameB, " (Data.CapNProto.Untyped.Struct b)\n"]
  where
    nameB = fmtTypeName name

fmtTypeName :: TypeName -> TB.Builder
fmtTypeName = TB.fromString . show

fmtValName :: ValName -> TB.Builder
fmtValName = TB.fromString . show

fmtId :: Word64 -> TB.Builder
fmtId = TB.fromString . printf "X%08x"
