{-|
Module: Language.CapNProto.Codegen.Types
Description: Data types used by the code generator.

This module defines data types used to represent haskell modules to be
generated; these are the stage of translation before actually outputting
text files.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.CapNProto.Codegen.Types
    ( TypeName
    , ValName
    , typeName
    , valName
    , Name(..)
    , QName(..)
    , Module(..)
    )
  where

import Data.Char (isAlphaNum, isAscii, isLower, isUpper)
import Data.Word (Word64)

-- | An unqualified Haskell type name
newtype TypeName = TypeName String

-- | An unqualified Haskell value name
newtype ValName = ValName String

instance Show TypeName where
    show (TypeName name) = name

instance Show ValName where
    show (ValName name) = name

-- | Smart constructor for 'TypeName'. If its argument is not a legal Haskell
-- type identifier, this evaluates to an error.
typeName :: String -> TypeName
typeName = TypeName . getName isUpper

-- | Smart constructor for 'ValName'. If its argument is not a legal Haskell
-- value identifier, this evaluates to an error.
valName :: String -> ValName
valName = ValName . getName isLower

-- | @getName pred name@ evaluates to @name@ if:
--
-- * @name@ is composed entirely of one or more ascii alphanumeric characters,
--   underscores, and the single quote character.
-- * The first character of @name@ satisfies the predicate @pred@.
--
-- This is a helper for 'typeName' and 'valName'.
getName :: (Char -> Bool) -> String -> String
getName pred name
    | legalName name = name
    | otherwise = error "illegal name: " ++ show name
  where
    legalName []          = False
    legalName name@(c:cs) = and $ pred c : map legalChar name
    legalChar c = c `elem` "_`" || (isAscii c && isAlphaNum c)

-- | An unqualified Haskell name
data Name
    = Type TypeName -- ^ The name of a type
    | Value ValName -- ^ The name of a value

-- | @QName id name@ is qualified Haskell name, from a generated module for
-- the capnp node with the id @id@
data QName = QName !Word64 Name

-- | A haskell module to be generated
data Module = Module
    { modId          :: !Word64    -- ^ The capnp id for this node.
    , imports        :: [Word64]   -- ^ A list of capnp ids for imported nodes
    , structWrappers :: [TypeName] -- ^ A list of names of structs to be generated.
    }
