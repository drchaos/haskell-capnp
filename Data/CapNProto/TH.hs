{-# LANGUAGE TemplateHaskell #-}
module Data.CapNProto.TH
    ( mkStructWrappers
    , mkListReaders
    )
  where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Word
import Control.Monad.Catch(throwM)

import qualified Data.CapNProto.Errors as E
import qualified Data.CapNProto.Untyped as U

-- | For a type with one data constructor, with the same name as its type
-- constructor, convert a 'Name' for the data constructor to a 'Name' for
-- the type constructor.
inferTypeName :: Name -> Name
inferTypeName (Name occ (NameG DataName pkgName modName)) =
    Name occ (NameG TcClsName pkgName modName)
inferTypeName name = name

mkStructWrapper :: String -> DecQ
mkStructWrapper name = do
    let name' = mkName name
    let b = mkName "b"
    return $ NewtypeD [] name' [PlainTV b] Nothing
                (NormalC name' [ ( Bang NoSourceUnpackedness
                                         NoSourceStrictness
                                  , AppT (ConT ''U.Struct) (VarT b)
                                  )
                                ])
                []

mkStructWrappers :: [String] -> DecsQ
mkStructWrappers = mapM mkStructWrapper


-- | @requireCon con@ constructs a function that matches its argument against
-- the unary data constructor named by @con@, returning the value contained
-- within if the pattern matches, and calling 'throwM' with a
-- 'SchemaViolationError' otherwise.
requireCon :: Name -> ExpQ
requireCon con = do
    result <- newName "result"
    let errmsg = "Expected " ++ show con
    [| \arg -> case arg of
                    $(return $ ConP con [VarP result]) ->
                        return $(return $ VarE result)
                    _ ->
                        throwM $ E.SchemaViolationError $(return $ LitE $ StringL errmsg)
     |]

mkListReader :: String -> Word16 -> Name -> Name -> DecsQ
mkListReader name offset parentData childData = do
    let parentType = inferTypeName parentData
    let childType = inferTypeName childData
    let fnName = mkName name
    struct <- newName "struct"
    body <- mkBody struct
    m <- newName "m"
    b <- newName "b"
    let m' = return $ VarT m
    let b' = return $ VarT b
    ty <- [t| U.ReadCtx $m' $b'
              => $(return $ ConT parentType) $b'
              -> $m' (Maybe (U.ListOf $b' ($(return $ ConT childType) $b'))) |]
    return $ [ SigD fnName ty
             , FunD fnName [Clause [ConP parentData [VarP struct]]
                                   (NormalB body)
                                   []
                           ]
             ]
 where
    childCon = return $ ConE childData
    mkBody struct = do
        [| do ptr <- U.ptrSection $(return $ VarE struct) >>= U.index offset
              case ptr of
                    Nothing -> return Nothing
                    Just ptr' -> do
                        ptrList <- $(requireCon 'U.PtrList) ptr'
                        listStruct <- $(requireCon 'U.ListStruct) ptrList
                        return $ Just $ fmap $(childCon) listStruct |]

mkListReaders :: Name -> [(String, Name, Word16)] -> DecsQ
mkListReaders parent readers = do
    concat <$> mapM mkReader readers
  where
    mkReader (name, child, offset) = mkListReader name offset parent child
