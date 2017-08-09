{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies #-}
module Control.Monad.CapNProto.MessageBuilder where

import Control.Monad (when, forM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.RWS.Strict (RWST(runRWST), local, get, put)
import Data.Primitive.ByteArray
    ( MutableByteArray
    , newByteArray
    , sizeofMutableByteArray
    , copyMutableByteArray
    , writeByteArray
    )

import Data.CapNProto.Blob (BlobSlice(..))
import Data.CapNProto.Schema (Field)
import Data.Bits
import Data.Word


-- wrapper types for numbers of bytes & words -- helpful for avoiding mixing
-- up units:
newtype ByteCount = ByteCount Int deriving(Num, Real, Integral, Ord, Eq, Enum)
newtype WordCount = WordCount Int deriving(Num, Real, Integral, Ord, Eq, Enum)

-- conversion functions for the above:
bytesToWords :: ByteCount -> WordCount
bytesToWords (ByteCount n) = WordCount (n `div` 8)
wordsToBytes :: WordCount -> ByteCount
wordsToBytes (WordCount n) = ByteCount (n * 8)

-- | Internal state of a builder.
data BuilderState s = BuilderState
    { nextAlloc :: !ByteCount -- ^ offset into the array for the next allocation
    , array :: MutableByteArray s -- ^ array storing the message being built
    }

data BuilderEnv = BuilderEnv
    { parentOff :: !WordCount -- ^ offset into the array to the start of
                              -- the parent object.
    }

newtype BuilderT p s m a =
    BuilderT (RWST BuilderEnv () (BuilderState s) m a)
    deriving(Functor, Applicative, Monad)

runBuilderT :: (PrimMonad m)
    => BuilderT p (PrimState m) m a
    -> m (BlobSlice (MutableByteArray (PrimState m)), a)
runBuilderT (BuilderT m) = do
    initialArray <- newByteArray 0
    (x, bs, ()) <- runRWST
                       m
                       BuilderEnv   { parentOff = 0 }
                       BuilderState { array = initialArray, nextAlloc = 0 }
    return ( BlobSlice { blob = array bs
                       , offset = 0
                       , sliceLen = fromIntegral $ nextAlloc bs
                       }
           , x
           )


instance MonadTrans (BuilderT p s) where
    lift = BuilderT . lift

ensureSpaceFor :: (PrimMonad m) => ByteCount -> BuilderT p (PrimState m) m ()
ensureSpaceFor (ByteCount sz) = BuilderT $ do
    bs@BuilderState{..} <- get
    when (sizeofMutableByteArray array - fromIntegral nextAlloc < sz) $ do
        array' <- lift $ newByteArray $ sizeofMutableByteArray array * 2 + sz
        copyMutableByteArray array' 0 array 0 (fromIntegral nextAlloc)
        put bs{ array = array' }

alloc :: (PrimMonad m)
    => WordCount -> BuilderT p (PrimState m) m WordCount
alloc szWords = do
    let szBytes = wordsToBytes szWords
    ensureSpaceFor $ szBytes
    bs@BuilderState{..} <- BuilderT get
    BuilderT $ put bs { nextAlloc = nextAlloc + szBytes }
    return $ bytesToWords nextAlloc


withParent :: (PrimMonad m)
    => WordCount -> BuilderT c (PrimState m) m () -> BuilderT p (PrimState m) m WordCount
withParent sz (BuilderT m) = do
    off <- alloc sz
    BuilderT $ local (\env -> env { parentOff = off }) m
    return off

class BuildSelf a where
    buildSelf :: (PrimMonad m, s ~ PrimState m)
        => a -> WordCount -> Word16 -> BuilderT p s m ()

instance BuildSelf Word64 where
    buildSelf n words 0 = BuilderT $ do
        arr <- array <$> get
        let base = fromIntegral $ wordsToBytes words
        forM_ ([0,1..7] :: [Int]) $ \i -> do
            writeByteArray arr (base + i) $ n `shiftR` (i * 8)
    buildSelf _ _ off =
        error $ "call to (Word64) buildSelf with bit offset " ++ show off ++
            " is not Word64-aligned."

setField   :: (PrimMonad m, s ~ PrimState m, BuildSelf c)
    => Field p c -> c                 -> BuilderT p s m ()
buildField :: (PrimMonad m, s ~ PrimState m)
    => Field p c -> BuilderT c s m () -> BuilderT p s m ()

setField = undefined
buildField = undefined
