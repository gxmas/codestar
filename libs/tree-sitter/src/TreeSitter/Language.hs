module TreeSitter.Language
  ( Language (..)
  , loadLanguage
  , languageVersion
  ) where

import Data.Word (Word32)
import Foreign (FunPtr, Ptr, nullFunPtr, nullPtr)
import System.Posix.DynamicLinker

import TreeSitter.FFI (TSLanguageRaw)
import TreeSitter.FFI qualified as FFI


newtype Language = Language (Ptr TSLanguageRaw)

loadLanguage :: FilePath -> String -> IO (Maybe Language)
loadLanguage libPath symbolName = do
  dl <- dlopen libPath [RTLD_LAZY]
  funPtr <- dlsym dl symbolName
  if funPtr == nullFunPtr
    then pure Nothing
    else do
      let langFn = mkLanguageFn funPtr
      langPtr <- langFn
      if langPtr == nullPtr
        then pure Nothing
        else pure (Just (Language langPtr))

languageVersion :: Language -> IO Word32
languageVersion (Language ptr) = fromIntegral <$> FFI.ts_language_version ptr

type LanguageFn = IO (Ptr TSLanguageRaw)

foreign import ccall "dynamic"
  mkLanguageFn :: FunPtr LanguageFn -> LanguageFn
