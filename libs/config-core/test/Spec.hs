module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Hedgehog ((===))
import qualified Hedgehog as H

import Config.Core

main :: IO ()
main = defaultMain $ testGroup "config-core"
  [ testProperty "session overrides project overrides global" prop_scopePrecedence
  , testProperty "unset removes a key" prop_unset
  , testProperty "type mismatch fails validation" prop_typeMismatch
  ]

prop_scopePrecedence :: H.Property
prop_scopePrecedence = H.property $ do
  cfg <- H.evalIO newConfiguration
  _ <- H.evalIO $ setConfig cfg Global  (ConfigKey "k") (CString "global")
  _ <- H.evalIO $ setConfig cfg Project (ConfigKey "k") (CString "project")
  _ <- H.evalIO $ setConfig cfg Session (ConfigKey "k") (CString "session")
  result <- H.evalIO (getMerged cfg (ConfigKey "k"))
  result === Just (CString "session")

prop_unset :: H.Property
prop_unset = H.property $ do
  cfg <- H.evalIO newConfiguration
  _ <- H.evalIO $ setConfig cfg Global (ConfigKey "k") (CString "v")
  H.evalIO $ unsetConfig cfg Global (ConfigKey "k")
  result <- H.evalIO (getConfig cfg (Just Global) (ConfigKey "k"))
  result === Nothing

prop_typeMismatch :: H.Property
prop_typeMismatch = H.property $ do
  cfg <- H.evalIO newConfiguration
  let schema = ConfigSchema
        { csKey         = ConfigKey "num"
        , csType        = TInt
        , csDefault     = Nothing
        , csRequired    = False
        , csValidator   = Nothing
        , csDescription = "a number"
        }
  H.evalIO (registerSchema cfg schema)
  result <- H.evalIO (setConfig cfg Global (ConfigKey "num") (CString "not a number"))
  case result of
    Left (ValidationError _ msg) -> msg === "type mismatch"
    Right ()                     -> H.failure
