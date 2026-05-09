{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Shared 'Arbitrary' instances for all JSON-RPC types. Every
-- instance defines 'shrink' for useful QuickCheck failure
-- minimization.
module Test.Network.JsonRpc.Generators
  (
  ) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Scientific (Scientific, fromFloatDigits)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Network.JsonRpc.Types.Internal
import Test.QuickCheck hiding (Failure, Success)

-- ====================================================================
-- Primitive generators
-- ====================================================================

-- | Bounded scientific values to avoid JSON round-trip edge cases.
arbitrarySizedScientific :: Gen Scientific
arbitrarySizedScientific = do
  -- Stay within [-1e15, 1e15] with at most 6 decimal places
  intPart <- choose (-1000000, 1000000 :: Int)
  fracPart <- choose (0, 999999 :: Int)
  let val = fromIntegral intPart + fromIntegral fracPart / 1000000.0 :: Double
  pure (fromFloatDigits val)

-- | Bounded 'Value' generator with depth limit to avoid divergence.
arbitraryValue :: Int -> Gen Value
arbitraryValue 0 =
  oneof
    [ String <$> arbitraryText
    , Number <$> arbitrarySizedScientific
    , Bool <$> arbitrary
    , pure Null
    ]
arbitraryValue depth =
  oneof
    [ String <$> arbitraryText
    , Number <$> arbitrarySizedScientific
    , Bool <$> arbitrary
    , pure Null
    , Array . V.fromList <$> resize 3 (listOf (arbitraryValue (depth - 1)))
    , Object . KM.fromList <$> resize 3 (listOf ((,) <$> (Key.fromText <$> arbitraryText) <*> arbitraryValue (depth - 1)))
    ]

arbitraryText :: Gen Text
arbitraryText = T.pack <$> listOf (elements (['a' .. 'z'] ++ ['0' .. '9']))

-- ====================================================================
-- Instances
-- ====================================================================

instance Arbitrary Id where
  arbitrary =
    oneof
      [ IdText <$> arbitraryText
      , IdNum <$> arbitrarySizedScientific
      ]
  shrink (IdText t) = IdText <$> shrinkText t
  shrink (IdNum _) = []

instance Arbitrary Params where
  arbitrary =
    oneof
      [ ParamsByPosition . V.fromList <$> resize 5 (listOf (arbitraryValue 2))
      , ParamsByName . KM.fromList <$> resize 5 (listOf ((,) <$> (Key.fromText <$> arbitraryText) <*> arbitraryValue 2))
      , pure NoParams
      ]
  shrink (ParamsByPosition v) = ParamsByPosition . V.fromList <$> shrinkList (const []) (V.toList v)
  shrink (ParamsByName _) = [NoParams]
  shrink NoParams = []

instance Arbitrary ErrorObject where
  arbitrary =
    ErrorObject
      <$> arbitrary
      <*> arbitraryText
      <*> oneof [pure Nothing, Just <$> arbitraryValue 2]
  shrink (ErrorObject c m d) =
    [ErrorObject c' m d | c' <- shrink c]
      ++ [ErrorObject c m' d | m' <- shrinkText m]
      ++ [ErrorObject c m Nothing | Just _ <- [d]]

instance Arbitrary Request where
  arbitrary =
    Request
      <$> arbitraryText
      <*> arbitrary
      <*> arbitrary
  shrink (Request m p i) =
    [Request m' p i | m' <- shrinkText m]
      ++ [Request m p' i | p' <- shrink p]
      ++ [Request m p i' | i' <- shrink i]

instance Arbitrary Notification where
  arbitrary =
    Notification
      <$> arbitraryText
      <*> arbitrary
  shrink (Notification m p) =
    [Notification m' p | m' <- shrinkText m]
      ++ [Notification m p' | p' <- shrink p]

instance Arbitrary Success where
  arbitrary =
    Success
      <$> arbitraryValue 2
      <*> arbitrary
  shrink (Success r i) =
    [Success r i' | i' <- shrink i]

instance Arbitrary Failure where
  arbitrary =
    Failure
      <$> arbitrary
      <*> arbitrary
  shrink (Failure e i) =
    [Failure e' i | e' <- shrink e]
      ++ [Failure e i' | i' <- shrink i]

instance Arbitrary Response where
  arbitrary =
    oneof
      [ ResponseSuccess <$> arbitrary
      , ResponseFailure <$> arbitrary
      ]
  shrink (ResponseSuccess s) = ResponseSuccess <$> shrink s
  shrink (ResponseFailure f) = ResponseFailure <$> shrink f

instance Arbitrary Message where
  arbitrary =
    oneof
      [ MsgRequest <$> arbitrary
      , MsgNotification <$> arbitrary
      , MsgResponse <$> arbitrary
      ]
  shrink (MsgRequest r) = MsgRequest <$> shrink r
  shrink (MsgNotification n) = MsgNotification <$> shrink n
  shrink (MsgResponse r) = MsgResponse <$> shrink r

-- Note: Arbitrary Value is already provided by aeson >= 2.2.

-- | Helper to shrink 'Text' values.
shrinkText :: Text -> [Text]
shrinkText t = T.pack <$> shrink (T.unpack t)
