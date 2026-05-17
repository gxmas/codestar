{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.MCP.Generators () where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.Scientific (Scientific, fromFloatDigits)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.QuickCheck

import Network.MCP.Codec
import Network.MCP.Types
import Network.MCP.Types.Capabilities
import Network.MCP.Types.Content

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

shortText :: Gen Text
shortText = T.pack <$> listOf1 (elements ['a' .. 'z'])

shortBS :: Gen ByteString
shortBS = BS.pack <$> resize 32 (listOf arbitrary)

shortMimeType :: Gen Text
shortMimeType = elements ["image/png", "image/jpeg", "audio/wav", "audio/mp3", "text/plain", "application/octet-stream"]

arbitrarySizedScientific :: Gen Scientific
arbitrarySizedScientific = do
  intPart <- choose (-10000, 10000 :: Int)
  fracPart <- choose (0, 9999 :: Int)
  let val = fromIntegral intPart + fromIntegral fracPart / 10000.0 :: Double
  pure (fromFloatDigits val)

arbitraryValue :: Int -> Gen Value
arbitraryValue 0 =
  oneof
    [ String <$> shortText
    , Number <$> arbitrarySizedScientific
    , Bool <$> arbitrary
    , pure Null
    ]
arbitraryValue depth =
  oneof
    [ String <$> shortText
    , Number <$> arbitrarySizedScientific
    , Bool <$> arbitrary
    , pure Null
    , Array . V.fromList <$> resize 3 (listOf (arbitraryValue (depth - 1)))
    , Object . KM.fromList <$> resize 3 (listOf ((,) <$> (Key.fromText <$> shortText) <*> arbitraryValue (depth - 1)))
    ]

-- | Generate a Value that is guaranteed to be a non-empty Object (for params/result).
-- Empty objects are avoided because they are semantically equivalent to absent
-- params and cannot be distinguished after meta injection/extraction.
arbitraryObjectValue :: Gen Value
arbitraryObjectValue =
  Object . KM.fromList <$> resize 3 (listOf1 ((,) <$> (Key.fromText <$> shortText) <*> arbitraryValue 1))

arbitraryMeta :: Gen Meta
arbitraryMeta = HM.fromList <$> resize 2 (listOf ((,) <$> shortText <*> arbitraryValue 1))

------------------------------------------------------------------------
-- Scalar newtypes
------------------------------------------------------------------------

instance Arbitrary RequestId where
  arbitrary = RequestId <$> oneof [Left <$> shortText, Right . abs <$> arbitrary]

instance Arbitrary ProgressToken where
  arbitrary = ProgressToken <$> oneof [Left <$> shortText, Right . abs <$> arbitrary]

instance Arbitrary Cursor where
  arbitrary = Cursor <$> shortText

instance Arbitrary ProtocolVersion where
  arbitrary = ProtocolVersion <$> shortText

instance Arbitrary Timestamp where
  arbitrary = Timestamp <$> shortText

------------------------------------------------------------------------
-- Enums
------------------------------------------------------------------------

instance Arbitrary LoggingLevel where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Role where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary IconTheme where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary TaskStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary TaskErrorKind where
  arbitrary = arbitraryBoundedEnum

------------------------------------------------------------------------
-- Types module
------------------------------------------------------------------------

instance Arbitrary RPCError where
  arbitrary = RPCError <$> arbitrary <*> shortText <*> liftArbitrary (arbitraryValue 1)

instance Arbitrary Implementation where
  arbitrary = Implementation <$> shortText <*> shortText <*> liftArbitrary shortText <*> liftArbitrary shortText

instance Arbitrary JSONRPCRequest where
  arbitrary =
    JSONRPCRequest
      <$> arbitrary
      <*> shortText
      <*> liftArbitrary arbitraryObjectValue
      <*> liftArbitrary arbitraryMeta

instance Arbitrary JSONRPCNotification where
  arbitrary =
    JSONRPCNotification
      <$> shortText
      <*> liftArbitrary arbitraryObjectValue
      <*> liftArbitrary arbitraryMeta

instance Arbitrary JSONRPCResult where
  arbitrary =
    JSONRPCResult
      <$> arbitrary
      <*> arbitraryObjectValue
      <*> liftArbitrary arbitraryMeta

instance Arbitrary JSONRPCError where
  arbitrary = JSONRPCError <$> liftArbitrary arbitrary <*> arbitrary

instance Arbitrary MCPMessage where
  arbitrary =
    oneof
      [ MCPRequest <$> arbitrary
      , MCPNotification <$> arbitrary
      , MCPResult <$> arbitrary
      , MCPError <$> arbitrary
      ]

------------------------------------------------------------------------
-- Content types
------------------------------------------------------------------------

instance Arbitrary URI where
  arbitrary = URI <$> shortText

instance Arbitrary Annotations where
  arbitrary =
    Annotations
      <$> liftArbitrary (resize 2 (listOf arbitrary))
      <*> liftArbitrary (choose (0.0, 1.0))
      <*> liftArbitrary arbitrary

instance Arbitrary Icon where
  arbitrary =
    Icon
      <$> arbitrary
      <*> liftArbitrary shortMimeType
      <*> liftArbitrary (resize 2 (listOf shortText))
      <*> liftArbitrary arbitrary

instance Arbitrary TextContent where
  arbitrary = TextContent <$> shortText <*> liftArbitrary arbitrary

instance Arbitrary ImageContent where
  arbitrary = ImageContent <$> shortBS <*> shortMimeType <*> liftArbitrary arbitrary

instance Arbitrary AudioContent where
  arbitrary = AudioContent <$> shortBS <*> shortMimeType <*> liftArbitrary arbitrary

instance Arbitrary ResourceLink where
  arbitrary = ResourceLink <$> arbitrary <*> shortText <*> liftArbitrary shortMimeType <*> liftArbitrary arbitrary

instance Arbitrary TextResource where
  arbitrary = TextResource <$> arbitrary <*> liftArbitrary shortMimeType <*> shortText

instance Arbitrary BlobResource where
  arbitrary = BlobResource <$> arbitrary <*> liftArbitrary shortMimeType <*> shortBS

instance Arbitrary ResourceContents where
  arbitrary = oneof [ResourceText <$> arbitrary, ResourceBlob <$> arbitrary]

instance Arbitrary EmbeddedResource where
  arbitrary = EmbeddedResource <$> arbitrary <*> liftArbitrary arbitrary

instance Arbitrary ContentBlock where
  arbitrary =
    oneof
      [ ContentText <$> arbitrary
      , ContentImage <$> arbitrary
      , ContentAudio <$> arbitrary
      , ContentLink <$> arbitrary
      , ContentEmbedded <$> arbitrary
      ]

------------------------------------------------------------------------
-- Capabilities
------------------------------------------------------------------------

instance Arbitrary RootsCapability where
  arbitrary = RootsCapability <$> liftArbitrary arbitrary

instance Arbitrary SamplingCapability where
  arbitrary = pure SamplingCapability

instance Arbitrary ElicitationCapability where
  arbitrary = ElicitationCapability <$> arbitrary <*> arbitrary

instance Arbitrary PromptsCapability where
  arbitrary = PromptsCapability <$> liftArbitrary arbitrary

instance Arbitrary ResourcesCapability where
  arbitrary = ResourcesCapability <$> liftArbitrary arbitrary <*> liftArbitrary arbitrary

instance Arbitrary ToolsCapability where
  arbitrary = ToolsCapability <$> liftArbitrary arbitrary

instance Arbitrary TasksClientCapability where
  arbitrary = pure TasksClientCapability

instance Arbitrary TasksServerCapability where
  arbitrary = pure TasksServerCapability

instance Arbitrary ClientCapabilities where
  arbitrary =
    ClientCapabilities
      <$> liftArbitrary arbitrary
      <*> liftArbitrary arbitrary
      <*> liftArbitrary arbitrary
      <*> liftArbitrary arbitrary
      <*> liftArbitrary (HM.fromList <$> resize 2 (listOf ((,) <$> shortText <*> arbitraryValue 1)))

instance Arbitrary ServerCapabilities where
  arbitrary =
    ServerCapabilities
      <$> liftArbitrary arbitrary
      <*> liftArbitrary arbitrary
      <*> liftArbitrary arbitrary
      <*> liftArbitrary (pure ())
      <*> liftArbitrary (pure ())
      <*> liftArbitrary arbitrary
      <*> liftArbitrary (HM.fromList <$> resize 2 (listOf ((,) <$> shortText <*> arbitraryValue 1)))

instance Arbitrary NegotiatedCapabilities where
  arbitrary = NegotiatedCapabilities <$> arbitrary <*> arbitrary

instance Arbitrary ToolAnnotations where
  arbitrary =
    ToolAnnotations
      <$> liftArbitrary shortText
      <*> liftArbitrary arbitrary
      <*> liftArbitrary arbitrary
      <*> liftArbitrary arbitrary
      <*> liftArbitrary arbitrary

instance Arbitrary TaskError where
  arbitrary = TaskError <$> arbitrary <*> shortText

------------------------------------------------------------------------
-- Codec
------------------------------------------------------------------------

instance Arbitrary CodecErrorKind where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CodecError where
  arbitrary = CodecError <$> arbitrary <*> shortText
