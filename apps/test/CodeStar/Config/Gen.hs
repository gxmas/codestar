{-# OPTIONS_GHC -Wno-orphans #-}
module CodeStar.Config.Gen where

import Data.Map.Strict qualified as Map
import Data.Monoid (Last (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Test.QuickCheck

import CodeStar.Config.Types
import CodeStar.Types (PlanningMode (..))

-- --------------------------------------------------------------------
-- Primitive generators and shrinkers
-- --------------------------------------------------------------------

arbitraryConfigText :: Gen Text
arbitraryConfigText = Text.pack <$> listOf1 (elements (['a'..'z'] ++ ['0'..'9'] ++ ['-', '_']))

-- Biased 70/30 toward Just so generated configs have meaningful field content.
-- 50/50 caused most PartialConfig values to be nearly empty, making monoid
-- law tests pass trivially on identity-like inputs.
arbitraryLast :: Gen a -> Gen (Last a)
arbitraryLast g = Last <$> frequency [(70, Just <$> g), (30, pure Nothing)]

shrinkConfigText :: Text -> [Text]
shrinkConfigText = map Text.pack . filter (not . null) . shrink . Text.unpack

shrinkMaybeText :: Maybe Text -> [Maybe Text]
shrinkMaybeText Nothing  = []
shrinkMaybeText (Just t) = Nothing : map Just (shrinkConfigText t)

-- Shrink a Last: Nothing stays Nothing; Just shrinks to Nothing then to smaller Just values.
shrinkLast :: (a -> [a]) -> Last a -> [Last a]
shrinkLast _ (Last Nothing)  = []
shrinkLast f (Last (Just x)) = Last Nothing : map (Last . Just) (f x)

-- --------------------------------------------------------------------
-- Enum instances
-- --------------------------------------------------------------------

instance Arbitrary TelemetryMode where
  arbitrary = elements [minBound .. maxBound]
  shrink TelemetryOtlp   = []
  shrink TelemetryStderr = [TelemetryOtlp]
  shrink TelemetryOff    = [TelemetryOtlp, TelemetryStderr]

instance Arbitrary PlanningMode where
  arbitrary = elements [minBound .. maxBound]
  -- Shrink toward minBound along the enum ordering.
  shrink x = take (fromEnum x) [minBound ..]

instance Arbitrary IndexStrategy where
  arbitrary = elements [NoIndex, RepoMapIndex, SemanticIndex]
  shrink NoIndex       = []
  shrink RepoMapIndex  = [NoIndex]
  shrink SemanticIndex = [NoIndex, RepoMapIndex]

instance Arbitrary SandboxMode where
  arbitrary = pure NoSandbox
  shrink _ = []

instance Arbitrary AuthMode where
  arbitrary = pure NoAuth
  shrink (JwtAuth _) = [NoAuth]
  shrink _ = []

instance Arbitrary McpTransport where
  arbitrary = elements [StdioTransport, HttpTransport]
  shrink StdioTransport = []
  shrink HttpTransport  = [StdioTransport]

-- --------------------------------------------------------------------
-- Composite instances
-- --------------------------------------------------------------------

instance Arbitrary ModelSpec where
  arbitrary =
    ModelSpec
      <$> arbitraryConfigText
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink (ModelSpec n temp tp mt) =
    [ModelSpec n' temp tp mt | n'    <- shrinkConfigText n]   ++
    [ModelSpec n temp' tp mt | temp' <- shrink temp]          ++
    [ModelSpec n temp tp' mt | tp'   <- shrink tp]            ++
    [ModelSpec n temp tp mt' | mt'   <- shrink mt]

instance Arbitrary ModelEntry where
  arbitrary =
    ModelEntry
      <$> arbitraryConfigText
      <*> elements ["anthropic", "openai"]
      <*> arbitraryConfigText
      <*> (ApiKey <$> arbitraryConfigText)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink (ModelEntry n prv mdl k tmp tp mt) =
    [ModelEntry n' prv mdl k tmp tp mt | n'   <- shrinkConfigText n] ++
    [ModelEntry n prv mdl k' tmp tp mt | k'   <- shrink k]

instance Arbitrary McpEndpoint where
  arbitrary =
    McpEndpoint
      <$> arbitraryConfigText
      <*> arbitraryConfigText
      <*> listOf arbitraryConfigText
      <*> (Map.fromList <$> listOf ((,) <$> arbitraryConfigText <*> arbitraryConfigText))
      <*> arbitrary
      <*> pure Nothing
  shrink (McpEndpoint n cmd as env' tr au) =
    [McpEndpoint n' cmd as env' tr au  | n'   <- shrinkConfigText n]              ++
    [McpEndpoint n cmd' as env' tr au  | cmd' <- shrinkConfigText cmd]            ++
    [McpEndpoint n cmd as' env' tr au  | as'  <- shrinkList shrinkConfigText as]  ++
    [McpEndpoint n cmd as env'' tr au  | env'' <- shrinkTextMap env']             ++
    [McpEndpoint n cmd as env' tr' au  | tr'  <- shrink tr]
    where
      shrinkTextMap m =
        map Map.fromList $
        shrinkList
          (\(k, v) -> [(k', v) | k' <- shrinkConfigText k] ++
                      [(k, v') | v' <- shrinkConfigText v])
          (Map.toList m)

instance Arbitrary ApiKey where
  arbitrary = ApiKey <$> arbitraryConfigText
  shrink (ApiKey t) = [ApiKey t' | t' <- shrinkConfigText t]

-- --------------------------------------------------------------------
-- Partial section instances
-- --------------------------------------------------------------------

instance Arbitrary PartialServerSection where
  arbitrary =
    PartialServerSection
      <$> arbitraryLast (choose (1, 65535))
      <*> arbitraryLast arbitraryConfigText
      <*> arbitraryLast (choose (1, 86400))
      <*> arbitraryLast (choose (1, 300))
      <*> arbitraryLast (choose (1, 300))
  shrink (PartialServerSection p h ht gst pi') =
    [PartialServerSection p' h ht gst pi'  | p'   <- shrinkLast shrink p]               ++
    [PartialServerSection p h' ht gst pi'  | h'   <- shrinkLast shrinkConfigText h]     ++
    [PartialServerSection p h ht' gst pi'  | ht'  <- shrinkLast shrink ht]              ++
    [PartialServerSection p h ht gst' pi'  | gst' <- shrinkLast shrink gst]             ++
    [PartialServerSection p h ht gst pi''  | pi'' <- shrinkLast shrink pi']

instance Arbitrary PartialTelemetrySection where
  arbitrary =
    PartialTelemetrySection
      <$> arbitraryLast arbitrary
      <*> arbitraryLast arbitraryConfigText
      <*> arbitraryLast (oneof [pure Nothing, Just <$> arbitraryConfigText])
      <*> arbitraryLast arbitrary
      <*> arbitraryLast arbitrary
      <*> arbitraryLast arbitraryConfigText
      <*> arbitraryLast (oneof [pure Nothing, Just <$> choose (1024, 65535)])
      <*> arbitraryLast (choose (0.0, 1.0))
  shrink (PartialTelemetrySection mo sn ep ls me mbh mp sr) =
    [PartialTelemetrySection mo' sn ep ls me mbh mp sr | mo' <- shrinkLast shrink mo]                  ++
    [PartialTelemetrySection mo sn' ep ls me mbh mp sr | sn' <- shrinkLast shrinkConfigText sn]        ++
    [PartialTelemetrySection mo sn ep' ls me mbh mp sr | ep' <- shrinkLast shrinkMaybeText ep]         ++
    [PartialTelemetrySection mo sn ep ls' me mbh mp sr | ls' <- shrinkLast shrink ls]                  ++
    [PartialTelemetrySection mo sn ep ls me' mbh mp sr | me' <- shrinkLast shrink me]                  ++
    [PartialTelemetrySection mo sn ep ls me mbh' mp sr | mbh' <- shrinkLast shrinkConfigText mbh]      ++
    [PartialTelemetrySection mo sn ep ls me mbh mp' sr | mp' <- shrinkLast shrink mp]                  ++
    [PartialTelemetrySection mo sn ep ls me mbh mp sr' | sr' <- shrinkLast shrink sr]

instance Arbitrary PartialContextSection where
  arbitrary =
    PartialContextSection
      <$> arbitraryLast (choose (1000, 1000000))
      <*> arbitraryLast (choose (0, 8192))
      <*> arbitraryLast (choose (0, 8192))
      <*> arbitraryLast (choose (0, 8192))
      <*> arbitraryLast (choose (0, 8192))
  shrink (PartialContextSection mt rmr mr cr rr) =
    [PartialContextSection mt' rmr mr cr rr  | mt'  <- shrinkLast shrink mt]  ++
    [PartialContextSection mt rmr' mr cr rr  | rmr' <- shrinkLast shrink rmr] ++
    [PartialContextSection mt rmr mr' cr rr  | mr'  <- shrinkLast shrink mr]  ++
    [PartialContextSection mt rmr mr cr' rr  | cr'  <- shrinkLast shrink cr]  ++
    [PartialContextSection mt rmr mr cr rr'  | rr'  <- shrinkLast shrink rr]

instance Arbitrary PartialCompactionSection where
  arbitrary =
    PartialCompactionSection
      <$> arbitraryLast (choose (0.1, 0.99))
      <*> arbitraryLast (choose (1000, 1000000))
  shrink (PartialCompactionSection tf mct) =
    [PartialCompactionSection tf' mct | tf'  <- shrinkLast shrink tf]  ++
    [PartialCompactionSection tf mct' | mct' <- shrinkLast shrink mct]

instance Arbitrary PartialShellSection where
  arbitrary =
    PartialShellSection
      <$> arbitraryLast (choose (1000, 300000))
      <*> arbitraryLast (choose (1, 256))
      <*> arbitraryLast (choose (1000, 1000000))
  shrink (PartialShellSection dt mc ot) =
    [PartialShellSection dt' mc ot | dt' <- shrinkLast shrink dt] ++
    [PartialShellSection dt mc' ot | mc' <- shrinkLast shrink mc] ++
    [PartialShellSection dt mc ot' | ot' <- shrinkLast shrink ot]

instance Arbitrary PartialSessionSection where
  arbitrary =
    PartialSessionSection
      <$> arbitraryLast (choose (1, 100))
      <*> arbitraryLast (choose (60, 86400))
  shrink (PartialSessionSection mpu it) =
    [PartialSessionSection mpu' it  | mpu' <- shrinkLast shrink mpu] ++
    [PartialSessionSection mpu it'  | it'  <- shrinkLast shrink it]

instance Arbitrary PartialGuardrailsSection where
  arbitrary =
    PartialGuardrailsSection
      <$> arbitraryLast (Set.fromList <$> listOf arbitraryConfigText)
      <*> arbitraryLast (Set.fromList <$> listOf arbitraryConfigText)
      <*> arbitraryLast (listOf arbitraryConfigText)
  shrink (PartialGuardrailsSection dl al sp) =
    [PartialGuardrailsSection dl' al sp | dl' <- shrinkLast shrinkTextSet dl] ++
    [PartialGuardrailsSection dl al' sp | al' <- shrinkLast shrinkTextSet al] ++
    [PartialGuardrailsSection dl al sp' | sp' <- shrinkLast (shrinkList shrinkConfigText) sp]
    where
      shrinkTextSet s =
        map Set.fromList $ shrinkList shrinkConfigText (Set.toList s)

instance Arbitrary PartialBudgetSection where
  arbitrary =
    PartialBudgetSection
      <$> arbitraryLast (choose (1, 1000))
      -- TOML integers are signed 64-bit; cap at maxBound::Int to stay in range.
      <*> arbitraryLast (oneof [pure Nothing, Just . fromIntegral <$> (choose (1, maxBound) :: Gen Int)])
      <*> arbitraryLast (oneof [pure Nothing, Just . fromIntegral <$> (choose (1, maxBound) :: Gen Int)])
  shrink (PartialBudgetSection ms stm dtm) =
    [PartialBudgetSection ms' stm dtm | ms'  <- shrinkLast shrink ms]  ++
    [PartialBudgetSection ms stm' dtm | stm' <- shrinkLast shrink stm] ++
    [PartialBudgetSection ms stm dtm' | dtm' <- shrinkLast shrink dtm]

instance Arbitrary PartialRepoMapSection where
  arbitrary =
    PartialRepoMapSection
      <$> arbitraryLast (choose (100, 60000))
      <*> arbitraryLast (choose (512, 32768))
      <*> arbitraryLast (choose (1, 100))
  shrink (PartialRepoMapSection ri rmt bs) =
    [PartialRepoMapSection ri' rmt bs | ri'  <- shrinkLast shrink ri]  ++
    [PartialRepoMapSection ri rmt' bs | rmt' <- shrinkLast shrink rmt] ++
    [PartialRepoMapSection ri rmt bs' | bs'  <- shrinkLast shrink bs]

instance Arbitrary PartialMemorySection where
  arbitrary =
    PartialMemorySection
      <$> arbitraryLast arbitrary
      <*> arbitraryLast (choose (1, 10000))
      <*> arbitraryLast arbitrary
  shrink (PartialMemorySection en me ad) =
    [PartialMemorySection en' me ad | en' <- shrinkLast shrink en] ++
    [PartialMemorySection en me' ad | me' <- shrinkLast shrink me] ++
    [PartialMemorySection en me ad' | ad' <- shrinkLast shrink ad]

instance Arbitrary PartialAuthSection where
  arbitrary =
    PartialAuthSection
      <$> arbitraryLast (elements ["none", "jwt"])
      <*> arbitraryLast arbitraryConfigText
      <*> arbitraryLast arbitraryConfigText
      <*> arbitraryLast arbitraryConfigText
      <*> arbitraryLast (oneof [pure Nothing, Just <$> arbitraryConfigText])
      <*> arbitraryLast (oneof [pure Nothing, Just <$> arbitraryConfigText])
      <*> arbitraryLast arbitraryConfigText
      <*> arbitraryLast arbitraryConfigText
      <*> arbitraryLast arbitraryConfigText
      <*> arbitraryLast (choose (1, 3600))
  shrink (PartialAuthSection mo uri inl sec iss aud cu co cr ttl) =
    [PartialAuthSection mo' uri inl sec iss aud cu co cr ttl | mo'  <- shrinkLast shrinkConfigText mo]  ++
    [PartialAuthSection mo uri' inl sec iss aud cu co cr ttl | uri' <- shrinkLast shrinkConfigText uri] ++
    [PartialAuthSection mo uri inl' sec iss aud cu co cr ttl | inl' <- shrinkLast shrinkConfigText inl] ++
    [PartialAuthSection mo uri inl sec' iss aud cu co cr ttl | sec' <- shrinkLast shrinkConfigText sec] ++
    [PartialAuthSection mo uri inl sec iss' aud cu co cr ttl | iss' <- shrinkLast shrinkMaybeText iss]  ++
    [PartialAuthSection mo uri inl sec iss aud' cu co cr ttl | aud' <- shrinkLast shrinkMaybeText aud]  ++
    [PartialAuthSection mo uri inl sec iss aud cu' co cr ttl | cu'  <- shrinkLast shrinkConfigText cu]  ++
    [PartialAuthSection mo uri inl sec iss aud cu co' cr ttl | co'  <- shrinkLast shrinkConfigText co]  ++
    [PartialAuthSection mo uri inl sec iss aud cu co cr' ttl | cr'  <- shrinkLast shrinkConfigText cr]  ++
    [PartialAuthSection mo uri inl sec iss aud cu co cr ttl' | ttl' <- shrinkLast shrink ttl]

instance Arbitrary PartialConfig where
  arbitrary =
    PartialConfig
      <$> arbitraryLast arbitraryConfigText
      <*> arbitraryLast (Map.fromList <$> mapM (\r -> (r,) <$> arbitrary) [minBound..maxBound])
      <*> arbitraryLast (listOf arbitrary)
      <*> arbitraryLast arbitraryConfigText
      <*> arbitraryLast arbitrary
      <*> arbitraryLast arbitrary
      <*> arbitrary
      <*> arbitraryLast (Text.unpack <$> arbitraryConfigText)
      <*> arbitraryLast arbitrary
      <*> arbitraryLast arbitrary
      <*> arbitraryLast (listOf arbitraryConfigText)
      <*> arbitraryLast (listOf arbitrary)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink (PartialConfig pr mr mdls am pm sm au wp ak is pe me sv tl cx cp sh se gr bu rm mn) =
    [PartialConfig pr' mr mdls am pm sm au wp ak is pe me sv tl cx cp sh se gr bu rm mn | pr'   <- shrinkLast shrinkConfigText pr]                   ++
    [PartialConfig pr mr' mdls am pm sm au wp ak is pe me sv tl cx cp sh se gr bu rm mn | mr'   <- shrinkLast shrinkModelRoleMap mr]                 ++
    [PartialConfig pr mr mdls' am pm sm au wp ak is pe me sv tl cx cp sh se gr bu rm mn | mdls' <- shrinkLast (shrinkList shrink) mdls]              ++
    [PartialConfig pr mr mdls am' pm sm au wp ak is pe me sv tl cx cp sh se gr bu rm mn | am'   <- shrinkLast shrinkConfigText am]                   ++
    [PartialConfig pr mr mdls am pm' sm au wp ak is pe me sv tl cx cp sh se gr bu rm mn | pm'   <- shrinkLast shrink pm]                             ++
    [PartialConfig pr mr mdls am pm sm' au wp ak is pe me sv tl cx cp sh se gr bu rm mn | sm'   <- shrinkLast shrink sm]                             ++
    [PartialConfig pr mr mdls am pm sm au' wp ak is pe me sv tl cx cp sh se gr bu rm mn | au'   <- shrink au]                                        ++
    [PartialConfig pr mr mdls am pm sm au wp' ak is pe me sv tl cx cp sh se gr bu rm mn | wp'   <- shrinkLast shrinkFilePath wp]                     ++
    [PartialConfig pr mr mdls am pm sm au wp ak' is pe me sv tl cx cp sh se gr bu rm mn | ak'   <- shrinkLast shrink ak]                             ++
    [PartialConfig pr mr mdls am pm sm au wp ak is' pe me sv tl cx cp sh se gr bu rm mn | is'   <- shrinkLast shrink is]                             ++
    [PartialConfig pr mr mdls am pm sm au wp ak is pe' me sv tl cx cp sh se gr bu rm mn | pe'   <- shrinkLast (shrinkList shrinkConfigText) pe]      ++
    [PartialConfig pr mr mdls am pm sm au wp ak is pe me' sv tl cx cp sh se gr bu rm mn | me'   <- shrinkLast (shrinkList shrink) me]                ++
    [PartialConfig pr mr mdls am pm sm au wp ak is pe me sv' tl cx cp sh se gr bu rm mn | sv'   <- shrink sv]                                        ++
    [PartialConfig pr mr mdls am pm sm au wp ak is pe me sv tl' cx cp sh se gr bu rm mn | tl'   <- shrink tl]                                        ++
    [PartialConfig pr mr mdls am pm sm au wp ak is pe me sv tl cx' cp sh se gr bu rm mn | cx'   <- shrink cx]                                        ++
    [PartialConfig pr mr mdls am pm sm au wp ak is pe me sv tl cx cp' sh se gr bu rm mn | cp'   <- shrink cp]                                        ++
    [PartialConfig pr mr mdls am pm sm au wp ak is pe me sv tl cx cp sh' se gr bu rm mn | sh'   <- shrink sh]                                        ++
    [PartialConfig pr mr mdls am pm sm au wp ak is pe me sv tl cx cp sh se' gr bu rm mn | se'   <- shrink se]                                        ++
    [PartialConfig pr mr mdls am pm sm au wp ak is pe me sv tl cx cp sh se gr' bu rm mn | gr'   <- shrink gr]                                        ++
    [PartialConfig pr mr mdls am pm sm au wp ak is pe me sv tl cx cp sh se gr bu' rm mn | bu'   <- shrink bu]                                        ++
    [PartialConfig pr mr mdls am pm sm au wp ak is pe me sv tl cx cp sh se gr bu rm' mn | rm'   <- shrink rm]                                        ++
    [PartialConfig pr mr mdls am pm sm au wp ak is pe me sv tl cx cp sh se gr bu rm mn' | mn'   <- shrink mn]
    where
      shrinkModelRoleMap m =
        [Map.insert r ms' m | (r, ms) <- Map.toList m, ms' <- shrink ms]
      shrinkFilePath fp =
        filter (not . null) $ shrink fp
