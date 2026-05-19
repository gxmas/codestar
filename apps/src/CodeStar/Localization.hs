{- |
= CodeStar.Localization — hierarchical fault localisation

Given a bug description and a repository overview, this module narrows the
problem down from the whole codebase to specific patch candidates through
a sequence of LLM calls.

== Pipeline

@
  ObjectiveSpec + repoCtx
       │
       ▼  Step 1: File localisation
  [LocalizedFile]           — which files likely contain the bug?
       │
       ▼  Step 2: Function localisation
  [LocalizedFunction]       — which functions in those files?
       │
       ▼  Step 4: Patch generation
  [PatchCandidate]          — concrete old→new code replacements
       │
       ▼
  LocalizationResult
@

Each step is a __single non-streaming LLM call__ ('singleCall') with
temperature 0 for determinism.  The output is parsed as structured text
rather than JSON to keep the prompts simple and model-agnostic.

This module is used for the automated debugging / issue-resolution workflow
and is separate from the main agent loop, which handles interactive tasks.
-}
module CodeStar.Localization
  ( -- * Results
    LocalizationResult (..)
  , LocalizedFile (..)
  , LocalizedFunction (..)
  , PatchCandidate (..)

    -- * Pipeline
  , localize
  , LocalizationConfig (..)
  , defaultLocalizationConfig
  ) where

import Data.Text (Text)
import Data.Text qualified as Text

import CodeStar.LLM.Base
  ( CompletionRequest (..)
  , CompletionResponse (..)
  , Content (..)
  , LlmClientDict (..)
  , Message (..)
  , Role (..)
  )
import CodeStar.Types (ObjectiveSpec (..))

-- --------------------------------------------------------------------
-- Result types
-- --------------------------------------------------------------------

data LocalizedFile = LocalizedFile
  { lfPath :: !FilePath
  , lfReason :: !Text
  }
  deriving stock (Eq, Show)

data LocalizedFunction = LocalizedFunction
  { lnFile :: !FilePath
  , lnName :: !Text
  , lnStartLine :: !Int
  , lnEndLine :: !Int
  , lnReason :: !Text
  }
  deriving stock (Eq, Show)

data PatchCandidate = PatchCandidate
  { pcFile :: !FilePath
  , pcOldCode :: !Text
  , pcNewCode :: !Text
  , pcReason :: !Text
  }
  deriving stock (Eq, Show)

-- | The complete result of the localisation pipeline.
data LocalizationResult = LocalizationResult
  { lrFiles     :: ![LocalizedFile]     -- ^ Files ranked by suspicion.
  , lrFunctions :: ![LocalizedFunction] -- ^ Suspicious functions within those files.
  , lrPatches   :: ![PatchCandidate]    -- ^ Candidate fixes ready for application.
  }
  deriving stock (Eq, Show)

-- --------------------------------------------------------------------
-- Config
-- --------------------------------------------------------------------

data LocalizationConfig = LocalizationConfig
  { maxFiles :: !Int
  -- ^ max files to localise to
  , maxFunctions :: !Int
  -- ^ max functions per file
  , maxPatches :: !Int
  -- ^ max patch candidates to generate
  }
  deriving stock (Eq, Show)

defaultLocalizationConfig :: LocalizationConfig
defaultLocalizationConfig =
  LocalizationConfig
    { maxFiles = 5
    , maxFunctions = 3
    , maxPatches = 3
    }

-- --------------------------------------------------------------------
-- Pipeline
-- --------------------------------------------------------------------

{- | Four-step hierarchical fault localisation:
  1. File localisation: which files are relevant?
  2. Function localisation: which functions within those files?
  3. Line localisation: narrow to the specific code region.
  4. Patch generation: produce N candidate fixes.
No agent loop — each step is a single LLM call.
-}
localize ::
  -- | Architect/Validator model for analysis calls
  LlmClientDict ->
  LocalizationConfig ->
  ObjectiveSpec ->
  -- | repository context (repo map + relevant file contents)
  Text ->
  IO (Either Text LocalizationResult)
localize client cfg spec repoCtx = do
  fileResult <- localizeFiles client cfg spec repoCtx
  case fileResult of
    Left err -> pure (Left err)
    Right files -> do
      fnResult <- localizeFunctions client cfg spec repoCtx files
      case fnResult of
        Left err -> pure (Left err)
        Right fns -> do
          patches <- generatePatches client cfg spec fns
          case patches of
            Left err -> pure (Left err)
            Right ps ->
              pure $
                Right
                  LocalizationResult
                    { lrFiles = files
                    , lrFunctions = fns
                    , lrPatches = ps
                    }

-- --------------------------------------------------------------------
-- Step 1: File localisation
-- --------------------------------------------------------------------

localizeFiles ::
  LlmClientDict ->
  LocalizationConfig ->
  ObjectiveSpec ->
  Text ->
  IO (Either Text [LocalizedFile])
localizeFiles client cfg spec repoCtx = do
  let prompt =
        Text.unlines
          [ "Given this bug report and codebase overview, identify the "
              <> Text.pack (show cfg.maxFiles)
              <> " most likely files to contain the bug."
          , ""
          , "Bug description: " <> spec.description
          , ""
          , "Repository overview:"
          , repoCtx
          , ""
          , "For each file, respond with one line: <filepath>: <reason>"
          , "List at most " <> Text.pack (show cfg.maxFiles) <> " files."
          ]
  result <- singleCall client prompt
  case result of
    Left err -> pure (Left err)
    Right txt -> pure (Right (parseFileLines txt))

parseFileLines :: Text -> [LocalizedFile]
parseFileLines txt =
  [ LocalizedFile (Text.unpack fp) reason
  | line <- Text.lines txt
  , let parts = Text.splitOn ": " line
  , (fp, reason) <- case parts of
      (first : rest@(_ : _)) ->
        let p = Text.strip first
            r = Text.strip (Text.intercalate ": " rest)
         in [(p, r)]
      _ -> []
  , not (Text.null fp)
  ]

-- --------------------------------------------------------------------
-- Step 2: Function localisation
-- --------------------------------------------------------------------

localizeFunctions ::
  LlmClientDict ->
  LocalizationConfig ->
  ObjectiveSpec ->
  Text ->
  [LocalizedFile] ->
  IO (Either Text [LocalizedFunction])
localizeFunctions client cfg spec repoCtx files = do
  let fileList = Text.intercalate ", " (map (Text.pack . (.lfPath)) files)
      prompt =
        Text.unlines
          [ "Given the bug description and these relevant files, identify the "
              <> Text.pack (show cfg.maxFunctions)
              <> " most suspicious functions."
          , ""
          , "Bug: " <> spec.description
          , "Files to examine: " <> fileList
          , ""
          , "Context:"
          , repoCtx
          , ""
          , "For each function respond with one line:"
          , "<filepath>:<function_name>:<start_line>-<end_line>: <reason>"
          ]
  result <- singleCall client prompt
  case result of
    Left err -> pure (Left err)
    Right txt -> pure (Right (parseFunctionLines txt))

parseFunctionLines :: Text -> [LocalizedFunction]
parseFunctionLines txt =
  [ LocalizedFunction (Text.unpack fp) name startLine endLine reason
  | line <- Text.lines txt
  , let parts = Text.splitOn ":" line
  , length parts >= 4
  , let fp = Text.strip (parts !! 0)
  , let name = Text.strip (parts !! 1)
  , let range = Text.strip (parts !! 2)
  , let reason = Text.strip (Text.intercalate ":" (drop 3 parts))
  , Just (startLine, endLine) <- [parseRange range]
  , not (Text.null fp)
  , not (Text.null name)
  ]
 where
  parseRange t =
    case Text.splitOn "-" t of
      [s, e] ->
        let startLine = readInt s
            endLine = readInt e
         in if startLine > 0 && endLine >= startLine
              then Just (startLine, endLine)
              else Nothing
      _ -> Nothing
  readInt t = case reads (Text.unpack (Text.strip t)) of
    [(n, "")] -> n
    _ -> 0

-- --------------------------------------------------------------------
-- Step 4: Patch generation
-- --------------------------------------------------------------------

generatePatches ::
  LlmClientDict ->
  LocalizationConfig ->
  ObjectiveSpec ->
  [LocalizedFunction] ->
  IO (Either Text [PatchCandidate])
generatePatches client cfg spec fns = do
  let fnList = Text.intercalate "\n" (map formatFn fns)
      prompt =
        Text.unlines
          [ "Generate " <> Text.pack (show cfg.maxPatches) <> " candidate patches for this bug."
          , ""
          , "Bug: " <> spec.description
          , ""
          , "Suspicious locations:"
          , fnList
          , ""
          , "For each candidate patch respond with:"
          , "FILE: <filepath>"
          , "OLD:"
          , "<original code>"
          , "NEW:"
          , "<fixed code>"
          , "REASON: <explanation>"
          , "---"
          ]
  result <- singleCall client prompt
  case result of
    Left err -> pure (Left err)
    Right txt -> pure (Right (parsePatches txt))

formatFn :: LocalizedFunction -> Text
formatFn f =
  Text.pack f.lnFile
    <> " "
    <> f.lnName
    <> " (lines "
    <> Text.pack (show f.lnStartLine)
    <> "-"
    <> Text.pack (show f.lnEndLine)
    <> "): "
    <> f.lnReason

parsePatches :: Text -> [PatchCandidate]
parsePatches txt = go (Text.lines txt) []
 where
  go [] acc = reverse acc
  go ls acc =
    case break (Text.isPrefixOf "FILE:") ls of
      (_, []) -> reverse acc
      (_, fl : rest) ->
        let fp = Text.strip (Text.drop 5 fl)
            (old, rest2) = extractBlock "OLD:" "NEW:" rest
            (new, rest3) = extractBlock "NEW:" "REASON:" rest2
            reason = case dropWhile (not . Text.isPrefixOf "REASON:") rest3 of
              (r : _) -> Text.strip (Text.drop 7 r)
              [] -> Text.empty
            rest4 = dropWhile (not . Text.isPrefixOf "---") rest3
            rest5 = case rest4 of (_ : xs) -> xs; [] -> []
            acc' =
              if Text.null fp
                then acc
                else PatchCandidate (Text.unpack fp) old new reason : acc
         in go rest5 acc'

  extractBlock start end ls =
    let body =
          takeWhile
            (not . Text.isPrefixOf end)
            (dropWhile (not . Text.isPrefixOf start) ls)
        content = drop 1 body -- skip the "START:" line
     in (Text.unlines content, drop (length body) ls)

-- --------------------------------------------------------------------
-- Helper
-- --------------------------------------------------------------------

singleCall :: LlmClientDict -> Text -> IO (Either Text Text)
singleCall client prompt = do
  let req =
        CompletionRequest
          { messages = [Message User [TextContent prompt]]
          , systemPrompt = Nothing
          , tools = []
          , maxTokens = 4096
          , temperature = Just 0.0
          , topP = Nothing
          }
  result <- client.complete req
  case result of
    Left err -> pure (Left (Text.pack (show err)))
    Right resp -> pure (Right (extractText resp))

extractText :: CompletionResponse -> Text
extractText resp = foldMap go resp.responseContent
 where
  go (TextContent t) = t
  go _ = Text.empty
