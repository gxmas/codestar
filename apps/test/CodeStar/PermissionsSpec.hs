module CodeStar.PermissionsSpec (spec) where

import Control.Exception (IOException, catch)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import CodeStar.Permissions

spec :: Spec
spec = describe "CodeStar.Permissions" $ do
  describe "basic operations" $ do
    it "grant then check returns True" $
      withPermissionDirs $ \(workspace, globalDir) -> do
        store <- newPermissionStore workspace globalDir
        grant store "read" Session
        check store "read" `shouldReturn` True

    it "revoke then check returns False" $
      withPermissionDirs $ \(workspace, globalDir) -> do
        store <- newPermissionStore workspace globalDir
        grant store "write" Session
        revoke store "write" Session
        check store "write" `shouldReturn` False

  describe "scope persistence" $ do
    it "session permissions do not persist across store recreation" $
      withPermissionDirs $ \(workspace, globalDir) -> do
        store1 <- newPermissionStore workspace globalDir
        grant store1 "shell" Session
        check store1 "shell" `shouldReturn` True

        store2 <- newPermissionStore workspace globalDir
        check store2 "shell" `shouldReturn` False

    it "project permissions persist across store recreation" $
      withPermissionDirs $ \(workspace, globalDir) -> do
        store1 <- newPermissionStore workspace globalDir
        grant store1 "edit" Project
        check store1 "edit" `shouldReturn` True

        store2 <- newPermissionStore workspace globalDir
        check store2 "edit" `shouldReturn` True

    it "global permissions persist across store recreation" $
      withPermissionDirs $ \(workspace, globalDir) -> do
        store1 <- newPermissionStore workspace globalDir
        grant store1 "grep" Global
        check store1 "grep" `shouldReturn` True

        store2 <- newPermissionStore workspace globalDir
        check store2 "grep" `shouldReturn` True

    it "revoke overrides prior grant in the same scope" $
      withPermissionDirs $ \(workspace, globalDir) -> do
        store <- newPermissionStore workspace globalDir
        grant store "read" Project
        check store "read" `shouldReturn` True
        revoke store "read" Project
        check store "read" `shouldReturn` False

  describe "model-based command testing" $ do
    prop "matches a (Scope, Permission) oracle for random sequences" $
      forAll genCommands $ \cmds ->
        checkCoverage $
          cover 20 (length cmds >= 10) "long sequence (10+)" $
          cover 20 (length cmds < 5) "short sequence (<5)" $
          cover 40 (any isGrant cmds) "contains grants" $
          cover 40 (any isRevoke cmds) "contains revokes" $
          ioProperty $ withPermissionDirs $ \(workspace, globalDir) -> do
            store <- newPermissionStore workspace globalDir
            let expected = foldl applyModel Map.empty cmds
            mapM_ (applyStore store) cmds
            checks <-
              mapM
                ( \perm ->
                    (== expectedCheck expected perm) <$> check store perm
                )
                allPermissions
            pure (and checks)

isGrant :: PermCmd -> Bool
isGrant (GrantCmd _ _) = True
isGrant _ = False

isRevoke :: PermCmd -> Bool
isRevoke (RevokeCmd _ _) = True
isRevoke _ = False

data PermCmd
  = GrantCmd PermissionScope Text
  | RevokeCmd PermissionScope Text
  deriving stock (Eq, Show)

genCommands :: Gen [PermCmd]
genCommands = resize 20 (listOf genCommand)

genCommand :: Gen PermCmd
genCommand =
  oneof
    [ GrantCmd <$> genScope <*> genPermission
    , RevokeCmd <$> genScope <*> genPermission
    ]

genScope :: Gen PermissionScope
genScope = elements [Session, Project, Global]

genPermission :: Gen Text
genPermission = elements allPermissions

allPermissions :: [Text]
allPermissions = ["read", "write", "edit", "grep", "shell"]

applyStore :: PermissionStore -> PermCmd -> IO ()
applyStore store cmd = case cmd of
  GrantCmd scope perm -> grant store perm scope
  RevokeCmd scope perm -> revoke store perm scope

applyModel :: Map.Map (PermissionScope, Text) Bool -> PermCmd -> Map.Map (PermissionScope, Text) Bool
applyModel model cmd = case cmd of
  GrantCmd scope perm -> Map.insert (scope, perm) True model
  RevokeCmd scope perm -> Map.insert (scope, perm) False model

expectedCheck :: Map.Map (PermissionScope, Text) Bool -> Text -> Bool
expectedCheck model perm =
  or
    [ Map.findWithDefault False (scope, perm) model
    | scope <- [Session, Project, Global]
    ]

withPermissionDirs :: ((FilePath, FilePath) -> IO a) -> IO a
withPermissionDirs action = do
  tmp <- getTemporaryDirectory
  let root = tmp </> "codestar-permissions-spec"
      workspace = root </> "workspace"
      globalDir = root </> "global"

  ignoreIO (removePathForcibly root)
  createDirectoryIfMissing True workspace
  createDirectoryIfMissing True globalDir
  result <- action (workspace, globalDir)
  ignoreIO (removePathForcibly root)
  pure result

ignoreIO :: IO () -> IO ()
ignoreIO io = io `catch` handler
 where
  handler :: IOException -> IO ()
  handler _ = pure ()
