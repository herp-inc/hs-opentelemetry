import Data.List (intercalate, isPrefixOf)
import Data.Maybe (mapMaybe)
import Distribution.Simple (Args, defaultMainWithHooks, simpleUserHooks)
import qualified Distribution.Simple
import Distribution.Simple.Setup (BuildFlags)
import Distribution.Types.HookedBuildInfo (HookedBuildInfo, emptyHookedBuildInfo)
import GHC.Stack (HasCallStack)
import System.Directory (copyFile, createDirectoryIfMissing, getTemporaryDirectory, removeFile)
import System.FilePath (takeDirectory)
import System.IO (IOMode (ReadMode), hClose, hGetContents, hPutStr, hSetNewlineMode, noNewlineTranslation, openTempFile, withFile)


main :: IO ()
main = defaultMainWithHooks simpleUserHooks {Distribution.Simple.preBuild = preBuild}


{- | Create a wrapper module.

@
append :: (RedisCtx m f, Otel.MonadTracer m, MonadUnliftIO m, HasCallStack) => ByteString -> ByteString -> m (f Integer)
append = wrap2 "append" Orig.append
@
-}
preBuild :: HasCallStack => Args -> BuildFlags -> IO HookedBuildInfo
preBuild _ _ = do
  let
    sourceTextPath = "functions.txt"
    destinationCodePath = "gen/OpenTelemetry/Instrumentation/Hedis/Internal/Action.hs"
    oldConstraintText = "RedisCtx m f"
    newConstraintText = "(RedisCtx m f, Otel.MonadTracer m, MonadUnliftIO m, HasCallStack)"
  temporaryFilePath <-
    withFile sourceTextPath ReadMode $ \sourceTextHandle -> do
      temporaryDirectoryPath <- getTemporaryDirectory
      createDirectoryIfMissing True temporaryDirectoryPath
      (temporaryFilePath, temporaryHandle) <- openTempFile temporaryDirectoryPath "generated.hs"
      putStrLn $ "temporary file: " ++ temporaryFilePath
      hSetNewlineMode sourceTextHandle noNewlineTranslation
      hSetNewlineMode temporaryHandle noNewlineTranslation
      lines <- Prelude.lines <$> hGetContents sourceTextHandle
      let
        actionInfos =
          flip mapMaybe lines $ \line ->
            if "--" `isPrefixOf` line
              then Nothing
              else
                let
                  name = takeWhile (/= ' ') line
                  paramCount = length $ filter (== ('-', '>')) $ zip (init line) $ tail line
                  typ = replace oldConstraintText newConstraintText line
                  term = name ++ " = withFrozenCallStack $ wrap" ++ show paramCount ++ " \"" ++ name ++ "\" " ++ "Orig." ++ name
                in
                  if name == "command"
                    then Nothing -- becuase `command` has an unexposed type
                    else Just (name, typ, term)
        names = (\(name, _, _) -> name) <$> actionInfos
        declerations = (\(_, typ, term) -> typ ++ "\n" ++ term) <$> actionInfos
      hPutStr temporaryHandle $
        unlines $
          [ "{-# LANGUAGE ConstraintKinds #-}"
          , "{-# LANGUAGE OverloadedStrings #-}"
          , "{-# OPTIONS_GHC -Wno-missing-export-lists #-}"
          , "{-# OPTIONS_GHC -Wno-missing-import-lists #-}"
          , ""
          , "module OpenTelemetry.Instrumentation.Hedis.Internal.Action where"
          , ""
          , "import OpenTelemetry.Instrumentation.Hedis.Internal.Wrapper"
          , "import qualified Database.Redis as Orig"
          , "import Database.Redis hiding (" ++ intercalate ", " names ++ ")"
          , ""
          , "import Control.Monad.IO.Unlift (MonadUnliftIO)"
          , "import Data.ByteString (ByteString)"
          , "import GHC.Stack (HasCallStack, withFrozenCallStack)"
          , "import qualified OpenTelemetry.Trace.Monad as Otel (MonadTracer)"
          , ""
          ]
            ++ declerations
      hClose temporaryHandle
      pure temporaryFilePath
  createDirectoryIfMissing True $ takeDirectory destinationCodePath
  copyFile temporaryFilePath destinationCodePath
  removeFile temporaryFilePath
  pure emptyHookedBuildInfo


replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace old new =
  intercalate new . split old
  where
    split _ [] = []
    split sep xs =
      let (p, xs') = split1 [] xs
      in p : split sep xs'
      where
        split1 acc [] = (reverse acc, [])
        split1 acc xs@(y : ys) =
          let (a, as) = splitAt (length sep) xs
          in if a == sep
              then (reverse acc, as)
              else split1 (y : acc) ys
