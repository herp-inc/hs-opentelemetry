{-# LANGUAGE CPP #-}

import Data.Char (isDigit)
import Data.Foldable (for_)
import Data.List (intercalate, intersperse, isPrefixOf, replicate, stripPrefix)
import Distribution.Simple (Args, UserHooks (preBuild), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.Setup (BuildFlags)
import Distribution.Types.HookedBuildInfo (HookedBuildInfo, emptyHookedBuildInfo)
import Proto3.Suite.DotProto.Generate (CompileArgs (..), RecordStyle (RegularRecords), StringType (StringType), compileDotProtoFileOrDie)
import System.IO (
  Handle,
  IOMode (ReadMode),
  hClose,
  hGetLine,
  hIsEOF,
  hPutStrLn,
  hSetNewlineMode,
  noNewlineTranslation,
  openTempFile,
  stdin,
  withFile,
 )


#if PROTO3_SUITE_NO_PREFIX
import Proto3.Suite.DotProto.Generate (IsPrefixed (IsPrefixed))
#endif


main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { preBuild = compileProto
      }

#if !PROTO3_SUITE_NO_PREFIX
compileProto :: Args -> BuildFlags -> IO HookedBuildInfo
compileProto _ _ = do
  let
    compileArgs =
      CompileArgs
        { includeDir = []
        , extraInstanceFiles = []
        , inputProto = "echo.proto"
        , outputDir = "gen"
        , stringType = StringType "Data.Text.Lazy" "Text"
        , recordStyle = RegularRecords
        }
  compileDotProtoFileOrDie compileArgs
  pure emptyHookedBuildInfo
#else
compileProto :: Args -> BuildFlags -> IO HookedBuildInfo
compileProto _ _ = do
  let
    compileArgs =
      CompileArgs
        { includeDir = []
        , extraInstanceFiles = []
        , inputProto = "echo.proto"
        , outputDir = "gen"
        , stringType = StringType "Data.Text.Lazy" "Text"
        , recordStyle = RegularRecords
        , isPrefixed = IsPrefixed True
        }
  compileDotProtoFileOrDie compileArgs
  pure emptyHookedBuildInfo
#endif
