import Data.Char (isDigit)
import Data.Foldable (for_)
import Data.List (intercalate, intersperse, isPrefixOf, replicate, stripPrefix)
import Distribution.Simple (Args, UserHooks (preBuild), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.Setup (BuildFlags)
import Distribution.Types.HookedBuildInfo (HookedBuildInfo, emptyHookedBuildInfo)
import Proto3.Suite.DotProto.Generate (CompileArgs (..), compileDotProtoFileOrDie)
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


main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { preBuild = compileProto
      }


compileProto :: Args -> BuildFlags -> IO HookedBuildInfo
compileProto _ _ = do
  let
    compileArgs =
      CompileArgs
        { includeDir = []
        , extraInstanceFiles = []
        , inputProto = "echo.proto"
        , outputDir = "gen"
        }
  compileDotProtoFileOrDie compileArgs
  pure emptyHookedBuildInfo
