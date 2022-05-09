import Control.Exception (evaluate)
import Control.Monad (unless)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified RequestSpec
import System.Exit (exitFailure)

main :: IO ()
main = sequence [RequestSpec.tests] >>= (\res -> unless (and res) exitFailure)
