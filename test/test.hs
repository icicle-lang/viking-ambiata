import           Disorder.Core.Main

import qualified Test.Viking.ByteStream
import qualified Test.Viking.Char8Stream


main :: IO ()
main =
  disorderMain [
      Test.Viking.ByteStream.tests
    , Test.Viking.Char8Stream.tests
    ]
