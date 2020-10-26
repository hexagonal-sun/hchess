
import Piece
import Test.Tasty
import Test.Tasty.HUnit

pieceEnumTest :: Piece -> TestTree
pieceEnumTest p = testCase ("Piece: " ++ show p) $ do
  let ev = fromEnum p
      resultingPiece = toEnum ev
  assertEqual (show resultingPiece) p resultingPiece

pieceTests :: TestTree
pieceTests = testGroup "Piece Enum conversion" $ map pieceEnumTest [Piece c k | k <- allKinds, c <- [White, Black]]
 
main :: IO ()
main = defaultMain pieceTests
