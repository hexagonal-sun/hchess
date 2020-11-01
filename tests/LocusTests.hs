
import Locus
import PrettyPrint
import Test.Tasty
import Test.Tasty.HUnit

locTranslateTest :: (Locus -> a) -> (a -> Locus) -> IO ()
locTranslateTest f y = do
  let locs = map (\loc -> (loc, f loc)) allLocaii
  let asserts = map (\(loc, x) -> assertEqual (pp loc) loc $ y x) locs
  sequence_ asserts

pseudoLocTranslation :: TestTree
pseudoLocTranslation = testCase "Locus -> PseudoLocus -> Locus" $ locTranslateTest locToPl plToLoc

frTranslation :: TestTree
frTranslation = testCase "Locus -> (File, Rank) -> Locus" $ locTranslateTest locToFR frToLoc

pieceTests :: TestTree
pieceTests = testGroup "Locus Conversion:" [ pseudoLocTranslation
                                           , frTranslation ]
 
main :: IO ()
main = defaultMain pieceTests
