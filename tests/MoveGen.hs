
import Perft
import Fen
import Test.Tasty
import Test.Tasty.HUnit

data PerftTest = PerftTest String [Int]

perftTest :: PerftTest -> TestTree
perftTest (PerftTest fen perfts) = testCase ("Perft: " ++ fen) $ do
  let fp = parseFen fen
  case fp of
    Left e -> assertFailure $ show e
    Right game -> mapM_ (\(i, p) -> assertEqual ("perft " ++ show i) p $ perftInt i game) $
      zip [1..] perfts

perftTests :: TestTree
perftTests = testGroup "Perft Tests" $ map perftTest [
    PerftTest "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq" [20, 400, 8902, 197281, 4865609]
  , PerftTest "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq" [48, 2039, 97862, 4085603]
  , PerftTest "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w -" [14, 191, 2812, 43238, 674624]
  , PerftTest "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq" [6, 264, 9467, 422333]
  , PerftTest "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ" [44, 1486, 62379, 2103487]
  , PerftTest "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w -" [46, 2079, 89890, 3894594]
  ]
 
main :: IO ()
main = defaultMain perftTests
