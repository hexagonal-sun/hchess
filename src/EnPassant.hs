module EnPassant
  (EnPassant(..),
   defaultState,
   isEPLocus,
   update,
   captureLoc) where

import Locus
import Piece
import Move

newtype EnPassant = EnPassant (Maybe (File,Colour))

defaultState :: EnPassant
defaultState = EnPassant Nothing

-- Given a move, update the EP state.
update :: PieceKind -> Move -> EnPassant
update Pawn m | fr == R7 && tr == R5 = EnPassant $ Just (ff,Black)
              | fr == R2 && tr == R4 = EnPassant $ Just (ff,White)
  where fr = locToRank $ from m
        tr = locToRank $ to m
        ff = locToFile $ from m

update _ _ = EnPassant Nothing

isEPLocus :: EnPassant -> Locus -> Bool
isEPLocus (EnPassant Nothing) _ = False
isEPLocus (EnPassant (Just (file,c))) l = locToFR l == l'
  where rank = if c == White then R3 else R6
        l'   = (file,rank)

-- If a piece attacks `attack`, given EnPassant, return the locus of the maybe
-- captured Pawn.
captureLoc :: EnPassant -> Locus -> Maybe Locus
captureLoc (EnPassant Nothing) _ = Nothing
captureLoc ep@(EnPassant (Just (file,c))) attack = if isEPLocus ep attack then Just (frToLoc (file,rank)) else Nothing
  where rank = if c == White then R4 else R5
