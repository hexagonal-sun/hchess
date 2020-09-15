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
update Pawn (Move from to _) | fr == R7 && tr == R5 = EnPassant $ Just (ff,Black)
                             | fr == R2 && tr == R4 = EnPassant $ Just (ff,White)
  where fr = locToRank from
        tr = locToRank to
        ff = locToFile from

update _ _ = EnPassant Nothing

isEPLocus :: EnPassant -> Locus -> Bool
isEPLocus (EnPassant Nothing) _ = False
isEPLocus (EnPassant (Just (file,c))) l = locToFR l == l'
  where rank = if c == White then R3 else R6
        l'   = (file,rank)

-- If a piece attacks `to`, given EnPassant, return the locus of the maybe
-- captured Pawn.
captureLoc :: EnPassant -> Locus -> Maybe Locus
captureLoc (EnPassant Nothing) _ = Nothing
captureLoc ep@(EnPassant (Just (file,c))) to = if isEPLocus ep to then Just (frToLoc (file,rank)) else Nothing
  where rank = if c == White then R4 else R5
