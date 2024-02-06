module Board
    ( 
        Board(..),
        Field(..),
        startBoard,
        getField,
        setField
    ) where

data Board = Board
    {
        a8 :: Field, b8 :: Field, c8 :: Field, d8 :: Field, e8 :: Field, f8 :: Field, g8 :: Field, h8 :: Field,
        a7 :: Field, b7 :: Field, c7 :: Field, d7 :: Field, e7 :: Field, f7 :: Field, g7 :: Field, h7 :: Field,
        a6 :: Field, b6 :: Field, c6 :: Field, d6 :: Field, e6 :: Field, f6 :: Field, g6 :: Field, h6 :: Field,
        a5 :: Field, b5 :: Field, c5 :: Field, d5 :: Field, e5 :: Field, f5 :: Field, g5 :: Field, h5 :: Field,
        a4 :: Field, b4 :: Field, c4 :: Field, d4 :: Field, e4 :: Field, f4 :: Field, g4 :: Field, h4 :: Field,
        a3 :: Field, b3 :: Field, c3 :: Field, d3 :: Field, e3 :: Field, f3 :: Field, g3 :: Field, h3 :: Field,
        a2 :: Field, b2 :: Field, c2 :: Field, d2 :: Field, e2 :: Field, f2 :: Field, g2 :: Field, h2 :: Field,
        a1 :: Field, b1 :: Field, c1 :: Field, d1 :: Field, e1 :: Field, f1 :: Field, g1 :: Field, h1 :: Field
    }

instance Show Board where
    show (Board h1 h2 h3 h4 h5 h6 h7 h8
                g1 g2 g3 g4 g5 g6 g7 g8
                f1 f2 f3 f4 f5 f6 f7 f8
                e1 e2 e3 e4 e5 e6 e7 e8
                d1 d2 d3 d4 d5 d6 d7 d8
                c1 c2 c3 c4 c5 c6 c7 c8
                b1 b2 b3 b4 b5 b6 b7 b8
                a1 a2 a3 a4 a5 a6 a7 a8 ) =
        "\n+-----------------------------------+\n" ++
        "| 8 | " ++ show h1 ++ " | " ++ show h2 ++ " | " ++ show h3 ++ " | " ++ show h4 ++ " | " ++ show h5 ++ " | " ++ show h6 ++ " | " ++ show h7 ++ " | " ++ show h8 ++ " |\n" ++
        "|---+---+---+---+---+---+---+---+---|\n" ++
        "| 7 | " ++ show g1 ++ " | " ++ show g2 ++ " | " ++ show g3 ++ " | " ++ show g4 ++ " | " ++ show g5 ++ " | " ++ show g6 ++ " | " ++ show g7 ++ " | " ++ show g8 ++ " |\n" ++
        "|---+---+---+---+---+---+---+---+---|\n" ++
        "| 6 | " ++ show f1 ++ " | " ++ show f2 ++ " | " ++ show f3 ++ " | " ++ show f4 ++ " | " ++ show f5 ++ " | " ++ show f6 ++  " | " ++show f7 ++  " | " ++show f8 ++ " |\n" ++
        "|---+---+---+---+---+---+---+---+---|\n" ++
        "| 5 | " ++ show e1 ++ " | " ++ show e2 ++ " | " ++ show e3 ++ " | " ++ show e4 ++ " | " ++ show e5 ++ " | " ++ show e6 ++ " | " ++ show e7 ++ " | " ++ show e8 ++ " |\n" ++
        "|---+---+---+---+---+---+---+---+---|\n" ++
        "| 4 | " ++ show d1 ++ " | " ++ show d2 ++ " | " ++ show d3 ++ " | " ++ show d4 ++ " | " ++ show d5 ++ " | " ++ show d6 ++ " | " ++ show d7 ++  " | " ++show d8 ++ " |\n" ++
        "|---+---+---+---+---+---+---+---+---|\n" ++
        "| 3 | " ++ show c1 ++ " | " ++ show c2 ++ " | " ++ show c3 ++ " | " ++ show c4 ++ " | " ++ show c5 ++ " | " ++ show c6 ++ " | " ++ show c7 ++ " | " ++ show c8 ++ " |\n" ++
        "|---+---+---+---+---+---+---+---+---|\n" ++
        "| 2 | " ++ show b1 ++ " | " ++ show b2 ++ " | " ++ show b3 ++ " | " ++ show b4 ++ " | " ++ show b5 ++ " | " ++ show b6 ++  " | " ++ show b7 ++ " | " ++ show b8 ++ " |\n" ++
        "|---+---+---+---+---+---+---+---+---|\n" ++
        "| 1 | " ++ show a1 ++ " | " ++ show a2 ++ " | " ++ show a3 ++ " | " ++ show a4 ++ " | " ++ show a5 ++ " | " ++ show a6 ++ " | " ++ show a7 ++ " | " ++ show a8 ++ " |\n" ++
        "|---+---+---+---+---+---+---+---+---|\n" ++
        "|   | A | B | C | D | E | F | G | H |\n" ++
        "+-----------------------------------+\n"

getField :: Board -> String -> Field
getField board "A1" = a1 board
getField board "A2" = a2 board
getField board "A3" = a3 board
getField board "A4" = a4 board
getField board "A5" = a5 board
getField board "A6" = a6 board
getField board "A7" = a7 board
getField board "A8" = a8 board
getField board "B1" = b1 board
getField board "B2" = b2 board
getField board "B3" = b3 board
getField board "B4" = b4 board
getField board "B5" = b5 board
getField board "B6" = b6 board
getField board "B7" = b7 board
getField board "B8" = b8 board
getField board "C1" = c1 board
getField board "C2" = c2 board
getField board "C3" = c3 board
getField board "C4" = c4 board
getField board "C5" = c5 board
getField board "C6" = c6 board
getField board "C7" = c7 board
getField board "C8" = c8 board
getField board "D1" = d1 board
getField board "D2" = d2 board
getField board "D3" = d3 board
getField board "D4" = d4 board
getField board "D5" = d5 board
getField board "D6" = d6 board
getField board "D7" = d7 board
getField board "D8" = d8 board
getField board "E1" = e1 board
getField board "E2" = e2 board
getField board "E3" = e3 board
getField board "E4" = e4 board
getField board "E5" = e5 board
getField board "E6" = e6 board
getField board "E7" = e7 board
getField board "E8" = e8 board
getField board "F1" = f1 board
getField board "F2" = f2 board
getField board "F3" = f3 board
getField board "F4" = f4 board
getField board "F5" = f5 board
getField board "F6" = f6 board
getField board "F7" = f7 board
getField board "F8" = f8 board
getField board "G1" = g1 board
getField board "G2" = g2 board
getField board "G3" = g3 board
getField board "G4" = g4 board
getField board "G5" = g5 board
getField board "G6" = g6 board
getField board "G7" = g7 board
getField board "G8" = g8 board
getField board "H1" = h1 board
getField board "H2" = h2 board
getField board "H3" = h3 board
getField board "H4" = h4 board
getField board "H5" = h5 board
getField board "H6" = h6 board
getField board "H7" = h7 board
getField board "H8" = h8 board

setField :: Board -> Field -> String -> Board
setField board newField "A1" = board {a1 = newField}
setField board newField "A2" = board {a2 = newField}
setField board newField "A3" = board {a3 = newField}
setField board newField "A4" = board {a4 = newField}
setField board newField "A5" = board {a5 = newField}
setField board newField "A6" = board {a6 = newField}
setField board newField "A7" = board {a7 = newField}
setField board newField "A8" = board {a8 = newField}
setField board newField "B1" = board {b1 = newField}
setField board newField "B2" = board {b2 = newField}
setField board newField "B3" = board {b3 = newField}
setField board newField "B4" = board {b4 = newField}
setField board newField "B5" = board {b5 = newField}
setField board newField "B6" = board {b6 = newField}
setField board newField "B7" = board {b7 = newField}
setField board newField "B8" = board {b8 = newField}
setField board newField "C1" = board {c1 = newField}
setField board newField "C2" = board {c2 = newField}
setField board newField "C3" = board {c3 = newField}
setField board newField "C4" = board {c4 = newField}
setField board newField "C5" = board {c5 = newField}
setField board newField "C6" = board {c6 = newField}
setField board newField "C7" = board {c7 = newField}
setField board newField "C8" = board {c8 = newField}
setField board newField "D1" = board {d1 = newField}
setField board newField "D2" = board {d2 = newField}
setField board newField "D3" = board {d3 = newField}
setField board newField "D4" = board {d4 = newField}
setField board newField "D5" = board {d5 = newField}
setField board newField "D6" = board {d6 = newField}
setField board newField "D7" = board {d7 = newField}
setField board newField "D8" = board {d8 = newField}
setField board newField "E1" = board {e1 = newField}
setField board newField "E2" = board {e2 = newField}
setField board newField "E3" = board {e3 = newField}
setField board newField "E4" = board {e4 = newField}
setField board newField "E5" = board {e5 = newField}
setField board newField "E6" = board {e6 = newField}
setField board newField "E7" = board {e7 = newField}
setField board newField "E8" = board {e8 = newField}
setField board newField "F1" = board {f1 = newField}
setField board newField "F2" = board {f2 = newField}
setField board newField "F3" = board {f3 = newField}
setField board newField "F4" = board {f4 = newField}
setField board newField "F5" = board {f5 = newField}
setField board newField "F6" = board {f6 = newField}
setField board newField "F7" = board {f7 = newField}
setField board newField "F8" = board {f8 = newField}
setField board newField "G1" = board {g1 = newField}
setField board newField "G2" = board {g2 = newField}
setField board newField "G3" = board {g3 = newField}
setField board newField "G4" = board {g4 = newField}
setField board newField "G5" = board {g5 = newField}
setField board newField "G6" = board {g6 = newField}
setField board newField "G7" = board {g7 = newField}
setField board newField "G8" = board {g8 = newField}
setField board newField "H1" = board {h1 = newField}
setField board newField "H2" = board {h2 = newField}
setField board newField "H3" = board {h3 = newField}
setField board newField "H4" = board {h4 = newField}
setField board newField "H5" = board {h5 = newField}
setField board newField "H6" = board {h6 = newField}
setField board newField "H7" = board {h7 = newField}
setField board newField "H8" = board {h8 = newField}

data Field = B | W | O deriving Eq

instance Show Field
    where
        show B = "B"
        show W = "W"
        show O = " "

startBoard :: Board
startBoard =
    Board
    {
        a8 = B, b8 = B, c8 = B, d8 = B, e8 = B, f8 = B, g8 = B, h8 = B,
        a7 = B, b7 = B, c7 = B, d7 = B, e7 = B, f7 = B, g7 = B, h7 = B,
        a6 = O, b6 = O, c6 = O, d6 = O, e6 = O, f6 = O, g6 = O, h6 = O,
        a5 = O, b5 = O, c5 = O, d5 = O, e5 = O, f5 = O, g5 = O, h5 = O,
        a4 = O, b4 = O, c4 = O, d4 = O, e4 = O, f4 = O, g4 = O, h4 = O,
        a3 = O, b3 = O, c3 = O, d3 = O, e3 = O, f3 = O, g3 = O, h3 = O,
        a2 = W, b2 = W, c2 = W, d2 = W, e2 = W, f2 = W, g2 = W, h2 = W,
        a1 = W, b1 = W, c1 = W, d1 = W, e1 = W, f1 = W, g1 = W, h1 = W
    }
