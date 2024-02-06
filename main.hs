import System.IO
import System.Environment

import Board

data Direction = Forward | Backward deriving Eq
data Player = White | Black deriving (Show, Eq)

parseMoves :: String -> [String]
parseMoves =
    foldr (\x (a:acc) -> if x == '-' then "":a:acc else (x:a):acc) [""]

checkFieldCoords :: Char -> Char -> Bool
checkFieldCoords a b =
    a `elem` ['A'..'H'] && b `elem` ['1'..'8']

checkMoves :: [String] -> Bool
checkMoves moves =
    length moves == length (filter (\a -> length a == 2) moves) &&
    foldl (\acc [a, b] -> acc && checkFieldCoords a b) True moves &&
    length moves > 1

nextN :: Char -> Direction -> Char
nextN n Forward = succ n
nextN n Backward = pred n

checkColumnJump :: Board -> String -> String -> Char -> Direction -> Bool
checkColumnJump board [a, b] [c, d] n direction
    | n == d = getField board [c, d] == O
    | otherwise = (getField board [a, n] /= O) && checkColumnJump board [a, b] [c, d] (nextN n direction) direction

checkRowJump :: Board -> String -> String -> Char -> Direction -> Bool
checkRowJump board [a, b] [c, d] n direction 
    | n == c = getField board [c, d] == O
    | otherwise = (getField board [n, b] /= O) && checkRowJump board [a, b] [c, d] (nextN n direction) direction

tryOneJump :: Board -> String -> String -> Bool
tryOneJump board [a, b] [c, d]
    | a == c && succ b < d = checkColumnJump board [a, b] [c, d] (succ b) Forward
    | a == c && pred b > d = checkColumnJump board [a, b] [c, d] (pred b) Backward
    | a == c && b == d = True
    | succ a < c && b == d = checkRowJump board [a, b] [c, d] (succ a) Forward
    | pred a > c && b == d = checkRowJump board [a, b] [c, d] (pred a) Backward
    | otherwise = False

tryJump :: Board -> [String] -> Bool
tryJump board [from] = True
tryJump board (from:to:rest) = 
    tryOneJump board from to && tryJump board (to:rest)

tryMove :: Board -> [String] -> Bool
tryMove board [[a, b], [c, d]]
    | a == c && succ b == d && getField board [c, d] == O = True
    | a == c && pred b == d && getField board [c, d] == O = True
    | a == c && b == d && getField board [c, d] == O = True
    | succ a == c && b == d && getField board [c, d] == O = True
    | pred a == c && b == d && getField board [c, d] == O = True
    | otherwise = False

move :: Board -> Player -> String -> String -> Board
move board Black from to =
    setField (setField board O from) B to
move board White from to =
    setField (setField board O from) W to

checkWin :: Board -> Player -> Bool
checkWin board White =
    (a7 board == W) && (a8 board == W) && (b7 board == W) && (b8 board == W) && (c7 board == W) && (c8 board == W) && (d7 board == W) && (d8 board == W)
    && (e7 board == W) && (e8 board == W) && (f7 board == W) && (f8 board == W) && (g7 board == W) && (g8 board == W) && (h7 board == W) && (h8 board == W)
checkWin board Black =
    (a1 board == B) && (a2 board == B) && (b1 board == B) && (b2 board == B) && (c1 board == B) && (c2 board == B) && (d1 board == B) && (d2 board == B)
    && (e1 board == B) && (e2 board == B) && (f1 board == B) && (f2 board == B) && (g1 board == B) && (g2 board == B) && (h1 board == B) && (h2 board == B)

other :: Player -> Player
other White = Black
other Black = White

game :: Board -> Player -> IO ()
game board player =
    do
        print board

        if checkWin board (other player)
            then 
                do
                    putStr "\n!!! "
                    putStr (show (other player))
                    putStrLn " WINS !!!"
            else do
                putStr (show player)
                putStrLn " turn"
                moves <- getLine
                let movesArray = parseMoves moves

                if checkMoves movesArray && ((getField board (head movesArray) == W && player == White) || (getField board (head movesArray) == B && player == Black))
                    then
                        if length movesArray > 2
                            then
                                if tryJump (setField board O (head movesArray)) movesArray
                                    then game (move board player (head movesArray) (last movesArray)) (other player)
                                    else putStrLn "Invalid input, try again!" >> game board player
                            else
                                    if tryJump board movesArray || tryMove board movesArray
                                    then game (move board player (head movesArray) (last movesArray)) (other player)
                                    else putStrLn "Invalid input, try again!" >> game board player
                    else
                        putStrLn "Invalid input, try again!" >> game board player

showInstructions :: Handle -> IO ()
showInstructions handle = do
    eof <- hIsEOF handle
    if eof 
        then do
            putStrLn ""
            welcomeMessage
        else do
            line<-hGetLine handle
            putStrLn line
            showInstructions handle

welcomeMessage :: IO ()
welcomeMessage = do
    putStrLn "Hello! Welcome to the \"jumpers board game!\""
    putStrLn "To start game type start, to show instuctions how to play type help."
    command <- getLine
    
    if command == "start"
        then 
            putStrLn "Gl, hf!\n"
        else 
            if command == "help"
                then do 
                    fileHandle <- openFile "instructions.txt" ReadMode
                    showInstructions fileHandle
                else putStrLn "Incorrect command, try again!\n" >> welcomeMessage

startGame :: IO ()
startGame = do
    putStrLn "Choose starting color: black or white"
    color <- getLine

    if color == "black"
        then game startBoard Black
        else
            if color == "white"
                then game startBoard White
                else putStrLn "Wrong color, try again\n" >> startGame

main :: IO ()
main = do
    welcomeMessage
    startGame
