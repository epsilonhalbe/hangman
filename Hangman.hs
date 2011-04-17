--author: Martin "ε/2" Heuschober
--

module Hangman (make_blanks, main) where

import Data.List (intersperse)
import Data.Set (fromList)
import System.Random (getStdRandom, randomR)
import Control.Monad (liftM)
import System.IO (hSetBuffering,stdin, BufferMode(NoBuffering))
import System.Console.ANSI (clearScreen)

main:: IO ()
main = do
    clearScreen
    putStrLn "this is a game of hangman"
    guessword <- getRandomWord
    drawMan 0
    putStrLn (intersperse ' '( make_blanks [] guessword))
    game_loop [] [] guessword
    putStrLn "Want to play again??"
    hSetBuffering stdin NoBuffering
    c <- getChar
    regame c
        where regame c
                | elem c "yY" = do
                    putStrLn "game on mate"
                    main
                | elem c "nN" = putStrLn "\nGame Over"
                | otherwise = do
                    putStrLn "\nyou must type one of Yy to confirm or nN to abort"
                    hSetBuffering stdin NoBuffering
                    c'<- getChar
                    regame c'

game_loop:: [Char] -> [Char] -> String -> IO ()
game_loop ws rs guessword = do -- ws ... wrong letters, rs ... right letters
    let n = length ws
    putStrLn "please do guess a letter"
    hSetBuffering stdin NoBuffering
    c <- getChar
    if (elem c guessword)
        then do
                clearScreen
                drawMan n
                drawGuess ws (rs++[c]) guessword
                if (fromList (rs++[c])) ==  (fromList guessword)
                    then putStrLn ("congratz you've got it right: "++guessword)
                    else game_loop ws (rs++[c]) guessword
        else do
                clearScreen
                drawMan (n+1)
                drawGuess (ws++[c]) rs guessword
                if (n+1) < 11
                    then game_loop (ws++[c]) rs guessword
                    else putStrLn ("Game Over - It would have been:"++guessword)

drawGuess :: [Char] -> [Char] -> String -> IO ()
drawGuess ws rs guessword = do
    putStrLn ((intersperse ' ' (make_blanks rs guessword))++"\n")
    if null ws
        then putStrLn ("no wrong guesses yet")
        else putStrLn ("You already guessed: "++(intersperse ',' ws))

make_blanks :: [Char] -> String -> [Char]
make_blanks rs guessword = map (argElem rs) guessword
    where argElem cs' x
            |elem x cs' = x
            |otherwise = '_'

{- getting dictionary.txt with input other than written myself leads to crash-}
getRandomWord :: IO String
getRandomWord = do
    xs <- getLines "dictionary.txt"
    n <- getStdRandom (randomR (0,(length xs)-1))
    return (xs !! n)

getLines :: FilePath -> IO [String]
getLines = liftM lines . readFile


drawMan :: Int -> IO ()
drawMan n
    |n == 0 = putStrLn "\n\
\____________________\n\
\|                   |\n\
\|                   |\n\
\|                   |\n\
\|                   |\n\
\|                   |\n\
\|                   |\n\
\|                   |\n\
\|                   |\n\
\|                   |\n\
\|___________________|\n"
    |n == 1 = putStrLn "\n\
\____________________\n\
\|                   |\n\
\|                   |\n\
\|                   |\n\
\|                   |\n\
\|                   |\n\
\|                   |\n\
\|                   |\n\
\|   _____________   |\n\
\|  /             \\  |\n\
\|_/_______________\\_|\n"
    |n == 2 = putStrLn "\n\
\____________________\n\
\|                   |\n\
\|   |               |\n\
\|   |               |\n\
\|   |               |\n\
\|   |               |\n\
\|   |               |\n\
\|   |               |\n\
\|   |____________   |\n\
\|  /             \\  |\n\
\|_/_______________\\_|\n"
    |n == 3 = putStrLn "\n\
\____________________\n\
\|    ___________    |\n\
\|   |               |\n\
\|   |               |\n\
\|   |               |\n\
\|   |               |\n\
\|   |               |\n\
\|   |               |\n\
\|   |____________   |\n\
\|  /             \\  |\n\
\|_/_______________\\_|\n"
    |n == 4 = putStrLn "\n\
\____________________\n\
\|    ___________    |\n\
\|   |  /            |\n\
\|   | /             |\n\
\|   |/              |\n\
\|   |               |\n\
\|   |               |\n\
\|   |               |\n\
\|   |____________   |\n\
\|  /             \\  |\n\
\|_/_______________\\_|\n"
    |n == 5 = putStrLn "\n\
\____________________\n\
\|    ___________    |\n\
\|   |  /            |\n\
\|   | /             |\n\
\|   |/              |\n\
\|   |               |\n\
\|   |               |\n\
\|   |               |\n\
\|   |____________   |\n\
\|  /             \\  |\n\
\|_/_______________\\_|\n"
    |n == 6 = putStrLn "\n\
\____________________\n\
\|    ___________    |\n\
\|   |  /      $     |\n\
\|   | /             |\n\
\|   |/              |\n\
\|   |               |\n\
\|   |               |\n\
\|   |               |\n\
\|   |____________   |\n\
\|  /             \\  |\n\
\|_/_______________\\_|\n"
    |n == 7 = putStrLn "\n\
\____________________\n\
\|    ___________    |\n\
\|   |  /      $     |\n\
\|   | /       @     |\n\
\|   |/              |\n\
\|   |               |\n\
\|   |               |\n\
\|   |               |\n\
\|   |____________   |\n\
\|  /             \\  |\n\
\|_/_______________\\_|\n"
    |n == 8 = putStrLn "\n\
\____________________\n\
\|    ___________    |\n\
\|   |  /      $     |\n\
\|   | /       @     |\n\
\|   |/        |     |\n\
\|   |         |     |\n\
\|   |               |\n\
\|   |               |\n\
\|   |____________   |\n\
\|  /             \\  |\n\
\|_/_______________\\_|\n"
    |n == 9 = putStrLn "\n\
\____________________\n\
\|    ___________    |\n\
\|   |  /      $     |\n\
\|   | /       @     |\n\
\|   |/       \\|/    |\n\
\|   |         |     |\n\
\|   |               |\n\
\|   |               |\n\
\|   |____________   |\n\
\|  /             \\  |\n\
\|_/_______________\\_|\n"
    |n == 10 = putStrLn "\n\
\____________________\n\
\|    ___________    |\n\
\|   |  /      $     |\n\
\|   | /       @     |\n\
\|   |/       \\|/    |\n\
\|   |         |     |\n\
\|   |        /_\\    |\n\
\|   |        | |    |\n\
\|   |___\\____|_|_   |\n\
\|  /             \\  |\n\
\|_/_______________\\_|\n"
    |n == 11 = putStrLn "\n\
\____________________\n\
\|    ___________    |\n\
\|   |  /      $     |\n\
\|   | /       $     |\n\
\|   |/        @     |\n\
\|   |        \\|/    |\n\
\|   |         |     |\n\
\|   |        / \\    |\n\
\|   |___/________   |\n\
\|  /             \\  |\n\
\|_/_______________\\_|\n"
    |otherwise = putStrLn "out of bounds error"

