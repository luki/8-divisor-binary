module Main where

type State = Int
type Input = String

nextState :: State -> Input -> Bool

nextState 3 [] = True
nextState _ [] = False

nextState 0 (x:xs)
  | x == '0'  = nextState 1 xs
  | otherwise = nextState 0 xs

nextState 1 (x:xs)
  | x == '0'  = nextState 2 xs
  | otherwise = nextState 0 xs

nextState 2 (x:xs)
  | x == '0'  = nextState 3 xs
  | otherwise = nextState 0 xs

nextState 3 (x:xs)
  | x == '0'  = nextState 3 xs
  | otherwise = nextState 0 xs

isWord :: String -> Bool
isWord = nextState 0

main :: IO ()
main = do
  putStrLn . show . isWord $ "10001000"
  putStrLn . show . isWord $ "10001"
