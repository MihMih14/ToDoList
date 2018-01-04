
data Task = Task {
  describe :: String,
  complete :: Complete
} deriving(Show)

data Complete = Yes | No deriving(Show)

main :: IO()
main = do
    putStr "$ "
    instruction <- getLine
    putStrLn instruction
    main

add :: [Task] -> Task -> [Task]
add tasks task = tasks ++ [task]

remove :: [Task] -> Int -> [Task]
remove tasks i =
    if i <= 0 || i > length tasks then do
        tasks
    else do
        let (lSide, rSide) = splitAt i tasks
        init lSide ++ rSide
