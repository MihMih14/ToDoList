
data Task = Task {
  describe :: String,
  complete :: Complete
} deriving(Show)

data Complete = Yes | No deriving(Show)

main = do
    putStr "$ "
    instruction <- getLine
    putStrLn instruction
    main
