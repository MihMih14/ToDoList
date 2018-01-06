import Data.List.Split
import Data.Char

data Task = Task {
  describe :: String,
  complete :: Complete
} deriving(Eq, Show)

data Complete = Yes | No deriving(Eq, Show)

main :: IO()
main = do
    let tasks = []
    app tasks

app:: [Task] -> IO()
app tasks = do
  putStr "$ "
  instruction <- getLine
  case instruction of
      "add" -> do
          putStrLn "Please enter description"
          description <- getLine
          app $ add tasks (Task description No)
      "remove" -> do
           putStrLn "Please Enter number tasks: "
           num <- getLine
           app $ remove tasks (read num)
      "complete" -> do
            putStrLn "Plese enter number of task"
            num <- getLine
            app $ completeTask tasks (read num)
      "help" -> do
            putStrLn "addfunc"
            app tasks
      "show" -> do
            showItems tasks
            app tasks
      "exit" -> putStrLn "good bye =)"
      _ -> do
          putStrLn ""
          app tasks



add :: [Task] -> Task -> [Task]
add tasks task = tasks ++ [task]

remove :: [Task] -> Int -> [Task]
remove tasks i =
    if i <= 0 || i > length tasks then do
        tasks
    else do
        let (lSide, rSide) = splitAt i tasks
        init lSide ++ rSide

getCurrentItems :: [Task] -> [Task]
getCurrentItems tasks = filter ((== No) . complete) tasks

showItems :: [Task] -> IO ()
showItems tasks
    | null tasks = putStrLn ""
    | otherwise = do
        putStrLn $ "Name of task : " ++ describe (head tasks)
        putStrLn $ "Compeleted : " ++ parseString (complete (head tasks))
        showItems $ tail tasks

completeTask :: [Task] -> Int -> [Task]
completeTask tasks i
    | null tasks = tasks
    | i <= 0 || i > length tasks = tasks
    | otherwise = do
        let (lSide, rSide) = splitAt i tasks
        let currentTask = last lSide
        let task = Task (describe currentTask) Yes
        init lSide ++ [task] ++ rSide

parseString :: Complete -> [Char]
parseString complete
    | complete == Yes = "Yes"
    | complete == No = "No"
    | otherwise = ""
