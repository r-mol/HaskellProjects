module Project where

import Text.Read (readMaybe)

type Task = String

printAllTasks :: [Task] -> IO ()
printAllTasks [] = putStrLn "No tasks yet!!"
printAllTasks (task:tasks) = do
    putStrLn task
    printAllTasks tasks

removeTask :: Int -> [Task] -> [Task]
removeTask _ [] = []
removeTask i (task : tasks)
    | i == 0 = tasks
    | otherwise = task : removeTask (i - 1) tasks 

data Command
    = Exit_
    | ShowTasks
    | RemoveTask Int
    | AddTask Task

makeProgram :: a -> IO a
makeProgram = return

handleCommand :: Command -> [Task] -> IO (Maybe [Task])
handleCommand command tasks = case command of
    Exit_ -> return Nothing
    ShowTasks -> do
        printAllTasks tasks
        continiue tasks
    RemoveTask taskIndex -> do
        continiue (removeTask taskIndex tasks)
    AddTask newTask -> do
        putStrLn ("New task: " ++ newTask)
        continiue (newTask : tasks)
    where
        continiue newTask = return (Just newTask)

getCommand :: IO (Maybe Command)
getCommand = do
    input <- getLine
    case input of
        "/exit" -> continiue Exit_
        "/show" -> continiue ShowTasks
        "/done" -> do
            indexStr <- getLine
            case readMaybe indexStr of
                Nothing -> do
                    putStrLn "ERROR: invalid index"
                    return Nothing
                Just i -> continiue (RemoveTask i)
        newTask -> continiue (AddTask newTask)
    where
        continiue newTask = return (Just newTask)

-- | Default entry point.
run :: IO ()
run = runWith ["buy milk", "grrunade hw"]

runWith :: [Task] -> IO () 
runWith tasks = do
    putStrLn "command>"
    command <- getCommand
    case command of
        Nothing -> do
            putStrLn "ERROR: unrecognized command"
            runWith tasks
        Just command' -> do
            newTasks <- handleCommand command' tasks
            case newTasks of
                Nothing -> putStrLn "bye!"
                Just newTasks' -> runWith newTasks'