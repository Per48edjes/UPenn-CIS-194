import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile] -> do
            input <- readFile inputFile
            mapM_ putStrLn (firstWords input)
        _ -> putStrLn "error: exactly one argument needed"

firstWords :: String -> [String]
firstWords = map (head . words) . lines
