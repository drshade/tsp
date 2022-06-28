import TspFile (readTspFile, TspFile(..))
import Lib (bruteforce)

main :: IO ()
main = do
    fileReading <- readTspFile "data/small13.tsp"
    case fileReading of
        Left err -> print err
        Right file@(TspFile tags coords) -> do
            print file
            let solution = bruteforce coords
            putStrLn $ "Shortest path -> " <> show solution
    pure ()
