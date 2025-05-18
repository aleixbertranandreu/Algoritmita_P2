-- Implementation of team selection algorithm in Haskell
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode, WriteMode), openFile, hGetLine, hClose, hPutStrLn, hIsEOF)
import Data.List (sort, tails, minimumBy, maximumBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Text.Printf (printf)

-- | Read input from a file, parsing n and points
llegirEntrada :: FilePath -> IO (Int, [(Int, Int)])
llegirEntrada nomFitxer = do
    handle <- openFile nomFitxer ReadMode
    nLine <- hGetLine handle
    let n = read nLine :: Int
    
    -- Read all remaining lines and parse points
    contents <- readPointsFromHandle handle []
    hClose handle
    
    return (n, contents)
  where
    -- Helper function to read points recursively
    readPointsFromHandle handle acc = do
      eof <- isEOF handle
      if eof
        then return (reverse acc)
        else do
          line <- hGetLine handle
          let [x, y] = map read $ words line
          readPointsFromHandle handle ((x, y) : acc)
          
    -- Check if at end of file
    isEOF h = do
      eof <- hIsEOF h
      return eof

-- | Write results to output file
escriureSortida :: FilePath -> Double -> [Int] -> IO ()
escriureSortida nomFitxer distancia equip1 = do
    handle <- openFile nomFitxer WriteMode
    -- Format distance with 6 decimal places
    hPutStrLn handle $ printf "%.6f" distancia
    -- Join team1 members with spaces
    hPutStrLn handle $ unwords (map show equip1)
    hClose handle

-- | Calculate Euclidean distance between two points
dist :: (Int, Int) -> (Int, Int) -> Double
dist (x1, y1) (x2, y2) = sqrt $ fromIntegral ((x1 - x2)^2 + (y1 - y2)^2)

-- | Generate all combinations of r elements from a list
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

-- | Maximize the minimum distance between two teams
maximitzaDistancia :: [(Int, Int)] -> (Double, [Int], [Int])
maximitzaDistancia punts = 
    let n = length punts `div` 2
        allIndices = [0..(length punts - 1)]
        
        -- Calculate result for each possible team1 configuration
        results = [calcDistance team1Indices | team1Indices <- combinations n allIndices]
        
        -- Find the best result (maximum minimum distance)
        (bestDistance, bestTeam1, bestTeam2) = maximumBy (comparing (\(d,_,_) -> d)) results
    in (bestDistance, bestTeam1, bestTeam2)
  where
    -- Helper to calculate distance and teams for a given team1 configuration
    calcDistance team1Indices = 
        let team1Set = Set.fromList team1Indices
            team2Set = Set.fromList [0..(length punts - 1)] `Set.difference` team1Set
            team1Indices' = Set.toList team1Set
            team2Indices' = Set.toList team2Set
            
            -- Calculate minimum distance between any pair of points from team1 to team2
            minDistance = minimum [dist (punts !! i) (punts !! j) | i <- team1Indices', j <- team2Indices']
            
            -- Add 1 to indices to match original 1-based indexing
            team1 = sort [i + 1 | i <- team1Indices']
            team2 = sort [j + 1 | j <- team2Indices']
        in (minDistance, team1, team2)

-- | Main program entry point
main :: IO ()
main = do
    args <- getArgs
    (inputFile, outputFile) <- case args of
        (inFile:outFile:_) -> return (inFile, outFile)
        _                  -> error "Usage: program inputFile outputFile"
    
    (n, punts) <- llegirEntrada inputFile
    let (distancia, equip1, equip2) = maximitzaDistancia punts
    escriureSortida outputFile distancia equip1
