import System.Environment (getArgs)
import Text.Printf (printf)
import Data.List (sort, (\\))

type Point = (Int, Int)
type Distance = Double

main :: IO ()
main = do
    args <- getArgs
    contents <- if null args then getContents else readFile (head args)
    let ls = lines contents
        k = read (head ls)
        pts = map ((\[a,b] -> (a,b)) . map read . words) (tail ls)
        n = length pts
        teams = combinations k [0..n-1]
        results = [ (minDistBetweenTeams pts team, team) | team <- teams ]
        maxDist = maximum (map fst results)
        bestTeams = [ team | (d, team) <- results, abs (d - maxDist) < 1e-6 ]
        finalTeam = sort (map (+1) (head bestTeams))  -- índexs 1-based
    printf "%.6f\n" maxDist
    putStrLn $ unwords (map show finalTeam)

-- Distància mínima entre membres d’equips diferents
minDistBetweenTeams :: [Point] -> [Int] -> Distance
minDistBetweenTeams pts teamA =
    let teamB = [0..length pts - 1] \\ teamA
    in minimum [euclidean (pts !! i) (pts !! j) | i <- teamA, j <- teamB]

-- Distància euclidiana
euclidean :: Point -> Point -> Distance
euclidean (x1, y1) (x2, y2) =
    sqrt . fromIntegral $ (x1 - x2)^2 + (y1 - y2)^2

-- Combinacions de k elements
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs)
    | k < 0     = []
    | otherwise = [x:ys | ys <- combinations (k-1) xs] ++ combinations k xs
