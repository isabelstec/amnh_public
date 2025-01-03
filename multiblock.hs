import System.IO
import System.Process
import System.Directory
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import Prelude 
import System.FilePath.Posix
import System.Environment 
import Epsilon (compareWithEpsilon)
import Debug.Trace
import System.Exit (die)
import Control.Monad
import System.FilePath


valuesAroundSmallest :: (Ord a, Num a) => [a] -> Int -> (Maybe a, Maybe a, Int)
valuesAroundSmallest xs idx =
    let left = if idx > 0 then Just (xs !! (idx - 1)) else Nothing
        right =
            if idx < length xs - 1
                then Just (xs !! (idx + 1))
                else case xs of
                        [] -> Nothing
                        _ -> Just ((xs !! idx) + 1) 
    in (left, right, idx)

valuesBetween :: (Fractional a, Enum a) => a -> a -> a -> [a]
valuesBetween left right middle = 
    let step = (right - left) / 10
        range = [left, left + step .. right]
        (before, after) = splitAt 5 range
    in before ++ [middle] ++ after

branchLengthUpdate :: String -> String -> String -> String
branchLengthUpdate distribution newNumber line
    | prefix `isPrefixOf` line = prefix ++ distribution ++ ":" ++ newNumber
    | otherwise = line
  where
    prefix = "\tBranchLength : "

processBlock :: String -> String -> [String] -> [String]
processBlock distribution newNumber = map (branchLengthUpdate distribution newNumber)

splitIntoBlocks1 :: [String] -> String -> [[String]]
splitIntoBlocks1 lines blockStartMarker = go lines []
  where
    go [] currentBlock = [currentBlock | not (null currentBlock)]
    go (line:rest) currentBlock
        | blockStartMarker `isInfixOf` line = 
            currentBlock : go rest [line]
        | otherwise = 
            go rest (currentBlock ++ [line])

splitIntoBlocks2 :: [String] -> String -> [[String]]
splitIntoBlocks2 lines blockStartMarker = go lines [] False
  where
    go [] currentBlock _ = [currentBlock | not (null currentBlock)] 
    go (line:rest) currentBlock foundFirstBlock
        | blockStartMarker `isInfixOf` line = 
            if foundFirstBlock 
            then 
                let newBlock = [line]
                in go rest [] True ++ [newBlock] 
            else 
                go rest [line] True  
        | foundFirstBlock = 
            
            go rest (currentBlock ++ [line]) True
        | otherwise = 
            go rest currentBlock foundFirstBlock


modifyConfigFile :: FilePath -> [[String]] -> IO ()
modifyConfigFile filePath blocks = writeFile filePath (unlines $ concat blocks)

getNumbers :: String -> String
getNumbers str = let (_, rest) = break isDigit str
                     (num, _) = span (\c -> isDigit c || c == '.') rest
                    in num

getLastLine :: FilePath ->  IO (Maybe String)
getLastLine filePath = do
    contents <- readFile filePath
    let reversed = reverse (lines contents)
    return $ case reversed of 
        [] -> Nothing
        (lastLine:_) -> Just lastLine

processArgs :: [String] -> (String, String, String, String)
processArgs inArgs =
    if length inArgs /= 4
        then errorWithoutStackTrace "Require four arguments: configFile dataPath name distribution"
        else
            let config = head inArgs
                datapath = inArgs !! 1
                naming = inArgs !! 2
                distribution = inArgs !! 3
            in (config, datapath, naming, distribution)

processFilesInDirectory :: FilePath -> IO [FilePath]
processFilesInDirectory datapath = do
    allContents <- getDirectoryContents datapath
    let validContents = filter (`notElem` [".", ".."]) allContents
    filterM (\f -> doesFileExist (datapath </> f)) validContents

extractBlockModel :: [String] -> Maybe String 
extractBlockModel block =
    let blockModelLine = find ("BlockModel" `isPrefixOf`) block 
    in case blockModelLine of 
        Just line -> Just (takeWhile (/= ' ') . dropWhile (== ' ') $ drop (length "BlockModel ") line)
        Nothing -> Nothing

extractIdentifier :: String -> String
extractIdentifier filename = 
    let baseName = takeBaseName filename 
        (_, identifier) = break (== '-') baseName  
    in tail identifier 

extractExact :: String -> String 
extractExact line = 
    if "BlockModel" `isPrefixOf` line
    then takeWhile (/= ' ') . dropWhile (== ' ') $ drop (length "BlockModel ") line
    else ""

findMatchingBlock :: [[String]] -> String -> Maybe [String]
findMatchingBlock blocks identifier = 
    let pattern = "-" ++ identifier ++ "-"
        matchesPattern block = any (\line -> pattern `isInfixOf` extractExact line) block
    in find matchesPattern blocks

updateBranchLengthIfMatch :: [String] -> String -> String -> [String] -> [String]
updateBranchLengthIfMatch optimalBlock distribution newBranchLength block = 
    if head block == head optimalBlock
        then map (updateBranchLengthLine distribution newBranchLength) block
        else block

updateBranchLengthLine :: String -> String -> String -> String
updateBranchLengthLine distribution newBranchLength line
    | "\tBranchLength : " `isPrefixOf` line =
        let prefix = "\tBranchLength : "
        in prefix ++ distribution ++ ":" ++ newBranchLength ++ ";"
    | otherwise = line

saveOptimalBlock :: FilePath -> [String] -> String -> String -> IO ()
saveOptimalBlock filePath optimalBlock distribution newBranchLength = do
    {-putStrLn $ "Saving optimal block to: " ++ filePath
    putStrLn $ "Optimal block:\n" ++ unlines optimalBlock
    putStrLn $ "New branch length: " ++ newBranchLength-}
    contents <- readFile filePath
    let blocks = splitIntoBlocks1 (lines contents) "BlockModel"

    {-putStrLn "Original blocks:"
    mapM_ (putStrLn . unlines) blocks-}

    let updatedBlocks = map (updateBranchLengthIfMatch optimalBlock distribution newBranchLength) blocks
    {-putStrLn "Updated blocks:"
    mapM_ (putStrLn . unlines) updatedBlocks-}

    let tempFilePath = filePath ++ ".tmp"
    writeFile tempFilePath (unlines $ concat updatedBlocks)
    renameFile tempFilePath filePath


tryingtoloop :: String -> String -> String -> String -> String -> String -> [String] -> IO (String, String)
tryingtoloop newNumber config datapath naming distribution blockmodel blockLines = do
    let outputFile = naming ++ ".config"
    let newNumbergood = newNumber ++ ";"
    let modifiedBlock = processBlock distribution newNumbergood blockLines

    configcontents <- readFile config 
    let blocks = splitIntoBlocks1 (lines configcontents) "BlockModel"
    let updatedBlocks = map (\b -> if head b == head blockLines then modifiedBlock else b) blocks 
    modifyConfigFile outputFile updatedBlocks 

    copyFile outputFile config 
    {--maybeComplexityPath <- findExecutable "phyloComplexity"
    complexityPath <- case maybeComplexityPath of
        Just path -> return path
        Nothing -> die "phyloComplexity executable not found in PATH"-}
    let complexityPath = "/home/istec/Desktop/PhyloAlgInfo/dist-newstyle/build/x86_64-linux/ghc-9.10.1/phyloComplexity-0.1.0/x/phyloComplexity/build/phyloComplexity/phyloComplexity"
    let stub = naming ++ "-" ++ newNumber
    callProcess complexityPath [outputFile, stub]

    let complexityFile = stub ++ ".complexity"
    contents <- readFile complexityFile
    let firstline = head (lines contents)
    let justComplexity = getNumbers firstline

    let test = "set(criterion:PMDL)\n" ++ "set(modelComplexity:" ++ justComplexity ++ ")\n"

    dataFiles <- processFilesInDirectory datapath
    let newfile = stub ++ ".pg"

    withComplexity <- openFile newfile WriteMode
    hPutStr withComplexity test

    let blocks2 = splitIntoBlocks2 (lines configcontents) "BlockModel"
    --print ("datafiles" , length dataFiles)

    if length dataFiles == 1 
    then do
        print ("what", dataFiles)
        let datafile = head dataFiles
        let readline = "read(\"" ++ datapath ++ "/" ++ datafile ++ "\", tcm: \"./" ++ stub ++ blockmodel ++ ".bit.tcm" ++ "\")\n"
        hPutStr withComplexity readline
        putStrLn ("BlockModel: " ++ blockmodel)

    else do
        forM_ dataFiles $ \datafile -> do
            let readline = "read(\"" ++ datapath ++ "/" ++ datafile ++ "\", tcm: \"./"
            let datafileIdentifier = extractIdentifier datafile
            let matchingBlock = findMatchingBlock blocks2 datafileIdentifier

            case matchingBlock of
                Just block -> do
                    let maybeblock = extractBlockModel block
                    case maybeblock of
                        Just blockModel -> do
                            let readline2 = stub ++ blockModel ++ ".bit.tcm" ++ "\")\n"
                            let together = readline ++ readline2
                            hPutStr withComplexity together
                        Nothing -> putStrLn ("No block model found for " ++ datafile)
                Nothing -> putStrLn ("No matching block found for " ++ datafile)

    let restofPhygFile = "build(distance, rdwag)" ++"\nreport(\"./" ++ stub ++ ".csv\", parameterEstimation, overwrite)\n" ++ "report(\"./" ++ stub ++ ".dot\", graphs, dotpdf, overwrite)"
    hPutStr withComplexity restofPhygFile

    hClose withComplexity


    maybePhygPath <- findExecutable "phyg"
    hPutStrLn stderr (show maybePhygPath)
    phygPath <- case maybePhygPath of
        Just path -> return path
        Nothing -> die "phyg executable not found in PATH"

    --let phygPath = "/home/istec/Desktop/PhyG/dist-newstyle/build/x86_64-linux/ghc-9.10.1/PhyG-1.1/x/phyg/opt/build/phyg/phyg"

    callProcess phygPath [newfile]

    let dotfile = "./" ++ stub ++ ".dot"
    lastline <- getLastLine dotfile
    case lastline of
        Nothing -> return (newNumber, "")
        Just line -> do
            let pmdl = getNumbers line
            let writing = "BranchLength: " ++ newNumber ++ " with a PMDL of: " ++ pmdl
            let filename = "PMDL" ++ naming ++ ".txt"
            appendFile filename (writing ++ "\n")
            return (newNumber, pmdl)


loopProcess :: [String] -> Double -> String -> FilePath -> String -> String -> String -> [String] -> IO ()
loopProcess newNumbers prevPMDL blockmodel copyConfig datapath naming distribution blockLines = do
    results <- mapM (\newNumber -> tryingtoloop newNumber copyConfig datapath naming distribution blockmodel blockLines) newNumbers

    let pmdlValues = map snd results
    let smallestPMDL = minimum pmdlValues
    let smallestPMDLString = show smallestPMDL
    let filteredResults = filter ((== smallestPMDLString) . show . snd) results

    case filteredResults of
        [] -> putStrLn "Error: No results found for the smallest PMDL." >> return ()
        _ -> do
            let smallestNumber = fst (head filteredResults)
            let indexsmallest = fromJust $ elemIndex smallestNumber newNumbers
            let floatlist = map read newNumbers :: [Double]
            let (left, right, _) = valuesAroundSmallest floatlist indexsmallest
            let smallestPMDLDouble = read smallestPMDL :: Double
            let (optimalBranchLength, _) = head filteredResults

            if isNothing left || isNothing right
                then putStrLn "Error: left or right is Nothing" >> return ()
                else do
                    let newNumbersList = valuesBetween (fromJust left) (fromJust right) (read smallestNumber :: Double)
                    if compareWithEpsilon prevPMDL smallestPMDLDouble
                        then do
                            let optimalBlock = blockLines
                            saveOptimalBlock copyConfig optimalBlock distribution optimalBranchLength
                            putStrLn "Convergence achieved."
                        else do
                            saveOptimalBlock copyConfig blockLines distribution optimalBranchLength
                            loopProcess (map show newNumbersList) smallestPMDLDouble blockmodel copyConfig datapath naming distribution blockLines

optimizeBlock :: FilePath -> String -> String -> String -> [String] -> IO ()
optimizeBlock copyConfig datapath naming distribution blockLines = do
    let maybeBlockModel = extractBlockModel blockLines
    case maybeBlockModel of
        Nothing -> putStrLn "Error: Unable to extract block model." >> return ()
        Just blockmodel -> do
            let initialNewNumbers = if distribution == "exponential" --need to do caps/misspellings
                                     then ["0.5", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "20", "30", "40", "50", "60", "70", "80", "90", "100"]
                                     else ["0.01", "0.011", "0.0125", "0.01429", "0.0166", "0.02", "0.025", "0.033", "0.05", "0.1", "0.111", "0.125", "0.1429", "0.166", "0.2", "0.25", "0.33", "0.5", "1", "2"]

            loopProcess initialNewNumbers 1e18 blockmodel copyConfig datapath naming distribution blockLines

main :: IO ()
main = do
    args <- getArgs
    let (config, datapath, naming, distribution) = processArgs args

    let copyConfig = naming ++ "_working.config" --makes a copy of the original config file so a person can use the original multiple times at once
    configExists <- doesFileExist config
    if configExists
        then copyFile config copyConfig
        else error $ "Config file does not exist: " ++ config

    contents <- readFile config
    let blocks = splitIntoBlocks1 (lines contents) "BlockModel"

    forM_ blocks $ \block -> do
        putStrLn "Optimizing: "
        optimizeBlock copyConfig datapath naming distribution block
        let writing = "new block:"
        let filename = "PMDL" ++ naming ++ ".txt"
        appendFile filename (writing ++ "\n")
        putStrLn "Optimized."
