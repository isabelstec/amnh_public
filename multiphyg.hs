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
import Text.Regex.Posix

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
    | prefix `isPrefixOf` line = prefix ++ distribution ++ ":" ++ newNumber ++ ";"
    | otherwise = line
  where
    prefix = "\tBranchLength : "

processBlock :: String -> String -> [String] -> [String]
processBlock distribution newNumber = map (branchLengthUpdate distribution newNumber)

splitIntoBlocks1 :: [String] -> String -> [[String]]  --with header
splitIntoBlocks1 lines blockStartMarker = go lines []
  where
    go [] currentBlock = [currentBlock | not (null currentBlock)]
    go (line:rest) currentBlock
        | blockStartMarker `isInfixOf` line = 
            currentBlock : go rest [line]
        | otherwise = 
            go rest (currentBlock ++ [line])

splitIntoBlocks2 :: [String] -> String -> [[String]]    --without header
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

processArgs :: [String] -> (String, String, String, String, Maybe String)
processArgs inArgs =
    case length inArgs of
        4 -> let [config, datapath, naming, distribution] = inArgs
             in (config, datapath, naming, distribution, Nothing)
        5 -> let [config, datapath, naming, distribution, phygFile] = inArgs
             in (config, datapath, naming, distribution, Just phygFile)
        _ -> errorWithoutStackTrace "Require four or five arguments: configFile dataPath stub distribution and/or phygfile"

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

{-extractIdentifier :: String -> String
extractIdentifier filename =
    let baseName = takeBaseName filename 
        (_, identifier) = break (\c -> c == '-' || c == '.') baseName  
    in tail identifier -}

{-extractIdentifier :: String -> IO String 
extractIdentifier filename = do 
    print("filename", filename)
    let baseName = takeBaseName filename
    print("baseName", baseName)
    let (_, identifier) = break (\c -> c == '-' || c == '.') baseName
    print("identifier", identifier)
    --let result = tail identifier
    --print( "tail identifier", result)
    return baseName-}

splitOnHyphen :: String -> [String]
splitOnHyphen [] = [""]
splitOnHyphen s = case break (== '-') s of 
    (w, "") -> [w]
    (w, _:s') -> w : splitOnHyphen s'

extractIdentifier :: String -> IO String
extractIdentifier filename = do
    let basename = takeBaseName filename
        parts = case dropWhile (== '-') basename of 
            "" -> [basename]
            s' -> splitOnHyphen s' 
        identifier = last parts
    return identifier

extractExact :: String -> String 
extractExact line = 
    if "BlockModel" `isPrefixOf` line
        then dropWhile (== ' ') . takeWhile (/= ' ') . drop (length "BlockModel ") $ line
        else ""

findMatchingBlock :: [[String]] -> String -> Maybe [String]
findMatchingBlock blocks identifier = 
    let pattern = "^[0-9]*" ++ identifier ++ "\\.neyModel$"    --hard coded for now but need to work a way around it
        matchesPattern block = any (\line -> extractExact line =~ pattern) block
    in find matchesPattern blocks


updateBranchLengthIfMatch :: [String] -> String -> String -> [String] -> [String]
updateBranchLengthIfMatch optimalBlock distribution newBranchLength block =
    if block == optimalBlock 
        then map (updateBranchLengthLine distribution newBranchLength) block
        else block


updateBranchLengthLine :: String -> String -> String -> String
updateBranchLengthLine distribution newBranchLength line
    | "\tBranchLength : " `isPrefixOf` line =
        let prefix = "\tBranchLength : "
            updatedLine = prefix ++ distribution ++ ":" ++ newBranchLength ++ ";"
        in trace ("Updating line: " ++ line ++ " to " ++ updatedLine) updatedLine
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


complexAndPhyg :: String -> String -> String -> String -> String -> [String] -> Maybe FilePath -> String -> IO [FilePath]
complexAndPhyg newNumber config datapath naming distribution blockLines maybePgFile currentPhygFile = do
    let outputFile = naming ++ ".config"

    configContents <- readFile config
    let blocks = splitIntoBlocks1 (lines configContents) "BlockModel"

    currentFile <- extractIdentifier currentPhygFile
    --putStrLn $ "Identifier for file: " ++ currentFile

    let maybeCurrentBlock = findMatchingBlock blocks currentFile
    case maybeCurrentBlock of
        Nothing -> error $ "No matching BlockModel found for: " ++ currentPhygFile
        Just currentBlock -> do
            let modifiedBlock = processBlock distribution newNumber currentBlock
            let updatedBlocks = map (\b -> if b == currentBlock then modifiedBlock else b) blocks

            let tempFile = config ++ ".tmp"
            modifyConfigFile tempFile updatedBlocks
            renameFile tempFile config 

            maybeComplexityPath <- findExecutable "phyloComplexity"
            complexityPath <- case maybeComplexityPath of
                Just path -> return path
                Nothing -> die "phyloComplexity executable not found in PATH"
            let stub = naming ++ "-" ++ newNumber
            callProcess complexityPath [config, stub]

            let complexityFile = stub ++ ".complexity"
            complexityContents <- readFile complexityFile
            let firstLine = head (lines complexityContents)
            let justComplexity = getNumbers firstLine

            let datafileIdentifier = takeBaseName currentPhygFile
            let newFile = naming ++ "-" ++ datafileIdentifier ++ "-" ++ newNumber ++ ".pg"
            withComplexity <- openFile newFile WriteMode
            let initialContent = "set(criterion:PMDL)\nset(modelComplexity:" ++ justComplexity ++ ")\n"
            hPutStr withComplexity initialContent

            case extractBlockModel currentBlock of
                Just blockModel -> do
                    let readline = "read(\"" ++ datapath ++ "/" ++ currentPhygFile ++ "\", tcm: \"./" ++ stub ++ blockModel ++ ".bit.tcm\")\n"
                    hPutStr withComplexity readline
                Nothing -> error "Failed to extract block model."

            case maybePgFile of
                Just pgFilePath -> do
                    pgContents <- readFile pgFilePath
                    hPutStr withComplexity pgContents
                    hPutStr withComplexity ("\nreport(\"./" ++ newFile ++ ".dot\", graphs, dotpdf, overwrite)")
                Nothing -> do
                    hPutStr withComplexity ("build(distance, rdwag)\nreport(\"./" ++ newFile ++ ".dot\", graphs, dotpdf, overwrite)")
            hClose withComplexity
            return [newFile]


loopProcess :: [String] -> Double -> String -> FilePath -> String -> String -> String -> [String] -> Maybe FilePath -> IO ()
loopProcess initialNewNumbers _ blockmodel copyConfig datapath naming distribution blockLines maybePgFile = do
    dataFiles <- processFilesInDirectory datapath
    forM_ dataFiles $ \dataFile -> do
        putStrLn $ "Processing data file: " ++ dataFile
        let newNumbers = initialNewNumbers
        let prevPMDL = 1e18
        let processFile numbers pmdl = do
                results <- forM numbers $ \newNumber -> do
                    phygFiles <- complexAndPhyg newNumber copyConfig datapath naming distribution blockLines maybePgFile dataFile
                    forM phygFiles $ \phygFile -> do
                        maybePhygPath <- findExecutable "phyg"
                        phygPath <- case maybePhygPath of
                            Just path -> return path
                            Nothing -> die "phyg executable not found in PATH"
                        callProcess phygPath [phygFile]

                        let dotfile = phygFile ++ ".dot"
                        lastLine <- getLastLine dotfile
                        case lastLine of
                            Nothing -> do
                                putStrLn $ "Error: No output in dot file for " ++ phygFile
                                return (newNumber, "") 
                            Just line -> do
                                let pmdl = getNumbers line
                                let writing = "BranchLength: " ++ phygFile ++ " " ++ newNumber ++ " with a PMDL of: " ++ pmdl
                                let filename = "PMDL" ++ naming ++ ".txt"
                                appendFile filename (writing ++ "\n")
                                return (newNumber, pmdl)

                let allResults = concat results
                let pmdlValues = map snd allResults
                let smallestPMDL = minimum pmdlValues
                let smallestPMDLValue = read smallestPMDL :: Double
                let filteredResults = filter ((== smallestPMDL) . snd) allResults

                case filteredResults of
                    [] -> putStrLn "Error: No results found for the smallest PMDL." >> return ()
                    _ -> do
                        let (optimalBranchLength, _) = head filteredResults
                        let indexSmallest = fromJust $ elemIndex optimalBranchLength (map fst allResults)
                        let floatList = map read (map fst allResults) :: [Double]
                        let (left, right, _) = valuesAroundSmallest floatList indexSmallest

                        if isNothing left || isNothing right
                            then putStrLn "Error: Left or right branch length is Nothing." >> return ()
                            else do
                                let newNumbersList = valuesBetween (fromJust left) (fromJust right) (read optimalBranchLength :: Double)
                                if compareWithEpsilon pmdl smallestPMDLValue
                                    then do
                                        saveOptimalBlock copyConfig blockLines distribution optimalBranchLength
                                        putStrLn $ "Convergence achieved for data file: " ++ dataFile
                                        appendFile ("PMDL" ++ naming ++ ".txt") $ "Optimal block for " ++ dataFile ++ ": " ++ optimalBranchLength ++ "\n"
                                    else do
                                        saveOptimalBlock copyConfig blockLines distribution optimalBranchLength
                                        putStrLn $ "Continuing optimization for data file: " ++ dataFile ++ " with new numbers: " ++ show newNumbersList
                                        processFile (map show newNumbersList) smallestPMDLValue
        processFile newNumbers prevPMDL


optimizeBlock :: FilePath -> String -> String -> String -> [String] -> Maybe FilePath -> IO ()
optimizeBlock copyConfig datapath naming distribution blockLines maybePgFile = do
    let maybeBlockModel = extractBlockModel blockLines
    case maybeBlockModel of
        Nothing -> putStrLn "Error: Unable to extract block model." >> return ()
        Just blockmodel -> do
            let initialNewNumbers = if distribution == "exponential" --will have it so it takes any spelling of exponential
                                     then ["0.5", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "20", "30", "40", "50", "60", "70", "80", "90", "100"]
                                     else ["0.01", "0.011", "0.0125", "0.01429", "0.0166", "0.02", "0.025", "0.033", "0.05", "0.1", "0.111", "0.125", "0.1429", "0.166", "0.2", "0.25", "0.33", "0.5", "1", "2"]

            loopProcess initialNewNumbers 1e18 blockmodel copyConfig datapath naming distribution blockLines maybePgFile


main :: IO ()
main = do
    args <- getArgs
    let (config, datapath, naming, distribution, maybePgFile) = processArgs args
    let copyConfig = naming ++ "_working.config"
    configExists <- doesFileExist config
    if configExists
        then copyFile config copyConfig
        else error $ "Config file does not exist: " ++ config
    contents <- readFile config
    let blocks = splitIntoBlocks1 (lines contents) "BlockModel"
    forM_ blocks $ \block -> do
        putStrLn "Optimizing: "
        optimizeBlock copyConfig datapath naming distribution block maybePgFile
        let filename = "PMDL" ++ naming ++ ".txt"
        appendFile filename "new block:\n"
        putStrLn "Optimized."

-- ./fileName configFile dataPath stub distribution possiblePhyGFile

-- ./multiphyg IE-lang-Neyman.config ./IE-fastc/fasta stub exponential indfiles.pg
