{-
Parses a simgle file with the FEParser.
Supports output to stdout with --to-stdout, allowing for #incext in Event Assembler.
-}

import System.Environment (getArgs)
import System.IO (stdout, hFlush)
import FEParser (parseFileWithDefinitions, parseDefinitions)
import FlagUtilities
import Data.ByteString (writeFile, hPut)
import Control.Monad (foldM)
import Data.Char (chr)
import Prelude hiding (writeFile)
import GBAUtilities (stripExtension, makeError)

main::IO ()
main = do
    args <- getArgs
    let options = getOptions args
    let params = getUnflaggedParams args
    let toStdOut = elem "--to-stdout" options

    if elem "--help" options
    then putStr $ makeError toStdOut "Usage: ./ParseFile <filename.txt> [-o <outputfile.dmp>] [--to-stdout] [--help]"
    else if length params /= 1
    then putStr $ makeError toStdOut "Incorrect number of parameters. Use ./ParseFile --help for usage."
    else if elem "-o" args && getParamAfterFlag "-o" args == Nothing
    then putStr $ makeError toStdOut "No output file specified."
    else do
    let inputFileName = head params

    parsedFile <- parseDefinitions (not toStdOut) >>= parseFileWithDefinitions inputFileName
    
    if elem "-o" args
    then case getParamAfterFlag "-o" args of 
        Just name -> case parsedFile of
            Left result -> writeFile name result
            Right (lineNum, err) -> return()
        Nothing -> return ()
    else return ()
    
    if toStdOut
    then case parsedFile of
        Left result -> hPut stdout result
        Right err -> putStr $ makeError toStdOut ("Error parsing file " ++ inputFileName ++ ", line " ++ show (fst err) ++ ": " ++ (snd err))
    else do 
        if not (elem "-o" args) 
        then case parsedFile of
            Left result -> writeFile (stripExtension inputFileName ++ ".dmp") result
            Right (lineNum, err) ->
                putStrLn ("Error on line " ++ show lineNum ++ ": " ++ err)
        else return ()
        putStrLn ("Finished parsing " ++ inputFileName ++ ".")
    
    -- putStr "Complete. Press enter to continue."
    -- hFlush stdout
    -- getLine
    
    return ()
