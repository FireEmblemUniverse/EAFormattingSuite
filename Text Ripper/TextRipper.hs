{-
Uses the unparser to rip every non-huffman-encoded text from an FE8 ROM.
-}

{-# LANGUAGE BangPatterns #-}

module Main where

import GBAUtilities (readWordAt, intToHex, hexToInt)

import FlagUtilities
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)
import Data.ByteString.Lazy (ByteString, unpack, snoc, readFile, writeFile, take, takeWhile, drop, copy)
import Data.ByteString as ByteString.Strict (pack, writeFile)
import Data.Foldable (toList)
import Control.Monad (foldM)
import Prelude hiding (take, takeWhile, drop, writeFile, readFile)
import qualified Prelude (take, writeFile)
import Data.Word
import Data.Bits
import Data.List (foldl')
import FEParser (unparse)

x <-$ f = f x
infixl 0 <-$

data Game = FE6 | FE7 | FE8

getTextTableReference FE6 = undefined
getTextTableReference FE7 = undefined
getTextTableReference FE8 = 0xA2A0

textTableReference = 0xA2A0
hardwareOffset = 0x08000000

main::IO ()
main = do
    args <- getArgs
    if null args || hasFlag args "--help"
    then putStrLn "Ripping a single text ID: " >> 
         putStrLn "./TextRipper ROMname.gba textID" >> putStrLn [] >>
         putStrLn "Ripping multiple texts in a row: " >> 
         putStrLn "./TextRipper ROMname.gba --section -start startNumber -stop stopNumber"
    else do
    romDat <- readFile (head args)
    createDirectoryIfMissing True "./Ripped Text Data"
    if hasFlag args "--section"
    then let startNum = getParamAfterFlag "-start" args <-$ fmap hexToInt::Maybe Int
             stopNum = getParamAfterFlag "-stop" args <-$ fmap hexToInt::Maybe Int
         in case startNum of
                Just start -> case stopNum of
                    Just stop -> do
                        let textTableOffset = readWordAt romDat textTableReference - hardwareOffset
                        extracted <- ripStartStop romDat start stop
                        let (pointersText, dataText) = makeEAFiles extracted textTableOffset
                        Prelude.writeFile "Install Ripped Text Pointers.txt" pointersText
                        Prelude.writeFile "Install Ripped Text Data.txt" dataText
                        return ()
                    Nothing -> putStrLn "No -stop number given."
                Nothing -> putStrLn "No -start number given."
    else let params = getParams args in 
        if length params /= 2 
        then putStrLn "Incorrect number of parameters given."
        else ripSingle romDat (hexToInt . head . tail $ args) >> return ()

readSingle::ByteString->Int->Maybe ByteString
readSingle romData textID = do
    let textTableOffset = readWordAt romData textTableReference - hardwareOffset
    let textEntryOffset = 4*textID + textTableOffset
    let textOffset = readWordAt romData textEntryOffset
    case shift textOffset (-0x1F) of
        1 -> let textOffsetROM = textOffset .&. 0xFFFFFF
             in Just $ flip snoc 0x0 . copy $ (takeWhile (/= 0x00) (drop (fromIntegral textOffsetROM) romData))
        0 -> Nothing

-- readTextTableLocation::String->IO Int
-- readTextTableLocation rom = readFile romName >>= (- hardwareOffset) . flip readWordAt textTableReference 

ripSingle::ByteString->Int->IO (Maybe Int)
ripSingle romDat textID = let textIDhex = prepadWithToLength (intToHex textID) '0' 4 in do
    case readSingle romDat textID of
        Just dat -> let !strict = {-# SCC bytestring_convert #-} (ByteString.Strict.pack . unpack $ dat)
                        !text = unparse strict 
                    in ByteString.Strict.writeFile ("Ripped Text Data/"++ textIDhex ++ ".dmp") strict >> Prelude.writeFile ("Ripped Text Data/"++ textIDhex ++".txt") text >> return (Just textID)
        Nothing -> {-putStrLn ("Text ID " ++ textIDhex ++ " not anti-huffman! Will be implemented in the future (maybe).") >> -}return Nothing


--readStartStop::String->Int->Int->IO [Maybe ByteString]
--readStartStop romName start stop = sequence $ map (ripSingle romName) [start..stop]

ripStartStop::ByteString->Int->Int->IO [Int]
ripStartStop romDat start stop = fmap filterJust . mapM (ripSingle romDat) $ [start..stop]

filterJust::[Maybe a]->[a]
filterJust [] = []
filterJust (Just x:xs) = x:filterJust xs
filterJust (Nothing:xs) = filterJust xs


makeEAFiles::[Int]->Int->(String, String)
makeEAFiles textIDs textTableLocation = foldl' (\acc->(\e-> 
    let (!ptrs, !dat) = acc 
        !hex = prepadWithToLength (intToHex e) '0' 4
    in (ptrs++"setText(0x"++hex++", text_data_"++hex++")\n", dat++"text_data_"++hex++":\n#incbin \"Ripped Text Data/"++hex++".dmp\"\n"))) ("#ifndef TextTable\n\t#define TextTable 0x15D48C\n#endif\n#define setText(textID, offset) \"ORG (TextTable+4*textID); POIN (offset | 0x80000000)\"\n\n", "") textIDs

prepadWithToLength str chr len = if length str < len then Prelude.take (len - length str) (repeat chr) ++ str else str
