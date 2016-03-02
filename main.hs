{-# LANGUAGE TemplateHaskell #-}

import System.Environment (getArgs)
import Data.Function (fix)
import Template (generateDay)

import AdventOfCode

$(generateDay [1..25]) -- creates: day "day1" = print.day1 ... day "day25" = print.day25
main = getArgs >>= fix (\loop f -> getLines >>= f >> loop f).day.head
	where
	getLines :: IO [String]
	getLines = do
		x <- getLine
		if x == "" then
			return []
		else do
			xs <- getLines
			return (x:xs)

