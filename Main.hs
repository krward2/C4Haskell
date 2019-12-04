--Author: Kenneth Ward

module Main where
	import Board
	import System.IO

	readSlot bd p = do 
			i<-getLine
			case reads i :: [(Int,String)] of
				[(n, "")] -> putStr(boardToStr playerToChar (dropInSlot bd n p))
				_ -> putStr("invalid input")
	
	startBoard = mkBoard 6 7
	main::IO ()
	main = do
		readSlot startBoard 1
		main
