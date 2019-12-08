--Author: Kenneth Ward

module Main where
	import Board
	import System.IO

	nextPlayer p
		|p == 1 = 2
		|p == 2 = 1
		|otherwise = 1

	readSlot bd p = do 
			i<-getLine
			if (isWonBy bd p) then putStr("Game won")
			else if (isFull bd) then putStr("Draw")
			else case reads i :: [(Int,String)] of
				[(n, "")] -> putStr(boardToStr playerToChar (dropInSlot bd n p))>>
						readSlot (dropInSlot bd n p) (nextPlayer p)
						
				_ -> putStr("invalid input")
	

	main::IO ()
	main = do
		readSlot (mkBoard 7 6) 1
