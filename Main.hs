--Author: Kenneth Ward

module Main where
	import Board
	import System.IO

	--Toggles between players
	nextPlayer p
		|p == 1 = 2
		|p == 2 = 1
		|otherwise = 1
	
	--Gets IO version of index/input
	getAction i = case reads i :: [(Int, String)] of
			[(n, "")] -> return (n)
			_ -> return(-1)

	--Gets IO version of board
	getBoard bd = return bd

	--Gets IO version of new board
	getNewBoard bd i p = return(dropInSlot bd i p)

	--The workhorse. Takes user input, decides result of the input, and displays the result
	readSlot bd p = do 
			l<-getLine
			i<-getAction l	
			newBd<-getNewBoard bd i p
			if(i<1||i>7) then putStr("Game quit.")
			else if(isWonBy newBd p) then putStr(boardToStr playerToChar newBd) >> putStr("Game won!")
			else if(isFull newBd) then putStr(boardToStr playerToChar newBd) >> putStr("Draw!")					
			else putStr(boardToStr playerToChar newBd ) >> readSlot newBd (nextPlayer p)
	

	--Prints instructions, and initiates the workhorse.
	main = do
		putStr("Enter a number 1-7 to drop a token in the corresponding column.\nEnter anything else to exit.\n")
		readSlot (mkBoard 6 7) 1
