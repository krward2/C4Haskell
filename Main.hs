module Main where
	import Board
	import System.IO
	
	--readSlot::String -> IO ()
	readSlot bd p = do 
			i<-getLine
			case reads i :: [(Int,String)] of
				[(n, "")] -> putStr(boardToStr playerToChar (dropInSlot bd n p))
				_ -> putStrLn "invalid input"
	--startPlayer = 1
	--startBoard = mkBoard 6 7
	main = do
		