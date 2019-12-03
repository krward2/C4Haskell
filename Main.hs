import Board
import System.IO

module Main where
	
	-readSlot::String -> IO ()
	readSlot bd p = do i<-getChar
			dropInSlot bd i p
	