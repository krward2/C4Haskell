module Board where

	mkBoard::Int->Int->[[Int]]
	mkBoard height width = replicate width (replicate height 0)

	mkPlayer = 1
	mkOpponent = 2
	
	isFull::[[Int]]->Bool
	isFull bd = notElem 0 (concat bd)

	numSlot:: [[Int]]->Int
	numSlot bd = length bd

	getColumn::[[Int]]->Int->[Int]
	getColumn bd i = last (take i bd)

	
	isSlotOpen::[[Int]]->Int->Bool
	isSlotOpen bd i = elem 0 (last (take i bd) )
	
	insertToken::[Int]->Int->[Int]
	insertToken (empty:full) p = ([x|x<-full,x==0]) ++ (p:[x|x<-full, x /= 0])

	insertCol::[[Int]]->[Int]->Int->[[Int]]
	insertCol (bd) col i = (take (i-1) bd) ++ (col:(drop (i+1) bd))

	dropInSlot::[[Int]]->Int->Int->[[Int]]
	dropInSlot bd i p
		|isSlotOpen bd i = insertCol (bd) (insertToken(getColumn bd i) p) (i) 
		|otherwise = bd
		
	--Rotates the board 90 degrees
	horizontalBd::[[Int]]->[[Int]]
	horizontalBd [] = []
	horizontalBd ([]:_) = []
	horizontalBd bd = map last bd : (horizontalBd (map init bd))


	--Doesnt account for order! :(
	--Broken
	isWinningLine::[Int]->Int->Bool
	isWinningLine line p = ( length (filter (==p) line) ) == 4

	--leftDiag bd =  rightDiag (reverse bd)
	
	--Need to add r/l diagonal win check
	--Broken
	isWonBy::[[Int]]->Int->Bool
	isWonBy bd p = (length ([col|col<-bd, isWinningLine col p]) == 1) || 
			(length ([row|row<-(horizontalBd bd), isWinningLine row p]) == 1)

	--isWonBy::[[Int]]->Int->Bool
	--isWonBy bd p = (length (filter (isWinningLine () p) bd) ) == 1

	playerToChar::

	boardToStr::[[Int]]->[[String]]

	