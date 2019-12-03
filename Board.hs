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
	insertCol (bd) col i = (take (i-1) bd) ++ (col:(drop i bd))

	dropInSlot::[[Int]]->Int->Int->[[Int]]
	dropInSlot bd i p
		|isSlotOpen bd i = insertCol (bd) (insertToken(getColumn bd i) p) (i) 
		|otherwise = bd
		
	--Rotates the board 90 degrees
	horizontal::[[a]]->[[a]]
	horizontal [] = []
	horizontal ([]:_) = []
	horizontal bd = map last bd : (horizontal (map init bd))


	--Doesnt account for order! :(
	--Broken
	isWinningLine::[Int]->Int->Bool
	isWinningLine line p = ( length (filter (==p) line) ) == 4

	--leftDiag bd =  rightDiag (reverse bd)
	
	--Need to add r/l diagonal win check
	--Broken
	isWonBy::[[Int]]->Int->Bool
	isWonBy bd p = (length ([col|col<-bd, isWinningLine col p]) == 1) || 
			(length ([row|row<-(horizontal bd), isWinningLine row p]) == 1)

	--isWonBy::[[Int]]->Int->Bool
	--isWonBy bd p = (length (filter (isWinningLine () p) bd) ) == 1]
	
	numRows::[[Int]]->Int
	numRows bd = length ( horizontal bd )
	
	getFirstRow::[[Int]]->[Int]
	getFirstRow bd = map head bd

	cutFirstRow::[[Int]]->[[Int]]
	cutFirstRow bd = map tail bd
	
	playerToChar::Int->Char
	playerToChar p
		|p == 1 = 'O'
		|p == 2 = 'X'
		|otherwise = '.'

	boardToStr::(Int->Char)->[[Int]]->String
	boardToStr playerToChar bd
		|numRows bd == 1 = (map playerToChar (getFirstRow bd))++['\n']
		|otherwise = ((map playerToChar (getFirstRow bd))++['\n']) ++ (boardToStr playerToChar (cutFirstRow bd))
	
	
	