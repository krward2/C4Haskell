--Author: Kenneth Ward

module Board where
	--Column based representation of the board.
	mkBoard::Int->Int->[[Int]]
	mkBoard columns rows = replicate columns (replicate rows 0)

	mkPlayer = 1
	mkOpponent = 2
	
	--True if there are no 0's in bd
	isFull::[[Int]]->Bool
	isFull bd = notElem 0 (concat bd)

	numSlot:: [[Int]]->Int
	numSlot bd = length bd

	--Gets the ith column of a board
	getColumn::[[Int]]->Int->[Int]
	getColumn bd i = last (take i bd)

	--True if there are 0's in the ith column
	isSlotOpen::[[Int]]->Int->Bool
	isSlotOpen bd i = elem 0 (getColumn bd i)
	
	--Drops a token in a column
	insertToken::[Int]->Int->[Int]
	insertToken (empty:full) p = ([x|x<-full,x==0]) ++ (p:[x|x<-full, x /= 0])

	--Replaces ith column with col
	insertCol::[[Int]]->[Int]->Int->[[Int]]
	insertCol (bd) col i = (take (i-1) bd) ++ (col:(drop i bd))

	--If slot is open, drops token in the ith column, and inserts updated column into board
	dropInSlot::[[Int]]->Int->Int->[[Int]]
	dropInSlot bd i p
		|isSlotOpen bd i = insertCol (bd) (insertToken(getColumn bd i) p) (i) 
		|otherwise = bd
		
	--Gives a row based representation of the board
	horizontal::[[a]]->[[a]]
	horizontal [] = []
	horizontal ([]:_) = []
	horizontal bd = map last bd : (horizontal (map init bd))

	--Checks if there are 4 player pieces in a column
	isWinningLine::[Int]->Int->Bool
	isWinningLine line p = ( length (filter (==p) line) ) == 4

	--leftDiag bd =  rightDiag (reverse bd)

	--Checks if there is at least 1 column or row that satisfies isWinningLine predicate
	isWonBy::[[Int]]->Int->Bool
	isWonBy bd p = (length ([col|col<-bd, isWinningLine col p]) == 1) || 
			(length ([row|row<-(horizontal bd), isWinningLine row p]) == 1)
	
	--Checks for the number of rows
	numRows::[[Int]]->Int
	numRows bd = length ( horizontal bd )
	
	--Gets first row of a column oriented board
	getFirstRow::[[Int]]->[Int]
	getFirstRow bd = map head bd

	--Removes the first row of a column based board
	cutFirstRow::[[Int]]->[[Int]]
	cutFirstRow bd = map tail bd
	
	playerToChar::Int->Char
	playerToChar p
		|p == 1 = 'O'
		|p == 2 = 'X'
		|otherwise = '.'

	--Stringifies top row, adds next line character to the end, repeats the process on the rest of the board
	boardToStr::(Int->Char)->[[Int]]->String
	boardToStr playerToChar bd
		|numRows bd == 1 = (map playerToChar (getFirstRow bd))++['\n']
		|otherwise = ((map playerToChar (getFirstRow bd))++['\n']) ++ (boardToStr playerToChar (cutFirstRow bd))
	
	
	