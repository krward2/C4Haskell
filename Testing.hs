module Testing where
	
	mkBoard::Int->Int->[[Int]]
	mkBoard height width = replicate width (replicate height 0)
	
	mkPlayer = 1
	mkOpponent = 2

	--Improved
	isFull::[Int]->Bool
	isFull col = any (/=0) col

	--Humor
	isBoardFull::[[Int]]->Bool
	isBoardFull bd = notElem 0 (concat bd)

	numSlot:: [[Int]]->Int
	numSlot bd = length bd

	getColumn::[[Int]]->Int->[Int]
	getColumn bd i
		|numSlot bd == 1 = head bd
		|otherwise = head(reverse(take i bd))

	--Can't handle -1
	--isSlotOpen::[[Int]]->Int->Bool
	--isSlotOpen bd i
	--	| i < 1 = False
	--	| i <= numSlot bd = not (isFull (getColumn bd i))
	
	isSlotOpen::[[Int]]->Int->Bool
	isSlotOpen bd i = any (==0) (last (take i bd) )

	insertToken::[Int]->Int->[Int]
	insertToken (empty:full) p = ([x|x<-full,x==0]) ++ (p:[x|x<-full, x /= 0])

	insertCol::[[Int]]->[Int]->Int->[[Int]]
	insertCol (h:t) col i
		|i == 1 = col:t
		|otherwise = h:(insertCol t col (i-1)) 

	dropInSlot::[[Int]]->Int->Int->[[Int]]
	dropInSlot bd i p
		|isSlotOpen bd i = insertCol (bd) (insertToken(getColumn bd i) p) (i) 
		|otherwise = bd
	--       1  2
	--     \ | /
	-- _____\|/_____3
	--      /|\
	--     / | \
        --          4
	
	rebuild::[Int]->[[Int]]
	rebuild (h:t)
		|length t == 1 = [h]:[t]
		|otherwise = [h]:rebuild t

	atomizeBoard::[[Int]]->[[Int]]
	atomizeBoard bd = rebuild (concat bd)
	
	horizontal::[[Int]]->[[Int]]
	horizontal [] = []
	horizontal ([]:_) = []
	horizontal bd = map last bd : (horizontal (map init bd))
	
	--horizontal::[[Int]]->[[Int]]
	--horizontal

	--rightDiagonal::[[Int]]->[[Int]]

	--leftDiagonal::[[Int]]->[[Int]]
	--leftDiagonal bd = rightDiagonal( reverse bd )

	--isWinningLine::[Int]->Bool
	--isWinningLine line p 
	--	|length [tokens|tokens<-line, tokens==p] == 4 = True
	--	|otherwise = False

	--isWonBy::[[Int]]->Int->Bool
	--isWonBy bd p
	--	|length [col|col<-bd, isWinningLine col p] == 1 = True
	--	|length [row|row<-(horizontal bd), isWinningLine row p] == 1 = True
	--	|length [diag|diag<-(rightDiagonal bd), isWinningLine diag p] == 1 = True
	--	|length [diag|diag<-(leftDiagonal bd), isWinningLine diag p] == 1 = True	
	--	|otherwise = False
	
	--playerToChar::[Int]->[String]
	--boardToStr::()->[[Int]]->[[String]]
	--boardToStr playerToChar bd
	