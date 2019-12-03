module Testing where
	
	
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
	