module Testing where
	--mkEmptyCol::Int->[Int]
	--mkEmptyCol 0 = []
	--mkEmptyCol length =
	--	0 : mkEmptyCol (length-1)
	
	mkEmptyCol::Int->[Int]
	mkEmptyCol length
		|length == 1 = [0]
		|otherwise = 0 : mkEmptyCol (length-1)

	mkBoard::Int->Int->[[Int]]
	mkBoard m n
		|n == 1 = [mkEmptyCol m]
		|otherwise = mkEmptyCol m : (mkBoard (m) (n-1))

	mkPlayer = 1
	mkOpponent = 2

	isFull::[Int]->Bool
	isFull (h:t)
		|h /= 0 = True
		|otherwise = False

	numSlot:: [[Int]]->Int
	numSlot bd = length bd

	getColumn::[[Int]]->Int->[Int]
	getColumn bd i
		|numSlot bd == 1 = head bd
		|otherwise = head(reverse(take i bd))

	--Can't handle -1
	isSlotOpen::[[Int]]->Int->Bool
	isSlotOpen bd i
		| i < 1 = False
		| i <= numSlot bd = not (isFull (getColumn bd i))
	
	--board = mkBoard 7 7
	--isOpen = [isSlotOpen board 1, isSlotOpen board 2, isSlotOpen board 3, isSlotOpen board 4, isSlotOpen board 5, isSlotOpen board 6, isSlotOpen board 7]
	
	--splitCol::[Int]->[Int]
	--splitCol(empty++full) = ()++()

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


	--a = insertToken [0,0,0,0,1] 2
	--ab = insertToken a 2
	--b = insertCol [[0],[0],[0]] [1] 1
	
	--bd = mkBoard 7 7
	--bd3 = dropInSlot bd 1 1
	--bd4 = dropInSlot bd3 1 1
	

	--testDIS::[[Int]]->Int->Int->Int->[[Int]]
	--testDIS bd i p n
	--	|n == 1 = dropInSlot bd i p 
	--	|otherwise = testDIS bd i p (n-1)
	
	--bd2 = testDIS bd 1 1 7

	--       1  2
	--     \ | /
	-- _____\|/_____3
	--      /|\
	--     / | \
                    4
	rotateBoard::[[Int]]->[[Int]]
	
	horizontalBoard::[[Int]]->[[Int]]
	horizontalBoard (h:t) = concat t

	isWonBy::[[Int]]->Int->Bool
	isWonBy bd p
		length t == 1 && (length [x|x<-(head t), x==p]) == 4 = True
	

		
	
	
