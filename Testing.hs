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

	isSlotOpen::[[Int]]->Int->Bool
	isSlotOpen bd i
		| i <= 1 = False
		| i <= numSlot bd = not (isFull (getColumn bd i))
	
	board = mkBoard 3 3
	isOpen = isSlotOpen board 3
	
