module Board where

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
		| i < 1 = False
		| i <= numSlot bd = not (isFull (getColumn bd i))
	
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