module Board where

	mkBoard::Int->Int->[[Int]]
	mkBoard height width = replicate width (replicate height 0)

	mkPlayer = 1
	mkOpponent = 2
	
	--Consider making isFull bd?
	isFull::[Int]->Bool
	isFull col = any (/=0) col

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