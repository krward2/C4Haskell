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
