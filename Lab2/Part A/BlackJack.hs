{- Task 3.2
size hand2
= size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
= 1 + size (Add (Card Jack Spades) Empty)
= 1 + 1 + size Empty
= 1 + 1 + 0
= 2
-}

module BlackJack where
  import Cards
  import Wrapper

  -- Returns an empty hand
  empty :: Hand
  empty = Empty

  -- Retrieves the value of a blackjack cardrank.
  valueRank :: Rank -> Integer
  valueRank (Numeric r) = r
  valueRank Ace         = 11
  valueRank _           = 10

  -- Retrieves the value of a blackjack card.
  valueCard :: Card -> Integer
  valueCard (Card r _)  = valueRank r

  -- Counts the number of aces in a hand.
  numberOfAces :: Hand -> Integer
  numberOfAces Empty           = 0
  numberOfAces (Add card hand) | rank card == Ace = 1 + numberOfAces hand
                               | otherwise        = numberOfAces hand

  -- Calculates the maximum value of a blackjack hand.
  -- All aces are considered to have value 11 in this function.
  maxValue :: Hand -> Integer
  maxValue Empty            = 0
  maxValue (Add card hand)  = valueCard card + maxValue hand

  -- Calculates the total value of a blackjack hand.
  -- When the maxValue of a hand is > 21, any aces will be valued 1 instead.
  value :: Hand -> Integer
  value hand | maxVal > 21  = maxVal - (10 * numberOfAces hand)
             | otherwise    = maxVal
             where
               maxVal = maxValue hand

  -- Checks if a hand is bust. I.e. if the value of the hand is > 21.
  gameOver :: Hand -> Bool
  gameOver hand = value hand > 21

  -- Returns the blackjack winner of a guesthand and a bankhand.
  winner :: Hand -> Hand -> Player
  winner guestHand bankHand | gameOver guestHand                = Bank
                            | gameOver bankHand                 = Guest
                            | value bankHand >= value guestHand = Bank
                            | otherwise                         = Guest
