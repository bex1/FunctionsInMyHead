module BlackJack where
  import Cards
  import Wrapper
  import System.Random
  import Test.QuickCheck hiding (shuffle)

  -- Returns an empty hand
  empty :: Hand
  empty = Empty

  -- Retrieves the value of a blackjack cardrank.
  valueRank :: Rank -> Integer
  valueRank (Numeric rank) = rank
  valueRank Ace            = 11
  valueRank _              = 10

  -- Retrieves the value of a blackjack card.
  valueCard :: Card -> Integer
  valueCard (Card rank _)  = valueRank rank

  -- Counts the number of aces in a hand.
  numberOfAces :: Hand -> Integer
  numberOfAces Empty           = 0
  numberOfAces (Add card hand) | rank card == Ace = 1 + numberOfAces hand
                               | otherwise        = numberOfAces hand

  -- Calculates the maximum value of a blackjack hand.
  -- All aces are considered to have value 11 in this function.
  value11 :: Hand -> Integer
  value11 Empty            = 0
  value11 (Add card hand)  = valueCard card + value11 hand

  -- Calculates the total value of a blackjack hand.
  -- When the value11 of a hand is > 21, any aces will be valued 1 instead.
  value :: Hand -> Integer
  value hand | maxVal > 21  = maxVal - (10 * numberOfAces hand)
             | otherwise    = maxVal
             where
               maxVal = value11 hand

  -- Checks if a hand is bust. I.e. if the value of the hand is > 21.
  gameOver :: Hand -> Bool
  gameOver hand = value hand > 21

  -- Returns the blackjack winner of a guesthand and a bankhand.
  winner :: Hand -> Hand -> Player
  winner guestHand bankHand | gameOver guestHand                = Bank
                            | gameOver bankHand                 = Guest
                            | value bankHand >= value guestHand = Bank
                            | otherwise                         = Guest

  -- Part B

  (<+) :: Hand -> Hand -> Hand
  Empty         <+ hand2 = hand2
  Add card hand <+ hand2 = Add card (hand <+ hand2)

  prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
  prop_onTopOf_assoc h1 h2 h3 = h1 <+ (h2 <+ h3) == (h1 <+ h2) <+ h3

  prop_size_onTopOf :: Hand -> Hand -> Bool
  prop_size_onTopOf h1 h2 = size h1 + size h2 == size (h1 <+ h2)

  -- Returns a hand with all cards of a suit. 13 cards.
  fullSuit :: Suit -> Hand
  fullSuit s =
    foldr (<+) Empty cardList
    where cardList = [Add (Card(Numeric i) s) Empty | i <- [2..10]]
                     ++ [Add (Card p s) Empty | p <- [Jack, Queen, King, Ace]]

  -- Returns a full deck of cards. 52 cards.
  fullDeck :: Hand
  fullDeck =
    foldr (<+) Empty fullSuits
    where fullSuits = map fullSuit [Hearts, Spades, Diamonds, Clubs]

  -- Draws a card from a deck and puts it into the hand.
  -- Use: draw deck hand -> (hand + card, deck - card)
  -- Error raised upon empty deck.
  draw :: Hand -> Hand -> (Hand, Hand)
  draw Empty _ = error "draw: The deck is empty."
  draw (Add c d) h = (d, Add c h)

  playBank :: Hand -> Hand
  playBank deck = playBank' deck Empty

  playBank' :: Hand -> Hand -> Hand
  playBank' deck hand | value hand >= 16 = hand
                      | otherwise        = uncurry playBank' $ draw deck hand

  removeCard :: Integer -> Hand -> (Card, Hand)
  removeCard = removeCard' Empty

  removeCard' :: Hand -> Integer -> Hand -> (Card, Hand)
  removeCard' _ i h1 | i < 0 || h1 == Empty = error "removeCard': card not in hand"
  removeCard' h2 0 (Add card h1) = (card, h1 <+ h2)
  removeCard' h2 i (Add card h1) = removeCard' (Add card h2) (i-1) h1

  shuffle :: StdGen -> Hand -> Hand
  shuffle = shuffle' Empty

  shuffle' :: Hand -> StdGen -> Hand -> Hand
  shuffle' shuffled g Empty       = shuffled
  shuffle' shuffled g unShuffled  = shuffle' (Add card shuffled) g' remaining
    where
      (i, g')           = randomR (0, size unShuffled - 1) g
      (card, remaining) = removeCard i unShuffled

  belongsTo :: Card -> Hand -> Bool
  c `belongsTo` Empty      = False
  c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

  prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
  prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffle g h

  prop_size_shuffle :: StdGen -> Hand -> Bool
  prop_size_shuffle g h = size h == size (shuffle g h)

  implementation = Interface {
     iEmpty = empty
     , iFullDeck = fullDeck
     , iValue = value
     , iGameOver = gameOver
     , iWinner = winner
     , iDraw = draw
     , iPlayBank = playBank
     , iShuffle = shuffle
  }

  main :: IO ()
  main = runGame implementation
