module BlackJack where
  import Cards
  import Wrapper
  import System.Random
  import Test.QuickCheck hiding (shuffle)

  -- Playable blackjack session.
  main :: IO ()
  main = runGame implementation

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
  numberOfAces (Add card hand)
      | rank card == Ace = 1 + numberOfAces hand
      | otherwise        = numberOfAces hand

  -- Calculates the maximum value of a blackjack hand.
  -- All aces are considered to have value 11 in this function.
  value11 :: Hand -> Integer
  value11 Empty           = 0
  value11 (Add card hand) = valueCard card + value11 hand

  -- Calculates the total value of a blackjack hand.
  -- When the value11 of a hand is > 21, any aces will be valued 1 instead.
  value :: Hand -> Integer
  value hand
      | maxVal > 21 = maxVal - (10 * numberOfAces hand)
      | otherwise   = maxVal
    where
      maxVal = value11 hand

  -- Checks if a hand is bust. I.e. if the value of the hand is > 21.
  gameOver :: Hand -> Bool
  gameOver hand = value hand > 21

  -- Returns the blackjack winner of a guesthand and a bankhand.
  winner :: Hand -> Hand -> Player
  winner guestHand bankHand
      | gameOver guestHand                = Bank
      | gameOver bankHand                 = Guest
      | value bankHand >= value guestHand = Bank
      | otherwise                         = Guest

  -- Combines two hands by putting the first hand on top of the second one
  (<+) :: Hand -> Hand -> Hand
  Empty <+ hand2         = hand2
  Add card hand <+ hand2 = Add card (hand <+ hand2)

  -- Test: Checks associativeness of the <+ operator
  prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
  prop_onTopOf_assoc h1 h2 h3 = h1 <+ (h2 <+ h3) == (h1 <+ h2) <+ h3

  -- Test: The size of the combined hand should be
  -- the sum of the sizes of the two individual hands
  prop_size_onTopOf :: Hand -> Hand -> Bool
  prop_size_onTopOf h1 h2 = size h1 + size h2 == size (h1 <+ h2)

  -- Returns a hand with all cards of a suit. 13 cards.
  fullSuit :: Suit -> Hand
  fullSuit suit = foldr (<+) Empty cardList
    where
      cardList = [Add (Card (Numeric rank) suit) Empty | rank <- [2..10]] ++
             [Add (Card rank suit) Empty | rank <- [Jack, Queen, King, Ace]]

  -- Returns a full deck of cards. 52 cards.
  fullDeck :: Hand
  fullDeck = foldr (<+) Empty fullSuits
    where
      fullSuits = map fullSuit [Hearts, Spades, Diamonds, Clubs]

  -- Draws a card from a deck and puts it into the hand.
  -- Use: draw deck hand -> (hand + card, deck - card)
  -- Error raised upon empty deck.
  draw :: Hand -> Hand -> (Hand, Hand)
  draw Empty _              = error "draw: The deck is empty."
  draw (Add card deck) hand = (deck, Add card hand)

  -- Lets the bank draw cards until its score is 16 or higher.
  playBank :: Hand -> Hand
  playBank deck = playBank' deck Empty

  -- Helper function for playBank.
  playBank' :: Hand -> Hand -> Hand
  playBank' deck hand
      | value hand >= 16 = hand
      | otherwise        = uncurry playBank' $ draw deck hand

  -- Removes the card at specified position from the hand. 0 indexed.
  -- Returns a tuple of the removed card and the modified hand.
  removeCard :: Integer -> Hand -> (Card, Hand)
  removeCard i hand = removeCard' i hand Empty

  -- Helper function for removeCard.
  removeCard' :: Integer -> Hand -> Hand -> (Card, Hand)
  removeCard' _ Empty _          = error "removeCard': card not in hand"
  removeCard' 0 (Add card p1) p2 = (card, p1 <+ p2)
  removeCard' i (Add card p1) p2 = removeCard' (i-1) p1 (Add card p2)

  -- Shuffles a deck/hand of cards using a number generator
  shuffle :: StdGen -> Hand -> Hand
  shuffle g deck = shuffle' g deck Empty

  -- Helper function for shuffle.
  shuffle' :: StdGen -> Hand -> Hand -> Hand
  shuffle' g Empty shuffled      = shuffled
  shuffle' g unShuffled shuffled = shuffle' g' remaining (Add card shuffled)
    where
      (i, g')           = randomR (0, size unShuffled - 1) g
      (card, remaining) = removeCard i unShuffled

  -- Test: A card remains in a hand after shuffling a hand.
  prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
  prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle g h

  -- Test: The size of the hand is preserved by shuffle.
  prop_size_shuffle :: StdGen -> Hand -> Bool
  prop_size_shuffle g h = size h == size (shuffle g h)

  -- Checks if a Card belongs to a Hand.
  belongsTo :: Card -> Hand -> Bool
  c `belongsTo` Empty      = False
  c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h
