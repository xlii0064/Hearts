{-
The main strategy is to filter out the invalid cards first then sort the current hand based on different strategy. So to filter out the invalid
cards, the validPlay function from Rules was used to filter out the invalid ones. But since that function requires all the previous cards, so
the cards from previous round and the memory were used to create a "fake" memory. The memory would store if a heart has been played or not so when
generating the fake memory, if the memory indicates a heart was indeed been played, the function would add a virtual heart card to the previous cards list
so the Heart cards wouldn't be filtered out. 
After getting all the valid cards, another function will see if the current trcik need to follow the suit or not. If so, then the strategy for same suit
will be used, which is always choosing the card that is smaller than the largest one. And if there's no card smaller than the largest one, then the player 
will play the largest card in the same suit because he has to lose anyway, so getting rid of the larger card is better.
If the player cannot follow the suit, then it will play the point card first then play the non-point card with the largest rank as the large cards would be a burden.
If the player starts this round, then he will always not to choose the cards which someone cannot follow the suit. This information will also be stored
inside the memory so the player would make decisions based on that.
-} 
module Player (
    playCard,
    makeBid
)
where

import Hearts.Types
import Control.Monad
import Cards
import Hearts.Rules
import Data.Either
import Data.List
import Data.Ord

playCard :: PlayFunc
playCard pID curHand curTrick prevRound = 
        if (clubTwoInHand curHand) then (returnFunc (Card Club Two) (unlines ["1","1","1","1","0"]))else --check if the current hand has club2 inside
        returnFunc (last --choose the one with the highest preference
        (sortByPreference ---sort the cards using different strategies
        (returnCardValid curHand pID curHand (splitTuplemyVer curTrick) --filter the cards that didn't obey the rules
        (addConditionsInsideToCreateAFakePrevious (readMemo prevRound) (readPrev prevRound)) --modify the memory so it knows if there's a heart has been played or not
        ) (splitTuplemyVer curTrick) (writeMemo (readMemo prevRound) (splitTuplemyVer curTrick))
        ))
        (writeMemo (readMemo prevRound) (splitTuplemyVer curTrick))--write the previous round's information into memory

--check if club 2 in in the list or not
clubTwoInHand::[Card]->Bool --current hand
clubTwoInHand []=False
clubTwoInHand ((Card suit rank):xs)=if suit==Club && rank==Two then True else clubTwoInHand xs

--read previous Trick to a list of cards
readPrev::Maybe ([(Card, PlayerId)], String)->[Card]--previous round
readPrev Nothing = []
readPrev (Just (l,_))=splitTuplemyVer l

--convert a list of tuples to a list of Card
splitTuplemyVer::[(Card, PlayerId)]->[Card]
splitTuplemyVer []=[]
splitTuplemyVer ((c,_):xs)= c: (splitTuplemyVer xs)

--read previous memory
readMemo::Maybe ([(Card, PlayerId)], String)->[String]--previous round tuple
readMemo Nothing = ["1","1","1","1","0"]
readMemo (Just (_,s)) = lines s

--check cards from previous round, if there's a heart, change the last item in the list to be 1
    --update the suit. If someone cannot follow that suit, change the corresponding value in the list to 0 
writeMemo::[String] --previous memory
        ->[Card]---current trick
        ->String
writeMemo = join . ((writeMemoForSuitsChecking . lines) .) . writeMemoForBleeding where
    writeMemoForBleeding s []=unlines s
    writeMemoForBleeding s ((Card suit _):xs)= if suit==Heart then (unlines (replaceThingsInMemo 4 s 1)) else writeMemo s xs--record heart presence
    writeMemoForSuitsChecking s [] = unlines s
    writeMemoForSuitsChecking s ((Card suit _):xs)=
        writeMemoForSuitsChecking (replaceThingsInMemo (suitIntoNumbers suit) s 0) xs --record if someone cannot follow the suit

--function to replace things in the list
replaceThingsInMemo :: Int--index
                    -> [String] --the list to have things replaced
                    -> Int --the new item
                    -> [String]
replaceThingsInMemo _ [] _= []
replaceThingsInMemo index (x:xs) newItem
    | index == 0 = (show newItem):xs
    | otherwise = x:(replaceThingsInMemo (index-1) xs newItem)
    
--create a fake previous cards based on whether the heart has been played or not and the previous trick
--This function is only used to check if it's a valid play or not
addConditionsInsideToCreateAFakePrevious::[String] --memory
                                        ->[Card]->[[Card]] --previous round
addConditionsInsideToCreateAFakePrevious memo prevRound = if memo!!4=="1" then [(prevRound ++ [(Card Heart Two)])] else [prevRound]
 
--filter the current hand according to the rules
returnCardValid::[Card] --the card list to be filters
                -> PlayerId-> [Card]  -- Hand
                -> [Card]                     -- cards in the previous trick
                -> [[Card]] ->[Card]       --all cards in the previous tricks
returnCardValid cards pID hand prevCards allPrevCards = if allPrevCards==[[]] then 
    filter (\c -> isRight (validPlay c pID hand prevCards [])) cards --pass empty previous cards if it's the 1st round
    else filter (\c -> isRight (validPlay c pID hand prevCards allPrevCards)) cards

--the function to sort the valid card list based on different strategy
sortByPreference::[Card]   --filtered hand
                ->[Card] --current trick
                ->String --memory
                ->[Card] --hand sorted by preference
sortByPreference filteredHand [] s=processAsFirstPlayer s filteredHand
sortByPreference filteredHand curTrick _ = if (sameSuitOrNot filteredHand) --if it's the same suit, use same suit strategy
    then  (sortBy (\a b -> compareCompareSameSuitStrategy a b curTrick) filteredHand)
    else  (sortBy (\a b -> compareCompareDiffSuitStrategy a b curTrick) filteredHand)

--first play strategy:sort the list based on memory
processAsFirstPlayer::String --memory
                    ->[Card]->[Card] --current hand
processAsFirstPlayer s= sortBy (\a b -> compareCompareForFirstPlayer (lines s) a b)

--the compare function for the fist player
compareCompareForFirstPlayer::[String]--memory
                            ->Card ---1st card to compare with
                            ->Card ---2nd card to compare with
                            ->Ordering
compareCompareForFirstPlayer [] (Card _ rank1) (Card _ rank2)=
    if rank1<rank2 then GT else LT --if the memory is empty, the smaller card gets the higher preference
compareCompareForFirstPlayer s (Card suit1 rank1) (Card suit2 rank2)= if 
    ((s!!(suitIntoNumbers suit1))=="1" && (s!!(suitIntoNumbers suit2))=="0") ||   --Not to choose the one that someone doesnt have that suit
    ((s!!(suitIntoNumbers suit1))=="0" && (s!!(suitIntoNumbers suit2))=="0" && rank1<rank2) || --prefer the small cards more as the 1st player
    ((s!!(suitIntoNumbers suit1))=="1" && (s!!(suitIntoNumbers suit2))=="1" && rank1<rank2)
    then GT else LT

--Convert suit into the correspoding index in the meomory
suitIntoNumbers::Suit->Int
suitIntoNumbers s
        |s==Spade =0
        |s==Club = 1
        |s==Diamond=2
        |otherwise = 3

--Check if all the cards in the list are in the same suit
sameSuitOrNot::[Card]->Bool
sameSuitOrNot [] =True --when list is empty return true
sameSuitOrNot (c:cs)= sameSuitOrNotAux c cs where --if 2 cards are not of the same suit, return false
    sameSuitOrNotAux _ [] =True
    sameSuitOrNotAux (Card suit1 rank) ((Card suit2 _):xs)= if suit1==suit2 then sameSuitOrNotAux (Card suit1 rank) xs else False

-- \\\\\\\\Although the following 2 functions can be merged into one large comparison fucntion, but for the sake of the readablity and maintenance,
--they were splited into 2 which play different kinds of cards accordingly\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
--The strategy to choose card when the player has to follow the suit
compareCompareSameSuitStrategy::Card --the first card to compare
                                ->Card --the 2nd card to be compared with
                                ->[Card]->Ordering -- the current trick                               
compareCompareSameSuitStrategy (Card suit1 rank1) c2 cards= 
    sameSuitStrategy (Card suit1 rank1) c2 (largestCardInSameSuit (head cards) cards) where
        sameSuitStrategy (Card _ rank11) (Card _ rank22) (Card _ rank) =
            if (rank11>rank22 && rank11<rank) || --if 2 cards are both smaller than the largest one in the current trick, select the larger one
                (rank11>rank22 && rank11>rank && rank22>rank) || --if both cards are larger than the largest one in the trcik,select the larger one
                (rank11<rank && rank22>rank) --select the one that is smaller than the largest card in the current trick
                then GT else LT

--The strategy to choose card when the player doesn't have the card from the same leading suit
compareCompareDiffSuitStrategy::Card --the first card to compare
                            ->Card --the 2nd card to be compared with
                            ->[Card]->Ordering -- the current trick                           
compareCompareDiffSuitStrategy (Card suit1 rank1) c2 _= diffSuitStrategy (Card suit1 rank1) c2  where
            diffSuitStrategy (Card Spade Queen) _ =GT    --when different suit, always play the Spade queen first
            diffSuitStrategy _ (Card Spade Queen) =LT
            diffSuitStrategy (Card suit11 rank11) (Card suit22 rank22)=
                if (suit11==Heart && suit22==Heart && (rank11>rank22 || rank11==rank22)) ||  --If card1 and card2 are both hearts, use the card with the largest rank
                    suit11==Heart && not(suit22==Heart) ||  ---If card1 is heart and card2 is not, use card1 first
                    not(suit11==Heart) && not(suit22==Heart) && (rank11>rank22 || rank11==rank22) --If both of them are not heart, consume the one with larager rank
                    then GT else LT

--find the largest card that have the same suit with the start card.This function is used to find out the largest card in the current trick in the same suit
largestCardInSameSuit::Card->[Card] --the start card to compare -> the card list
                       ->Card
largestCardInSameSuit c [] =c
largestCardInSameSuit (Card suit1 rank1) ((Card suit2 rank2):cs)  = --compare 2 cards and pass the larger one
    if suit1==suit2 && rank2>rank1 then largestCardInSameSuit (Card suit2 rank2) cs else largestCardInSameSuit (Card suit1 rank1) cs

--A nice simple return func that always returns a tuple that satisfy the return requirements.
returnFunc :: Card->String->(Card,String)
returnFunc c s =(c,s)

-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined