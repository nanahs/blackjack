module Card exposing (Card, addCard, handToValue, hasAce, makeDeck)


type Card
    = Card Internal


type alias Internal =
    { rank : Rank, suit : Suit }


type Suit
    = Hearts
    | Diamonds
    | Clubs
    | Spades


type Rank
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King


handToValue : List Card -> Int
handToValue cards =
    -- will always try to return the highest value if an ace is present
    let
        value =
            cards
                |> List.map cardToValue
                |> List.foldl (+) 0
    in
    if List.any isAce cards && value > 21 then
        value - 11
        --can be improved by recursive function that removes the ace from the hand, subtracts 11, and then calculates the value of the remaining cards

    else
        value


isAce : Card -> Bool
isAce (Card card) =
    card.rank == Ace


hasAce : List Card -> Bool
hasAce =
    List.any isAce


cardToValue : Card -> Int
cardToValue (Card card) =
    case card.rank of
        Ace ->
            11

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ten ->
            10

        Jack ->
            10

        Queen ->
            10

        King ->
            10


makeDeck : List Card
makeDeck =
    -- TODO shuffle this
    [ Hearts, Diamonds, Clubs, Spades ]
        |> List.concatMap
            (\suit ->
                [ Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King ]
                    |> List.map
                        (\rank ->
                            Card { rank = rank, suit = suit }
                        )
            )


addCard : Card -> List Card -> List Card
addCard card hand =
    card :: hand
