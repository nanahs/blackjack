module Player exposing (Player, addCard, canPlay, empty, getHand, stand)

import Card exposing (Card)


type Player
    = Player Internal


type alias Internal =
    { hand : List Card, canPlay : Bool }


empty : Player
empty =
    Player { hand = [], canPlay = True }


getHand : Player -> List Card
getHand (Player player) =
    player.hand


canPlay : Player -> Bool
canPlay (Player player) =
    player.canPlay


addCard : Card -> Player -> Player
addCard card (Player player) =
    let
        newHand =
            Card.addCard card player.hand
    in
    Player { player | hand = newHand }


stand : Player -> Player
stand (Player p) =
    Player { p | canPlay = False }
