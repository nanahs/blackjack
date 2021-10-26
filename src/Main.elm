module Main exposing (main)

import Browser
import Card exposing (Card)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Player exposing (Player)
import SelectList exposing (SelectList)


type Model
    = Model Internal


type alias Internal =
    { players : Dict String Player
    , deck : List Card
    , turn : SelectList String
    }



--
-- GENERAL RULES
-- > 21 bust
-- HIT to recieve card
-- STAND to finalize your hand
--
--
-- DEALER RULES
-- <= 16 must hit
-- > 16 must stand
--
--
-- PLAYER RULES
-- TODO splitting/doubling down
-- TODO naturals
-- TODO add face up/dace down cards
--
--
-- rtfeldman/elm-validate/4.0.1 could be an interesting way to handle checking for all the different scenarios after a state change


init : ( Model, Cmd Msg )
init =
    let
        playerDict =
            [ "1", "2", "3", "4", "dealer" ]
                |> List.map (\p -> ( p, Player.empty ))
                |> Dict.fromList
    in
    ( { players = playerDict
      , deck = Card.makeDeck
      , turn = SelectList.fromLists [] "1" [ "2", "3", "4", "dealer" ]
      }
    , Cmd.none
    )
        |> Tuple.mapFirst Model



-- UPDATE


type Msg
    = Hit String
    | Stand String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    Tuple.mapFirst Model <|
        case msg of
            Hit player ->
                case dealCard model.deck model.players player of
                    ( newPlayers, newDeck ) ->
                        ( { model | players = newPlayers, deck = newDeck }, Cmd.none )

            Stand player ->
                ( { model
                    | players =
                        Dict.update player
                            (Maybe.map Player.stand)
                            model.players
                  }
                , Cmd.none
                )


dealCard : List Card -> Dict String Player -> String -> ( Dict String Player, List Card )
dealCard deck players playerKey =
    case deck of
        one :: rest ->
            ( Dict.update playerKey
                (Maybe.map
                    (\player ->
                        let
                            newPlayer =
                                Player.addCard one player
                        in
                        if Card.handToValue (Player.getHand newPlayer) <= 21 then
                            newPlayer

                        else
                            Player.stand newPlayer
                    )
                )
                players
            , rest
            )

        _ ->
            ( players, deck )



-- VIEWS


view : Model -> Html Msg
view (Model model) =
    Html.div []
        [ viewPlayers model.players
        ]


viewPlayers : Dict String Player -> Html Msg
viewPlayers players =
    players
        |> Dict.toList
        |> List.sortBy (\( key, _ ) -> Maybe.withDefault 999 (String.toInt key))
        |> List.map viewPlayer
        |> Html.div [ Attributes.class "flex gap-x-2" ]


viewPlayer : ( String, Player ) -> Html Msg
viewPlayer ( name, player ) =
    let
        handVal =
            player
                |> Player.getHand
                |> Card.handToValue
    in
    Html.div [ Attributes.class "flex flex-col" ]
        [ Html.div [ Attributes.class "flex flex-col" ]
            [ Html.div [] [ Html.text (String.join " " [ "player", name ]) ]
            , Html.div [] [ Html.text (String.fromInt handVal) ]
            , Html.div []
                [ Html.button [ Attributes.class "px-3 py-1 bg-green-200 rounded-md", Events.onClick (Hit name) ] [ Html.text "Hit" ]
                , Html.button [ Attributes.class "px-3 py-1 bg-red-200 rounded-md", Events.onClick (Stand name) ] [ Html.text "Stand" ]
                ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = \model -> { title = "Black Jack", body = [ view model ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }
