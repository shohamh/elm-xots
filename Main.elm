module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (id)
import Basics exposing (toString, floor, negate)
import List exposing (..)
import Dict exposing (..)
import Maybe
import Maybe.Extra exposing (join)
import Debug exposing (log)
import Tuple exposing (..)


-- MODEL


type alias Model =
    { board : Dict ( Int, Int ) Player
    , currentPlayer : Player
    , width : Int
    , height : Int
    , winner : Maybe Player
    , lastMove : ( Int, Int )
    , chainToWin : Int
    }


model : Model
model =
    { board = Dict.empty
    , currentPlayer = X
    , width = 10
    , height = 10
    , winner = Nothing
    , lastMove = ( 0, 0 )
    , chainToWin = 5
    }


type Msg
    = NoOp
    | ButtonClick ( Int, Int )


type Player
    = X
    | O
    | T
    | S



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        ButtonClick ( x, y ) ->
            { model
                | board = Dict.insert ( x, y ) model.currentPlayer model.board
                , currentPlayer =
                    nextPlayer
                        model.currentPlayer
                , winner =
                    checkWin
                        { model
                            | board =
                                Dict.insert ( x, y )
                                    model.currentPlayer
                                    model.board
                        }
                , lastMove = ( x, y )
            }


checkWin : Model -> Maybe Player
checkWin model =
    let
        filterLastPlayer : ( Int, Int ) -> Player -> Bool
        filterLastPlayer ( x, y ) player =
            if player == prevPlayer model.currentPlayer then
                True
            else
                False

        filteredBoard =
            Dict.filter filterLastPlayer model.board

        longestChainRow =
            lengthOfLongestPartialListOfDefinedElements <|
                getRowFromBoard filteredBoard model.lastMove model.chainToWin

        longestChainCol =
            lengthOfLongestPartialListOfDefinedElements <|
                getColFromBoard filteredBoard model.lastMove model.chainToWin

        longestChainDiagonal =
            lengthOfLongestPartialListOfDefinedElements <|
                getDiagonalFromBoard filteredBoard model.lastMove model.chainToWin

        longestChainAntiDiagonal =
            lengthOfLongestPartialListOfDefinedElements <|
                getAntiDiagonalFromBoard filteredBoard model.lastMove model.chainToWin
    in
        if
            longestChainRow
                >= model.chainToWin
                || longestChainCol
                >= model.chainToWin
                || longestChainDiagonal
                >= model.chainToWin
                || longestChainAntiDiagonal
                >= model.chainToWin
        then
            Debug.log
                (toString longestChainRow
                    ++ ","
                    ++ toString longestChainCol
                    ++ ","
                    ++ toString longestChainDiagonal
                    ++ ","
                    ++ toString longestChainAntiDiagonal
                )
                Just
                (prevPlayer model.currentPlayer)
        else
            Debug.log
                (toString longestChainRow
                    ++ ","
                    ++ toString longestChainCol
                    ++ ","
                    ++ toString longestChainDiagonal
                    ++ ","
                    ++ toString longestChainAntiDiagonal
                )
                Nothing


lengthOfLongestPartialListOfDefinedElements : List (Maybe a) -> Int
lengthOfLongestPartialListOfDefinedElements list =
    case list of
        [] ->
            0

        Nothing :: rest ->
            0

        (Just _) :: rest ->
            1 + lengthOfLongestPartialListOfDefinedElements rest


getRowFromBoard : Dict ( Int, Int ) Player -> ( Int, Int ) -> Int -> List (Maybe Player)
getRowFromBoard filteredBoard lastMove chainToWin =
    List.map
        (\x ->
            Dict.get ( first lastMove + x, second lastMove )
                filteredBoard
        )
    <|
        List.range (negate chainToWin) chainToWin


getColFromBoard : Dict ( Int, Int ) Player -> ( Int, Int ) -> Int -> List (Maybe Player)
getColFromBoard filteredBoard lastMove chainToWin =
    List.map
        (\x ->
            Dict.get ( first lastMove, second lastMove + x )
                filteredBoard
        )
    <|
        List.range (negate chainToWin) chainToWin


getDiagonalFromBoard : Dict ( Int, Int ) Player -> ( Int, Int ) -> Int -> List (Maybe Player)
getDiagonalFromBoard filteredBoard lastMove chainToWin =
    List.map
        (\x ->
            Dict.get ( first lastMove + x, second lastMove + x )
                filteredBoard
        )
    <|
        List.range (negate chainToWin) chainToWin


getAntiDiagonalFromBoard : Dict ( Int, Int ) Player -> ( Int, Int ) -> Int -> List (Maybe Player)
getAntiDiagonalFromBoard filteredBoard lastMove chainToWin =
    List.map (\x -> Dict.get ( first lastMove + x, second lastMove + x ) filteredBoard) <|
        List.reverse <|
            List.range (negate chainToWin) chainToWin


nextPlayer : Player -> Player
nextPlayer player =
    case player of
        X ->
            O

        O ->
            T

        T ->
            S

        S ->
            X


prevPlayer : Player -> Player
prevPlayer player =
    case player of
        X ->
            S

        O ->
            X

        T ->
            O

        S ->
            T



-- VIEW


printPlayer : Maybe Player -> Html Msg
printPlayer player =
    case player of
        Nothing ->
            text ""

        Just X ->
            text "X"

        Just O ->
            text "O"

        Just T ->
            text "T"

        Just S ->
            text "S"


makeTdWithButton : Model -> ( Int, Int ) -> Html Msg
makeTdWithButton model ( x, y ) =
    td []
        [ button [ onClick (ButtonClick ( x, y )) ]
            [ printPlayer <| Dict.get ( x, y ) model.board
            ]
        ]


generateRow : Model -> Int -> Int -> Html Msg
generateRow model start length =
    tr []
        (List.map (makeTdWithButton model) <|
            List.map (\n -> ( start, n )) <|
                List.range 1 length
        )


showBoard : Model -> Html Msg
showBoard model =
    table []
        (List.map2 (generateRow model)
            (List.map (\x -> x * model.width)
                (List.range 1 model.height)
            )
            (List.repeat model.height model.width)
        )


view : Model -> Html Msg
view model =
    div []
        [ showBoard
            model
        , printPlayer model.winner
        ]



-- APP


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }
