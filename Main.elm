module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Basics exposing (toString)


-- MODEL


type alias Model =
    { currentValue : Int
    }


model : Model
model =
    { currentValue = 0
    }


type Msg
    = NoOp
    | ButtonClick



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        ButtonClick ->
            { model | currentValue = model.currentValue + 1 }



-- VIEW


generateRowInTable : Int -> Html Msg -> Html Msg
generateRowInTable n element =
    let
        row =
            []
    in
        tr []
            [ case List.length row of
                n ->
                    row

                _ ->
                    td [] [ element ] :: row
            ]


generateTable : Int -> Int -> Html Msg -> Html Msg
generateTable col row element =
    let
        table =
            []
    in
        table []
            [ case List.length table of
                col ->
                    table

                _ ->
                    (generateRowInTable row element) :: table
            ]


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick ButtonClick ]
            [ text
                ("currentValue: "
                    ++ toString model.currentValue
                )
            ]
        ]



-- APP


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }
