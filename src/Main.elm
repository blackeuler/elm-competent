module Main exposing (..)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { playerTurn : Turn
    , board : Board
    }


type Turn
    = O
    | X


not : Turn -> Turn
not v =
    case v of
        O ->
            X

        X ->
            O


type alias Board =
    List Cell


type Cell
    = Cell Position (Maybe Turn)


type alias Position =
    { x : Int
    , y : Int
    }


startBoard : Board
startBoard =
    [ Cell (Position 1 1) (Nothing)
    , Cell (Position 1 2) Nothing
    , Cell (Position 1 3) Nothing
    , Cell (Position 2 1) Nothing
    , Cell (Position 2 2) Nothing
    , Cell (Position 2 3) Nothing
    , Cell (Position 3 1) Nothing
    , Cell (Position 3 2) Nothing
    , Cell (Position 3 3) Nothing
    ]


win : Board -> Bool
win board =
    diagWin board || horizontalOVertical board


diagWin : Board -> Bool
diagWin board =
    majorDiagWin board || minorDiagWin board


majorDiagWin : Board -> Bool
majorDiagWin board =
    let
        cells =
            List.filter (\(Cell pos _) -> pos.x ==  pos.y ) board

        values =
            List.map (\(Cell _ val) -> val) cells
    in
    List.all (\y -> y == Just X) values || List.all (\v -> v == Just O) values

minorDiagWin : Board -> Bool
minorDiagWin board =
    let
        cells =
            List.filter (\(Cell pos _) -> pos.x == 3 - pos.y - 1) board

        values =
            List.map (\(Cell _ val) -> val) cells
    in
    List.all (\y -> y == Just X) values || List.all (\v -> v == Just O) values


horizontalOVertical : Board -> Bool
horizontalOVertical board =
    verticalWin board || horizontalWin board


verticalWin : Board -> Bool
verticalWin board =
    List.map (\col -> vertical col board) [ 1, 2, 3 ]
        |> List.any (\x -> x == True)


horizontalWin : Board -> Bool
horizontalWin board =
    List.map (\row -> horizontal row board) [ 1, 2, 3 ]
        |> List.any (\x -> x == True)


horizontal : Int -> Board -> Bool
horizontal row board =
    List.all (\x -> x == Just O) <| getRowValue row board


vertical : Int -> Board -> Bool
vertical col board =
    List.all (\y -> y == Just X) <| getColValue col board


getColValue : Int -> Board -> List (Maybe Turn)
getColValue col board =
    List.filter (\(Cell pos _) -> pos.y == col) board
        |> List.map (\(Cell _ val) -> val)


getRowValue : Int -> Board -> List (Maybe Turn)
getRowValue row board =
    List.filter (\(Cell pos _) -> pos.x == row) board
        |> List.map (\(Cell _ val) -> val)


init : Model
init =
    { playerTurn = O, board = startBoard }


type Msg
    = Reset
    | Click Position


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset ->
            init

        Click pos ->
            { playerTurn = not model.playerTurn, board = updateCell model.board pos model.playerTurn }


updateCell : Board -> Position -> Turn -> Board
updateCell board pos turn =
    List.map
        (\(Cell posR val) ->
            if pos == posR then
                Cell pos (Just turn)

            else
                Cell posR val
        )
        board


view : Model -> Html Msg
view model =
    div []
        [ viewGrid model.board
        , div []
            [ if win model.board then
                text "We have a winner"

              else
                text " "
            ]
        ]


viewGrid : Board -> Html Msg
viewGrid board =
    div
        [ style "display" "grid"
        , style "grid-template-columns" "1fr 1fr 1fr"
        , style "grid-template-rows" "1fr 1fr 1fr"
        ]
        (List.map
            viewCell
            board
        )


viewCell : Cell -> Html Msg
viewCell cell =
    case cell of
        Cell pos mtrn ->
            div
                [ style "grid-row" <| String.fromInt pos.x
                , style "grid-column" <| String.fromInt pos.y
                , style "border" "black solid 2px"
                , onClick <| Click pos
                ]
                [ showTurn mtrn ]


showTurn : Maybe Turn -> Html Msg
showTurn trn =
    case trn of
        Nothing ->
            text " "

        Just O ->
            text "O"

        Just X ->
            text "X"
