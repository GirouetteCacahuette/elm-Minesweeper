module Main exposing (Cell, Model, Msg(..), Page(..), Route(..), displayView, init, main, update)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Navigation exposing (Key)
import Html exposing (Html, a, button, div, h1, li, text, ul)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>))


mediumDifficultyNumberOfCells =
    252


mediumDifficultyNumberOfBombs =
    40


type alias Model =
    { key : Key
    , page : Page
    , cells : List Cell
    }


type alias Cell =
    { id : Int
    , mined : Bool
    , number : Maybe Int
    , clicked : Bool
    }


type Route
    = HomeRoute
    | GameRoute


type Msg
    = OnUrlRequest UrlRequest
    | OnUrlChange Url
    | OnFirstCellClick Int


type Page
    = HomePage
    | GamePage


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = displayView
        , subscriptions = always Sub.none
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( page, cmd ) =
            parserUrlToPageAndCommand url
    in
    ( Model key page (getDefaultCells mediumDifficultyNumberOfCells), cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlRequest urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Navigation.pushUrl model.key (Url.toString url) )

                External url ->
                    ( model, Navigation.load url )

        OnUrlChange url ->
            let
                ( page, cmd ) =
                    parserUrlToPageAndCommand url
            in
            ( Model model.key page model.cells, cmd )

        OnFirstCellClick cellId ->
            ( Model model.key GamePage model.cells, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ case model.page of
            HomePage ->
                displayHomePage

            GamePage ->
                displayGamePage model
        ]


displayView : Model -> Document Msg
displayView model =
    Document
        "Minesweeper"
        [ view model ]


displayHomePage : Html Msg
displayHomePage =
    div [ class "homePage" ]
        [ h1 [ class "Game title" ] [ text "Elm Minesweeper 💥 " ]
        , a [ class "startGameButton", href "#game" ] [ text "Start game 💣" ]
        ]


displayGamePage : Model -> Html Msg
displayGamePage model =
    div [ class "gamePage" ]
        [ h1 [] [ text "This is the Game page." ]
        , a [ class "homeButton", href "" ] [ text "⬅ Back to Home" ]
        , ul [ class "cells" ]
            (List.map
                (\cell ->
                    liCell cell
                )
                model.cells
            )
        ]


liCell : Cell -> Html Msg
liCell cell =
    li []
        [ button [ onClick (OnFirstCellClick cell.id) ]
            [ case cell.number of
                Nothing ->
                    text "Empty cell"

                Just number ->
                    text (String.fromInt number)
            ]
        ]


parserUrlToPageAndCommand : Url -> ( Page, Cmd Msg )
parserUrlToPageAndCommand url =
    let
        routeMaybe : Maybe Route
        routeMaybe =
            Parser.parse routeParser { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
    in
    case Maybe.withDefault HomeRoute routeMaybe of
        HomeRoute ->
            ( HomePage, Cmd.none )

        GameRoute ->
            ( GamePage, Cmd.none )


routeParser : Parser.Parser (Route -> Route) Route
routeParser =
    Parser.oneOf
        [ Parser.map HomeRoute Parser.top
        , Parser.map GameRoute (Parser.s "game")
        ]


getDefaultCells : Int -> List Cell
getDefaultCells numberOfCells =
    let
        ids : List Int
        ids =
            List.range 0 numberOfCells
    in
    List.map buildDefaultCellFromId ids


buildDefaultCellFromId : Int -> Cell
buildDefaultCellFromId id =
    Cell id False Nothing False
