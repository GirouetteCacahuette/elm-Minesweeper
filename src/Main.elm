module Main exposing (Cell, Model, Msg(..), Page(..), Route(..), displayView, init, main, update)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Navigation exposing (Key)
import Html exposing (Html, a, button, div, h1, li, text, ul)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import Random exposing (Generator, int, list)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>))
import Utils.Utils exposing (styles)


type alias GameInfo =
    { difficulty : Difficulty
    , numberOfCells : Int
    , numberOfBombs : Int
    , numberOfRows : Int
    , numberOfColumns : Int
    }


type alias Model =
    { key : Key
    , page : Page
    , cells : List Cell
    , gameInfo : GameInfo
    }


type alias Cell =
    { id : Int
    , mined : Bool
    , number : Maybe Int
    , clicked : Bool
    }


type Difficulty
    = Easy
    | Medium
    | Hard


type Route
    = HomeRoute
    | GameRoute


type Msg
    = OnUrlRequest UrlRequest
    | OnUrlChange Url
    | OnFirstCellClick Int
    | OnBombsIdsGenerated Int (List Int)


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
    ( Model
        key
        page
        (getDefaultCells
            mediumDifficultyGameInfo.numberOfCells
        )
        mediumDifficultyGameInfo
    , cmd
    )


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
            ( Model model.key page model.cells model.gameInfo, cmd )

        OnFirstCellClick cellId ->
            ( model, Random.generate (OnBombsIdsGenerated cellId) (bombsIdsGenerator model.gameInfo) )

        OnBombsIdsGenerated firstClickedCellId bombsIdsList ->
            ( { model
                | cells =
                    getFilledCells firstClickedCellId model.gameInfo.numberOfCells bombsIdsList
              }
            , Cmd.none
            )


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
        [ styles
        , view model
        ]


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
        , ul
            [ class "cells"
            , style "grid-template-columns"
                ("repeat("
                    ++ String.fromInt model.gameInfo.numberOfColumns
                    ++ ", 40px)"
                )
            , style "grid-template-rows"
                ("repeat("
                    ++ String.fromInt model.gameInfo.numberOfRows
                    ++ ", 40px)"
                )
            , style "width" (String.fromInt (model.gameInfo.numberOfColumns * 40) ++ "px")
            ]
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
        [ button
            [ onClick (OnFirstCellClick cell.id)
            , class
                (case cell.clicked of
                    True ->
                        "clicked"

                    False ->
                        "unclicked"
                )
            ]
            [ case cell.clicked of
                True ->
                    case cell.mined of
                        True ->
                            text "💥"

                        False ->
                            case cell.number of
                                Nothing ->
                                    text ""

                                Just number ->
                                    text (String.fromInt number)

                False ->
                    text ""
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
            List.range 1 numberOfCells
    in
    List.map buildDefaultCellFromId ids


buildDefaultCellFromId : Int -> Cell
buildDefaultCellFromId id =
    Cell id False Nothing False


bombsIdsGenerator : GameInfo -> Generator (List Int)
bombsIdsGenerator { numberOfBombs, numberOfCells } =
    list numberOfBombs (int 1 numberOfCells)


getFilledCells : Int -> Int -> List Int -> List Cell
getFilledCells firstClickedCellId numberOfCells bombsIds =
    let
        ids : List Int
        ids =
            List.range 1 numberOfCells
    in
    List.map
        (\id ->
            if firstClickedCellId == id then
                Cell id False Nothing True

            else if List.member id bombsIds then
                Cell id True Nothing False

            else
                Cell id False Nothing False
        )
        ids


easyDifficultyGameInfo : GameInfo
easyDifficultyGameInfo =
    GameInfo Easy 80 10 10 8


mediumDifficultyGameInfo : GameInfo
mediumDifficultyGameInfo =
    GameInfo Medium 252 40 14 18


hardDifficultyGameInfo : GameInfo
hardDifficultyGameInfo =
    GameInfo Hard 480 99 24 20
