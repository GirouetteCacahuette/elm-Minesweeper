module Main exposing (Model, Msg(..), Page(..), Route(..), init, main, update, view)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Navigation exposing (Key)
import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (class, href)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>))


type alias Model =
    { key : Key
    , page : Page
    }


type Route
    = HomeRoute
    | GameRoute


type Msg
    = CaseClicked
    | OnUrlRequest UrlRequest
    | OnUrlChange Url


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
    ( Model key page, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CaseClicked ->
            ( Model model.key HomePage, Cmd.none )

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
            ( Model model.key page, cmd )


view : Model -> Html Msg
view model =
    div []
        [ case model.page of
            HomePage ->
                displayHomePage

            GamePage ->
                displayGamePage
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


displayGamePage : Html Msg
displayGamePage =
    div [ class "gamePage" ]
        [ h1 [] [ text "This is the Game page." ]
        , a [ class "homeButton", href "" ] [ text "⬅ Back to Home" ]
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
