module Main exposing (Model, Msg(..), Page(..), Route(..), init, main, update, view)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Navigation exposing (Key)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>))


type alias Model =
    { key : Key
    , page : Page
    }


type Route
    = HomeRoute


type Msg
    = CaseClicked
    | OnUrlRequest UrlRequest
    | OnUrlChange Url


type Page
    = HomePage


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
        ]


parserUrlToPageAndCommand : Url -> ( Page, Cmd Msg )
parserUrlToPageAndCommand url =
    let
        routeMaybe : Maybe Route
        routeMaybe =
            Parser.parse routeParser url
    in
    case Maybe.withDefault HomeRoute routeMaybe of
        HomeRoute ->
            ( HomePage, Cmd.none )


routeParser : Parser.Parser (Route -> Route) Route
routeParser =
    Parser.oneOf
        [ Parser.map HomeRoute Parser.top
        ]
