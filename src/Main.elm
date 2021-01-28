module Main exposing (Model, Msg(..), init, main, search, searchDecoder, subscriptions, update, view, viewResponse)

import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, classList, css, placeholder, src, value)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Json.Decode as D exposing (Decoder, field, list, map2, string)



-- TYPES


type alias Country =
    { flag : String
    , name : String
    }



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> toUnstyled
        }



-- MODEL


type alias Model =
    { content : String
    , status : Status
    }


type Status
    = Failure
    | Loading
    | Success (List Country)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { content = "", status = Loading }, search Nothing )



-- UPDATE


type Msg
    = Reload
    | Search String
    | GotResponse (Result Http.Error (List Country))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reload ->
            ( { model | status = Loading }, search Nothing )

        Search str ->
            ( { model | content = str, status = Loading }, search (Just str) )

        GotResponse result ->
            case result of
                Ok list ->
                    ( { model | status = Success list }, Cmd.none )

                Err _ ->
                    ( { model | status = Failure }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "app"
        , css
            [ width (vw 100)
            , fontSize (px 16)
            , fontFamilies
                [ "Segoe UI"
                , "Frutiger"
                , "Frutiger Linotype"
                , "Dejavu Sans"
                , "Helvetica Neue"
                , "Arial"
                , "sans-serif"
                ]
            , overflowX hidden
            ]
        ]
        [ div
            [ class "app__container"
            , css
                [ width (pct 100)
                , maxWidth (px 500)
                , margin2 (px 0) auto
                , textAlign center
                ]
            ]
            [ h2
                [ class "app__heading"
                , css
                    [ margin2 (rem 1) (px 0) ]
                ]
                [ text "Countries" ]
            , div
                [ class "app__view"
                , css
                    [ width inherit
                    , maxWidth (px 500)
                    ]
                ]
                [ viewResponse model ]
            ]
        ]


viewResponse : Model -> Html Msg
viewResponse model =
    case model.status of
        Failure ->
            div
                [ classList
                    [ ( "app__search", True )
                    , ( "app__search-failure", True )
                    ]
                ]
                [ span
                    [ class "search__error" ]
                    [ text "I could not load a list of countries for some reason. " ]
                , button
                    [ class "search__text", onClick Reload ]
                    [ text "Try Again!" ]
                ]

        Loading ->
            div
                [ classList
                    [ ( "app__search", True )
                    , ( "app__search-loading", True )
                    ]
                , css
                    [ displayFlex
                    , flexDirection column
                    , alignItems center
                    ]
                ]
                [ input
                    [ placeholder "Enter the country name"
                    , value model.content
                    , onInput Search
                    , class "search__input"
                    , css
                        [ display block
                        , marginBottom (rem 0.5)
                        ]
                    ]
                    []
                , span
                    [ class "search__input" ]
                    [ text "Loading..." ]
                ]

        Success list ->
            div
                [ classList
                    [ ( "app__search", True )
                    , ( "app__search-success", True )
                    ]
                , css
                    [ displayFlex
                    , flexDirection column
                    , alignItems center
                    ]
                ]
                [ input
                    [ placeholder "Enter the country name"
                    , value model.content
                    , onInput Search
                    , class "search__input"
                    , css
                        [ display block
                        , marginBottom (rem 2)
                        ]
                    ]
                    []
                , viewList list
                ]


viewList : List Country -> Html Msg
viewList countries =
    ul
        [ class "app__list"
        , css
            [ margin (px 0)
            , padding (px 0)
            , width (pct 100)
            ]
        ]
        (List.map viewItem countries)


viewItem : Country -> Html Msg
viewItem country =
    li
        [ class "app__item"
        , css
            [ display block
            , marginBottom (rem 1)
            , overflowX hidden
            ]
        ]
        [ span
            [ class "item__text"
            , css
                [ display block
                , marginBottom (rem 0.5)
                ]
            ]
            [ text country.name ]
        , img
            [ src country.flag
            , class "item__flag"
            , css
                [ display block
                , width (pct 100)
                ]
            ]
            []
        ]



-- HTTP


search : Maybe String -> Cmd Msg
search str =
    let
        baseUrl =
            "https://restcountries.eu/rest/v2"

        fields =
            "?fields=name;flag"

        expect =
            Http.expectJson GotResponse searchDecoder
    in
    case str of
        Nothing ->
            Http.get
                { url = String.join "/" [ baseUrl, "all" ++ fields ]
                , expect = expect
                }

        Just name ->
            Http.get
                { url = String.join "/" [ baseUrl, "name", name ++ fields ]
                , expect = expect
                }


searchDecoder : Decoder (List Country)
searchDecoder =
    D.list
        (map2 Country
            (field "flag" string)
            (field "name" string)
        )
