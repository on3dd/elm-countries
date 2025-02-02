module Main exposing (..)

import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, placeholder, src, value)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Json.Decode as D exposing (Decoder, field, list, map3, string)



-- TYPES


type alias Country =
    { flag : String
    , name : String
    , region : String
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
            ( { model | content = "", status = Loading }, search Nothing )

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
            [ width (pct 100)
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
                , maxWidth (px 400)
                , margin2 (px 0) auto
                , textAlign center
                ]
            ]
            [ h1
                [ class "app__heading"
                , css
                    [ margin2 (rem 1) (px 0) ]
                ]
                [ text "Countries" ]
            , div
                [ class "app__view" ]
                [ viewResponse model ]
            ]
        ]


viewResponse : Model -> Html Msg
viewResponse model =
    case model.status of
        Failure ->
            div
                [ class "app__search"
                , css
                    [ textAlign center
                    ]
                ]
                [ p
                    [ class "search__error"
                    , css
                        []
                    ]
                    [ text "I could not load a list of countries for some reason. " ]
                , button
                    [ onClick Reload
                    , class "search__text"
                    , css
                        [ display inlineBlock
                        , fontSize (rem 1)
                        , padding2 (rem 0.5) (rem 1)
                        ]
                    ]
                    [ text "Try Again!" ]
                ]

        Loading ->
            div
                [ class "app__search"
                , css
                    [ displayFlex
                    , flexDirection column
                    , alignItems center
                    ]
                ]
                [ viewInput model.content
                , span
                    [ class "search__input" ]
                    [ text "Loading..." ]
                ]

        Success list ->
            div
                [ class "app__search"
                , css
                    [ displayFlex
                    , flexDirection column
                    , alignItems center
                    ]
                ]
                [ viewInput model.content
                , viewList list
                ]


viewInput : String -> Html Msg
viewInput content =
    input
        [ placeholder "Enter the country name"
        , value content
        , onInput Search
        , class "search__input"
        , css
            [ display block
            , fontSize (rem 1)
            , padding2 (rem 0.5) (rem 1)
            , marginBottom (rem 2)
            ]
        ]
        []


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
            [ displayFlex
            , marginBottom (rem 1.5)
            , border3 (px 1) solid (rgba 0 0 0 0.1)
            , boxShadow5 (px 0) (px 4) (px 12) (px 0) (rgba 0 0 0 0.1)
            ]
        ]
        [ div
            [ class "item__flag"
            , css
                [ flex (int 2)
                , position relative
                , overflow hidden
                ]
            ]
            [ img
                [ src country.flag
                , class "item__image"
                , css
                    [ display block
                    , height (pct 100)
                    , position absolute
                    , left (pct 50)
                    , transform (translateX (pct -50))
                    ]
                ]
                []
            ]
        , div
            [ class "item__info"
            , css
                [ flex (int 3)
                , margin2 (rem 1) (rem 1)
                , textAlign left
                ]
            ]
            [ span
                [ class "item__name"
                , css
                    [ display block
                    , marginBottom (rem 0.5)
                    , fontSize (rem 1.25)
                    , fontWeight (int 600)
                    ]
                ]
                [ text country.name ]
            , span
                [ class "item__region"
                , css
                    [ display block
                    , marginBottom (rem 0.5)
                    ]
                ]
                [ span
                    [ class "item__region-bold"
                    , css
                        [ display inlineBlock
                        , marginRight (ch 0.5)
                        , fontWeight (int 500)
                        , color (hex "#444")
                        ]
                    ]
                    [ text "Region:" ]
                , text country.region
                ]
            ]
        ]



-- HTTP


search : Maybe String -> Cmd Msg
search str =
    Http.get
        { url = searchUrl str
        , expect = Http.expectJson GotResponse searchDecoder
        }


searchUrl : Maybe String -> String
searchUrl str =
    let
        baseUrl =
            "https://restcountries.eu/rest/v2"

        fields =
            "?fields=name;flag;region"
    in
    case str of
        Nothing ->
            String.join "/" [ baseUrl, "all" ++ fields ]

        Just name ->
            if name |> String.trim |> String.isEmpty then
                String.join "/" [ baseUrl, "all" ++ fields ]

            else
                String.join "/" [ baseUrl, "name", name ++ fields ]


searchDecoder : Decoder (List Country)
searchDecoder =
    D.list
        (map3 Country
            (field "flag" string)
            (field "name" string)
            (field "region" string)
        )
