module Main exposing (Model, Msg(..), init, main, search, searchDecoder, subscriptions, update, view, viewResponse)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
        , view = view
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
    div []
        [ h2 [] [ text "Countries" ]
        , viewResponse model
        ]


viewResponse : Model -> Html Msg
viewResponse model =
    case model.status of
        Failure ->
            div []
                [ text "I could not load a list of countries for some reason. "
                , button [ onClick Reload ] [ text "Try Again!" ]
                ]

        Loading ->
            div []
                [ input
                    [ placeholder "Enter the country name"
                    , style "display" "block"
                    , value model.content
                    , onInput Search
                    , disabled True
                    ]
                    []
                , text "Loading..."
                ]

        Success list ->
            div []
                [ input
                    [ placeholder "Enter the country name"
                    , style "display" "block"
                    , value model.content
                    , onInput Search
                    ]
                    []
                , viewList list
                ]


viewList : List Country -> Html Msg
viewList countries =
    ul
        []
        (List.map viewItem countries)


viewItem : Country -> Html Msg
viewItem country =
    div []
        [ text country.name
        , img [ src country.flag ] []
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
