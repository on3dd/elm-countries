module Main exposing (Model(..), Msg(..), init, main, search, searchDecoder, subscriptions, update, view, viewResponse)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D exposing (Decoder, field, list, map2, string)


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


type Model
    = Failure
    | Loading
    | Success (List Country)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, search )



-- UPDATE


type Msg
    = MorePlease
    | GotResponse (Result Http.Error (List Country))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        MorePlease ->
            ( Loading, search )

        GotResponse result ->
            case result of
                Ok list ->
                    ( Success list, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



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
    case model of
        Failure ->
            div []
                [ text "I could not load a list of countries for some reason. "
                , button [ onClick MorePlease ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading..."

        Success list ->
            div []
                [ button [ onClick MorePlease, style "display" "block" ] [ text "More Please!" ]
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


search : Cmd Msg
search =
    Http.get
        { url = "https://restcountries.eu/rest/v2/all?fields=name;flag"
        , expect = Http.expectJson GotResponse searchDecoder
        }


searchDecoder : Decoder (List Country)
searchDecoder =
    D.list
        (map2 Country
            (field "flag" string)
            (field "name" string)
        )
