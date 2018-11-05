module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, int, maybe, string)
import Json.Decode.Pipeline exposing (optional, required)
import String
import Url.Builder as Url


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



{-
   MODEL

   Model section covers the entire data model used by the application. In this
   case data is modeled using the following types.

   LoadStatus:
        A union type representing the loading status of the application.
   
   Microlink:
        An automatically generated type representing a Microlink API response.

   MicrolinkDataImage:
        An automatically generated type representing an image reference.

   MicrolinkData:
        An automatically generated type representing Microlink data.

   Model:
        The data model used by the application to store state.
-}


type LoadStatus
    = Idle
    | Loading


type alias Microlink =
    { status : String
    , data : MicrolinkData
    }


type alias MicrolinkDataImage =
    { url : String
    , width : Int
    , height : Int
    , format : String
    , size : Int
    , sizePretty : String
    }


type alias MicrolinkData =
    { lang : Maybe String
    , author : Maybe String
    , title : Maybe String
    , publisher : Maybe String
    , image : Maybe MicrolinkDataImage
    , description : Maybe String
    , date : Maybe String
    , logo : Maybe MicrolinkDataImage
    , url : String
    }


type alias Model =
    { address : String
    , loading : LoadStatus
    , data : Maybe Microlink
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" Idle Nothing
    , Cmd.none
    )



{-
   UPDATE

   The update section covers all updates to the application state. The state
   is updated using messages represented by the Msg union type.
-}


type Msg
    = SubmitForm
    | ChangeAddress String
    | GotResult (Result Http.Error Microlink)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitForm ->
            ( { model | data = Nothing, loading = Loading }
            , getMicrolink model.address
            )

        ChangeAddress address ->
            ( { model | address = address }
            , Cmd.none
            )

        GotResult result ->
            case result of
                Ok data ->
                    ( { model | data = Just data, loading = Idle }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | data = Nothing, loading = Idle }
                    , Cmd.none
                    )



{-
   SUBSCRIPTIONS

   No subscriptions are actually used but this is required by Browser.element
-}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



{-
   VIEW

   The view section covers all of the functions needed to render the
   application.
-}


getImageUrl : Maybe MicrolinkDataImage -> String
getImageUrl image =
    case image of
        Nothing ->
            "https://www.freeiconspng.com/uploads/no-image-icon-32.png"

        Just i ->
            i.url


renderLoading : LoadStatus -> Html Msg
renderLoading status =
    case status of
        Loading ->
            div [ class "loading" ] [ text "Loading" ]

        _ ->
            text ""


renderData : Maybe Microlink -> List (Html Msg)
renderData data =
    case data of
        Nothing ->
            []

        Just link ->
            let
                props =
                    [ ( "Language", Maybe.withDefault "" link.data.lang )
                    , ( "Title", Maybe.withDefault "" link.data.title )
                    , ( "Publisher", Maybe.withDefault "" link.data.publisher )
                    , ( "Author", Maybe.withDefault "" link.data.author )
                    , ( "Description", Maybe.withDefault "" link.data.description )
                    ]

                logo =
                    getImageUrl link.data.logo

                image =
                    getImageUrl link.data.image
            in
            [ div [ class "logo" ] [ img [ src logo ] [] ]
            , table []
                (List.map
                    (\i ->
                        let
                            ( label, value ) =
                                i
                        in
                        tr []
                            [ th [] [ text label ]
                            , td [] [ text value ]
                            ]
                    )
                    props
                )
            , img [ src image, class "image" ] []
            ]


view : Model -> Html Msg
view model =
    let
        isDisabled =
            case model.loading of
                Loading ->
                    disabled True

                _ ->
                    disabled False
    in
    Html.form [ onSubmit SubmitForm ]
        [ div [ class "controls" ]
            [ input
                [ placeholder "URL (e.g. https://www.karhuhelsinki.fi/)"
                , value model.address
                , onInput ChangeAddress
                , isDisabled
                ]
                []
            , button [ type_ "submit", isDisabled ] [ text "›" ]
            ]
        , renderLoading model.loading
        , div [ class "result" ] <|
            renderData model.data
        ]



{-
   HTTP

   The HTTP section covers HTTP request handling and JSON decoding. The
   decoders are automatically generated from the JSON data.
-}


getMicrolink : String -> Cmd Msg
getMicrolink address =
    Http.get (toMicrolinkUrl address) decodeMicrolink
        |> Http.send GotResult


toMicrolinkUrl : String -> String
toMicrolinkUrl address =
    Url.crossOrigin "https://api.microlink.io" [] [ Url.string "url" address ]


decodeMicrolink : Decode.Decoder Microlink
decodeMicrolink =
    Decode.succeed Microlink
        |> required "status" Decode.string
        |> required "data" decodeMicrolinkData


decodeMicrolinkDataImage : Decode.Decoder MicrolinkDataImage
decodeMicrolinkDataImage =
    Decode.succeed MicrolinkDataImage
        |> required "url" Decode.string
        |> required "width" Decode.int
        |> required "height" Decode.int
        |> required "type" Decode.string
        |> required "size" Decode.int
        |> required "size_pretty" Decode.string


decodeMicrolinkDataLogo : Decode.Decoder MicrolinkDataImage
decodeMicrolinkDataLogo =
    Decode.succeed MicrolinkDataImage
        |> required "url" Decode.string
        |> required "width" Decode.int
        |> required "height" Decode.int
        |> required "type" Decode.string
        |> required "size" Decode.int
        |> required "size_pretty" Decode.string


decodeMicrolinkData : Decode.Decoder MicrolinkData
decodeMicrolinkData =
    Decode.succeed MicrolinkData
        |> optional "lang" (Decode.maybe Decode.string) (Just "")
        |> optional "author" (Decode.maybe Decode.string) (Just "")
        |> optional "title" (Decode.maybe Decode.string) (Just "")
        |> optional "publisher" (Decode.maybe Decode.string) (Just "")
        |> optional "image" (Decode.maybe decodeMicrolinkDataImage) Nothing
        |> optional "description" (Decode.maybe Decode.string) (Just "")
        |> optional "date" (Decode.maybe Decode.string) (Just "")
        |> optional "logo" (Decode.maybe decodeMicrolinkDataImage) Nothing
        |> required "url" Decode.string
