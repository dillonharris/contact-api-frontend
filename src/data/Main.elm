module Contact exposing (..)

import Html exposing (Html, div, text)
import Json.Decode exposing (Decoder, andThen, string, succeed)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (Value, object)


type alias Model =
    { title : Title
    , name : String
    , surname : String
    , nickname : String
    , website : String
    , relationship : Relationship
    , other : String
    }


type Title
    = Mr
    | Mrs
    | Ms


type Relationship
    = Family
    | Friend
    | Colleague
    | Other


type Msg
    = NoOp
    | SetTitle
    | SetName
    | SetSurname
    | SetNickname
    | SetWebsite
    | SetRelationship
    | SetOther


initialModel : Model
initialModel =
    { title = Mr
    , name = ""
    , surname = ""
    , nickname = ""
    , website = ""
    , relationship = Friend
    , other = ""
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


view : Model -> Html Msg
view model =
    div [] [ text "" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTitle ->
            ( { model | title = model.title }, Cmd.none )

        SetName ->
            ( { model | name = model.name }, Cmd.none )

        SetSurname ->
            ( model, Cmd.none )

        SetNickname ->
            ( model, Cmd.none )

        SetWebsite ->
            ( model, Cmd.none )

        SetRelationship ->
            ( model, Cmd.none )

        SetOther ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


titleDecoder : Decoder Title
titleDecoder =
    string
        |> andThen
            (\title ->
                let
                    titletoLower =
                        String.toLower title
                in
                case titletoLower of
                    "mr" ->
                        succeed Mr

                    "mrs" ->
                        succeed Mrs

                    "ms" ->
                        succeed Ms

                    _ ->
                        succeed Mr
            )


relationshipDecoder : Decoder Relationship
relationshipDecoder =
    string
        |> andThen
            (\relationship ->
                let
                    relationshipToLower =
                        String.toLower relationship
                in
                case relationshipToLower of
                    "family" ->
                        succeed Family

                    "friend" ->
                        succeed Friend

                    "colleague" ->
                        succeed Colleague

                    "other" ->
                        succeed Other

                    _ ->
                        succeed Other
            )


contactDecoder : Decoder Model
contactDecoder =
    decode Model
        |> required "title" titleDecoder
        |> required "name" string
        |> required "surname" string
        |> required "nickname" string
        |> required "website" string
        |> required "relationship" relationshipDecoder
        |> required "other" string


contactTitleEncoder : Model -> String
contactTitleEncoder model =
    case model.title of
        Mr ->
            "mr"

        Mrs ->
            "mr"

        Ms ->
            "mr"


relationshipEncoder : Model -> String
relationshipEncoder model =
    case model.relationship of
        Family ->
            "family"

        Friend ->
            "friend"

        Colleague ->
            "colleague"

        Other ->
            "other"


contactEncoder : Model -> Value
contactEncoder model =
    object
        [ ( "title", Encode.string <| contactTitleEncoder model )
        , ( "name", Encode.string model.name )
        , ( "surname", Encode.string model.surname )
        , ( "nickname", Encode.string model.nickname )
        , ( "website", Encode.string model.website )
        , ( "relationship", Encode.string <| relationshipEncoder model )
        , ( "other", Encode.string model.other )
        ]
