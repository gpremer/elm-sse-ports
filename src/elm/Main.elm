port module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import String

import SSE exposing (..)

main =
    App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions}

sseEndpoint = "http://localhost:8080/events"

-- model

type alias Model =
    { custom: Maybe String
    , generic: Maybe String
    }

init: (Model, Cmd Msg)
init =
    ( Model Nothing Nothing, Cmd.none )

-- Update

type Msg = CustomEvent String
    | GenericEvent String
    | ListenForCustomEvents
    | ListenForGenericEvents
    | UnknownEvent


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        CustomEvent text -> ({ model | custom = Just text }, Cmd.none)

        GenericEvent text -> ({ model | generic = Just text }, Cmd.none)

        ListenForCustomEvents -> (model, listenForEvents sseEndpoint "custom")

        ListenForGenericEvents -> (model, listenForMessageEvents sseEndpoint)

        UnknownEvent -> (model, Cmd.none)

-- View

view: Model -> Html Msg
view model =
    div []
        [ p [] [text <| Maybe.withDefault "No custom event yet" (Maybe.map (String.append "Last custom event: ") model.custom) ]
        , p [] [text <| Maybe.withDefault "No generic event yet" (Maybe.map (String.append "Last generic event: ") model.generic) ]
        , p [] [button [onClick ListenForCustomEvents] [text "Listen for custom events"]]
        , p [] [button [onClick ListenForGenericEvents] [text "Listen for generic events"]]
        ]

-- Subscriptions

subscriptions: Model -> Sub Msg
subscriptions model =
    events eventByType

eventByType: SsEvent -> Msg
eventByType ssEvent =
    case ssEvent.eventType of
        "custom" -> CustomEvent ssEvent.data

        "message" -> GenericEvent ssEvent.data

        _ -> UnknownEvent
