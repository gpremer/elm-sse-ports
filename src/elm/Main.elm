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
    , listeningForCustom: Bool
    , listeningForGeneric: Bool
    }

init: (Model, Cmd Msg)
init =
    ( Model Nothing Nothing False False, Cmd.none )

-- Update

type Msg = CustomEvent String
    | GenericEvent String
    | ListenForCustomEvents
    | ListenForGenericEvents
    | NoMoreCustomEvents
    | NoMoreGenericEvents
    | UnknownEventType


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        CustomEvent text -> ({ model | custom = Just text }, Cmd.none)

        GenericEvent text -> ({ model | generic = Just text }, Cmd.none)

        ListenForCustomEvents -> ({model | listeningForCustom = True}, listenForEvents sseEndpoint "custom")

        ListenForGenericEvents -> ({model | listeningForGeneric = True}, listenForMessageEvents sseEndpoint)

        NoMoreCustomEvents -> ({model | listeningForCustom = False}, stopListeningForTypedEvents "custom")

        NoMoreGenericEvents -> ({model | listeningForGeneric = False}, stopListeningForTypedEvents "message")

        UnknownEventType -> (model, Cmd.none) -- Silently drop events we don't know about. Can't happen anyway.

-- View

view: Model -> Html Msg
view model =
    div []
        [ p [] [text <| Maybe.withDefault "No custom event yet" (Maybe.map (String.append "Last custom event: ") model.custom) ]
        , p [] [text <| Maybe.withDefault "No generic event yet" (Maybe.map (String.append "Last generic event: ") model.generic) ]
        , p [] [eventListeningToggleButton model.listeningForCustom "Listen for custom events" "Stop listening for custom events" ListenForCustomEvents NoMoreCustomEvents]
        , p [] [eventListeningToggleButton model.listeningForGeneric "Listen for generic events" "Stop listening for generic events" ListenForGenericEvents NoMoreGenericEvents]
        ]

eventListeningToggleButton: Bool -> String -> String -> Msg -> Msg -> Html Msg
eventListeningToggleButton on offText onText offMsg onMsg =
    let
        (txt, msg) =
            if on then
                (onText, onMsg)
            else
                (offText, offMsg)
    in
        button [onClick msg] [text txt]

-- Subscriptions

subscriptions: Model -> Sub Msg
subscriptions model =
    events eventByType

eventByType: SsEvent -> Msg
eventByType ssEvent =
    case ssEvent.eventType of
        "custom" -> CustomEvent ssEvent.data

        "message" -> GenericEvent ssEvent.data

        _ -> UnknownEventType
