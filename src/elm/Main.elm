port module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import String
import Json.Decode.Extra
import Maybe.Extra

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
    { point2d: Maybe String
    , point3d: Maybe String
    , generic: Maybe String
    , sse: SseAccess Msg
    }

init: (Model, Cmd Msg)
init =
    let
        sseAccess = SSE.create sseEndpoint Noop
    in
        ( Model Nothing Nothing Nothing sseAccess , Cmd.none )

points2dEventType: EventType
points2dEventType = "2dpoint"

points3dEventType: EventType
points3dEventType = "3dpoint"

genericEvents: EventType
genericEvents = "message"

-- Update

type Msg = New2dPoint String
    | New3dPoint String
    | GenericEvent String
    | ListenFor2dPoints
    | ListenFor3dPoints
    | ListenForGenericEvents
    | NoMore2dPoints
    | NoMore3dPoints
    | NoMoreGenericEvents
    | Noop

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        New2dPoint text -> ({ model | point2d = Just text }, Cmd.none)

        New3dPoint text -> ({ model | point3d = Just text }, Cmd.none)

        GenericEvent text -> ({ model | generic = Just text }, Cmd.none)

        ListenFor2dPoints ->
            let
                decoder = \ev -> New2dPoint ev.data
            in
                setSseAndDo model (withListener points2dEventType decoder)

        ListenFor3dPoints ->
            let
                decoder = \ev -> New3dPoint ev.data
            in
                setSseAndDo model (withListener points3dEventType decoder)

        ListenForGenericEvents ->
            let
                untypedDecoder =  \ev -> GenericEvent ev.data
            in
                setSseAndDo model (withUntypedListener untypedDecoder)

        NoMore2dPoints ->
            setSseAndDo model (withoutListener points2dEventType)

        NoMore3dPoints ->
            setSseAndDo model (withoutListener points3dEventType)

        NoMoreGenericEvents ->
            setSseAndDo model withoutUntypedListener

        Noop -> (model, Cmd.none) -- Silently drop events we don't know about. Can't happen anyway.


setSseAndDo: Model -> (SseAccess Msg -> (SseAccess Msg, Cmd Msg)) -> (Model, Cmd Msg)
setSseAndDo model f =
    let
        (sse, cmd) = f model.sse
    in
        ( {model | sse = sse }
        , cmd
        )

-- View

view: Model -> Html Msg
view model =
    div []
        [ p [] [text <| Maybe.withDefault "No 2d point yet" (Maybe.map (String.append "Last 2d point: ") model.point2d) ]
        , p [] [text <| Maybe.withDefault "No 3d point yet" (Maybe.map (String.append "Last 3d point: ") model.point3d) ]
        , p [] [text <| Maybe.withDefault "No generic event yet" (Maybe.map (String.append "Last generic event: ") model.generic) ]
        , sseButtons model
        ]

sseButtons : Model -> Html Msg
sseButtons model =
    div [] <| List.map (\f -> f model) [ textButton, points2dButton, points3dButton ]

textButton: Model -> Html Msg
textButton model =
    eventListeningToggleButton (listeningForGeneric  model ) "Listen for text events" "Stop listening for text events" ListenForGenericEvents NoMoreGenericEvents

points2dButton: Model -> Html Msg
points2dButton model =
    eventListeningToggleButton (listeningFor points2dEventType model) "Listen for 2d points" "Stop listening for 2d points" ListenFor2dPoints NoMore2dPoints

points3dButton: Model -> Html Msg
points3dButton model =
    eventListeningToggleButton (listeningFor points3dEventType model) "Listen for 3d points" "Stop listening for 3d points" ListenFor3dPoints NoMore3dPoints

listeningFor: EventType -> Model -> Bool
listeningFor eventType model =
    SSE.hasListenerFor eventType model.sse

listeningForGeneric: Model -> Bool
listeningForGeneric model =
    Maybe.Extra.isJust model.sse.untypedDecoder

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
    serverSideEvents model.sse
