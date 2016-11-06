port module SSE exposing ( SsEvent
                         , listenForMessageEvents
                         , listenForEvents
                         , events
                         , stopMessageEvents
                         , stopTypedEvents
                         )

type alias SsEvent =
    { data : String
    , eventType : String
    , id : Maybe String
    }

listenForMessageEvents: String -> Cmd msg
listenForMessageEvents endpoint =
    listenForTypedEvents (endpoint, "message")

port listenForTypedEvents : (String, String) -> Cmd msg

{-| Listen for SSE events of a given event type by subscribing to the given endpoint.
-}
listenForEvents : String -> String -> Cmd msg
listenForEvents endpoint eventTypeName =
    listenForTypedEvents (endpoint, eventTypeName)

port stopMessageEvents : () -> Cmd msg

port stopTypedEvents : String -> Cmd msg

{-| A subscription of SSE events. You should probably transform the SsEvent to an instance of a type o fyour own domain.
-}
port events : (SsEvent -> msg) -> Sub msg
