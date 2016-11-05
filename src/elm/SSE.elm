port module SSE exposing ( SsEvent
                         , listenForMessageEvents
                         , listenForTypedEvents
                         , events
                         , stopMessageEvents
                         , stopTypedEvents
                         )

type alias SsEvent =
    { data : String
    , eventType : String
    , id : Maybe String
    }

port listenForMessageEvents : String -> Cmd msg

port listenForTypedEvents : (String, String) -> Cmd msg

port stopMessageEvents : () -> Cmd msg

port stopTypedEvents : String -> Cmd msg

{-| A subscription of SSE events
-}
port events : (SsEvent -> msg) -> Sub msg
