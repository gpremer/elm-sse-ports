/*jslint browser: true*/
/*global EventSource*/
/*exported initElmPorts*/
function initElmPorts(app) {
    var sources = {};

    app.ports.listenForTypedEvents.subscribe(function (addressAndEventType) {
        var address = addressAndEventType[0];
        var eventType = addressAndEventType[1];
        if (!sources[eventType]) {
            console.log("Subscribing for events on " + address + " of type " + eventType);
            sources[eventType] = new EventSource(address);
            sources[eventType].addEventListener(eventType, function (event) {
                app.ports.events.send({data: event.data || '', eventType: eventType, id: event.id || ''});
            });
        }
    });
    app.ports.stopListeningForTypedEvents.subscribe(function (eventType) {
        var source = sources[eventType];
        if (source) {
            source.close();
            delete sources[eventType];
        }
    });
}
