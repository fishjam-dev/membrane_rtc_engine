export type SerializedMediaEvent = string;

export interface MediaEvent {
  type: string;
  key?: string;
  data?: any;
}

export function serializeMediaEvent(mediaEvent: MediaEvent): SerializedMediaEvent {
  return JSON.stringify(mediaEvent);
}

export function deserializeMediaEvent(serializedMediaEvent: SerializedMediaEvent): MediaEvent {
  return JSON.parse(serializedMediaEvent) as MediaEvent;
}

export function generateMediaEvent(type: string, data?: any): MediaEvent {
  var event: MediaEvent = { type };
  if (data) {
    event = { ...event, data };
  }
  return event;
}
