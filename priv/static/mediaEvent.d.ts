export declare type SerializedMediaEvent = string;
export interface MediaEvent {
    type: string;
    key?: string;
    data?: any;
}
export declare function serializeMediaEvent(mediaEvent: MediaEvent): SerializedMediaEvent;
export declare function deserializeMediaEvent(serializedMediaEvent: SerializedMediaEvent): MediaEvent;
export declare function generateMediaEvent(type: string, data?: any): MediaEvent;
//# sourceMappingURL=mediaEvent.d.ts.map