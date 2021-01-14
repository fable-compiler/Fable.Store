import { Readable } from "svelte/store";

export const store: Readable<{
    position: [number, number],
    offset: [number, number],
}>;

export const dispatch: {
    mouseDown: (x: number, y: number, offsetX: number, offsetY: number) => void,
    mouseMove: (x: number, y: number) => void,
    mouseUp: () => void,
}
