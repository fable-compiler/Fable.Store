import { Readable } from "svelte/store";

export function makeStore(): [Readable<{
    position: [number, number],
    offset: [number, number],
}>, {
    mouseDown(x: number, y: number, offsetX: number, offsetY: number): void,
    mouseMove(x: number, y: number): void,
    mouseUp(): void,
}];