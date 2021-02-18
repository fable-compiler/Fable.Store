import { Readable } from "svelte/store";

export function makeStore(a0: string): Readable<{
    letters: Iterable<[number, {
        char: string,
        x: number,
        y: number,
    }]>,
    fps: number,
    second: number,
    count: number,
    text: string,
}>;