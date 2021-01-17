import { Readable } from "svelte/store";

export function makeStore(txt: string): Readable<{
    letters: Iterable<[number, {
        char: string,
        x: number,
        y: number,        
    }]>
    fps: number,
    second: number,
    count: number,
}>;
