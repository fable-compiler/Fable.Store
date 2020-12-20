import { Readable } from "svelte/store";

export function store(txt: string): Readable<{
    letters: Iterable<[number, {
        char: string,
        x: number,
        y: number,        
    }]>
    fps: number,
    second: number,
    count: number,
}>;
