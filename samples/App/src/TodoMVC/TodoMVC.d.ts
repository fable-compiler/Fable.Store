import { Readable } from "svelte/store";

export interface Entry {
    description: string,
    completed: boolean,
    id: string,
}

export function makeStore(): [Readable<{
    entries: Entry[],
}>, {
    add(text: string): void,
    delete(id: string): void,
    check(id: string, completed: boolean): void,
}]
