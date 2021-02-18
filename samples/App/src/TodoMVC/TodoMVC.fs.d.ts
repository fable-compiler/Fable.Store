import { Readable } from "svelte/store";

export function makeStore(): [Readable<{
    entries: {
        description: string,
        completed: boolean,
        id: string,
    }[],
}>, {
    add(text: string): void,
    delete(id: string): void,
    check(id: string, completed: boolean): void,
}];