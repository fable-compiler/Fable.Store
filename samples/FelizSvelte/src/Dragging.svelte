<script>
    // @ts-check
    import { store, dispatch } from "./Dragging.fs.js";

    /** @type SVGSVGElement */
    let svg;

    $: left = $store.position[0] - $store.offset[0];
    $: top = $store.position[1] - $store.offset[1];
</script>

<svelte:body
    on:mousedown={(ev) => {
        const rect = svg.getBoundingClientRect();
        dispatch.mouseDown(ev.x, ev.y, ev.x - rect.left, ev.y - rect.top);
    }}
    on:mousemove={(ev) => dispatch.mouseMove(ev.x, ev.y)}
    on:mouseup={(_) => dispatch.mouseUp()} />

<svg bind:this={svg} style="position:fixed; left:{left}px; top:{top}px">
    <rect width="100" height="100" style="fill:rgb(0, 255, 33)" />
</svg>
