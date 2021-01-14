<script>
	// @ts-check
	import { makeStore } from "./Dragging";

	let [store, dispatch] = makeStore();

	$: left = $store.position[0] - $store.offset[0];
	$: top = $store.position[1] - $store.offset[1];
</script>

<!-- Add mouse move/up events to page body because sometimes cursor leaves
	the boundaries of the rectangle while dragging -->
<svelte:body
	on:mousemove={(ev) => dispatch.mouseMove(ev.x, ev.y)}
	on:mouseup={(_) => dispatch.mouseUp()} />

<svg style="position:fixed; left:{left}px; top:{top}px">
	<rect
		width="100"
		height="100"
		style="fill:rgb(0, 255, 33)"
		on:mousedown={(ev) => {
			// @ts-ignore
			const rect = ev.target.getBoundingClientRect();
			dispatch.mouseDown(ev.x, ev.y, ev.x - rect.left, ev.y - rect.top);
		}}
	/>
</svg>
