<script>
	import TodoMVC from "./TodoMVC/TodoMVC.svelte";
	import Dragging from "./Dragging/Dragging.svelte";
	import TimeFlies from "./TimeFlies/TimeFlies.svelte";

	const STORAGE_KEY = "svelte-selected-app";
	let selected = localStorage.getItem(STORAGE_KEY) || "TodoMVC";
	$: localStorage.setItem(STORAGE_KEY, selected);
</script>

<select bind:value={selected}>
	<option value="TodoMVC">TodoMVC</option>
	<option value="Dragging">Dragging</option>
	<option value="TimeFlies">TimeFlies</option>
</select>

{#if selected === "Dragging"}
	<Dragging />
{:else if selected === "TimeFlies"}
	<TimeFlies text="Time flies, hurry up!" />
{:else}
	<TodoMVC />
{/if}

