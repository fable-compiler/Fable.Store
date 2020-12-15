<script>
	// @ts-check
	import { store, dispatch } from "./App";
</script>

<style>
	.reaction-app {
		font-family: Consolas, monospace;
		height: 100px;
	}
	.reaction-app span {
		position: fixed;
	}

	input {
		width: 300px;
	}
</style>

<div class="reaction-app">
	{#each [...$store.letters] as [idx, letter] (idx)}
		<span style="top:{letter.y}px; left:{letter.x}px">{letter.char}</span>
	{/each}
	<p>fps: {$store.fps}</p>
	<input
		value={$store.message}
		on:change={(ev) => {
			// @ts-ignore
			const value = ev.target.value;
			dispatch.message(value);
		}} />	
</div>
