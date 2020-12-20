<script>
	// @ts-check
	import { store } from "./App";
	export let text;
	let model = store(text)
</script>

<style>
	.reaction-app {
		font-family: Consolas, monospace;
		height: 100px;
	}
	.reaction-app span {
		position: fixed;
	}
</style>

<div class="reaction-app">
	{#each [...$model.letters] as [idx, letter] (idx)}
		<span style="top:{letter.y}px; left:{letter.x}px">{letter.char}</span>
	{/each}
	<p>fps: {$model.fps}</p>
</div>
