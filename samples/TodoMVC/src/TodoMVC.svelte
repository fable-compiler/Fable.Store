<script>
	import { store, dispatch } from "./TodoMVC.fs.js";
	import { quintOut } from "svelte/easing";
	import { crossfade } from "svelte/transition";
	import { flip } from "svelte/animate";

	const [send, receive] = crossfade({
		fallback(node, params) {
			const style = getComputedStyle(node);
			const transform = style.transform === "none" ? "" : style.transform;

			return {
				duration: 600,
				easing: quintOut,
				css: (t) => `
					transform: ${transform} scale(${t});
					opacity: ${t}
				`,
			};
		},
	});
</script>

<style>
	.new-todo {
		font-size: 1.4em;
		width: 100%;
		margin: 2em 0 1em 0;
	}

	.board {
		max-width: 36em;
		margin: 0 auto;
	}

	.left,
	.right {
		float: left;
		width: 50%;
		padding: 0 1em 0 0;
		box-sizing: border-box;
	}

	h2 {
		font-size: 2em;
		font-weight: 200;
		user-select: none;
	}

	label {
		top: 0;
		left: 0;
		display: block;
		font-size: 1em;
		line-height: 1;
		padding: 0.5em;
		margin: 0 auto 0.5em auto;
		border-radius: 2px;
		background-color: #eee;
		user-select: none;
	}

	input {
		margin: 0;
	}

	.right label {
		background-color: rgb(180, 240, 100);
	}

	button {
		float: right;
		height: 1em;
		box-sizing: border-box;
		padding: 0 0.5em;
		line-height: 1;
		background-color: transparent;
		border: none;
		color: rgb(170, 30, 30);
		opacity: 0;
		transition: opacity 0.2s;
	}

	label:hover button {
		opacity: 1;
	}
</style>

<div class="board">
	<input
		class="new-todo"
		placeholder="What needs to be done?"
		on:keydown={(ev) => {
			if (ev.key === 'Enter') {
				dispatch.add(ev.target.value);
				ev.target.value = '';
			}
		}} />

	<div class="left">
		<h2>todo</h2>
		{#each $store.entries.filter((t) => !t.completed) as todo (todo.id)}
			<label
				in:receive={{ key: todo.id }}
				out:send={{ key: todo.id }}
				animate:flip>
				<input
					type="checkbox"
					checked={todo.completed}
					on:change={(_) => dispatch.check(todo.id, true)} />
				{todo.description}
				<button on:click={() => dispatch.delete(todo.id)}>x</button>
			</label>
		{/each}
	</div>

	<div class="right">
		<h2>done</h2>
		{#each $store.entries.filter((t) => t.completed) as todo (todo.id)}
			<label
				in:receive={{ key: todo.id }}
				out:send={{ key: todo.id }}
				animate:flip>
				<input
					type="checkbox"
					checked={todo.completed}
					on:change={(_) => dispatch.check(todo.id, false)} />
				{todo.description}
				<button on:click={() => dispatch.delete(todo.id)}>x</button>
			</label>
		{/each}
	</div>
</div>
