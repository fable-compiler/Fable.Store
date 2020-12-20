import App from './App.svelte';

const app = new App({
	target: document.body,
	props: {
		text: "TIME FLIES LIKE AN ARROW",
	},
});

export default app;