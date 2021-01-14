module App

open Feliz
open Feliz.Router
open Feliz.Svelte

[<SvelteComponent "./TodoMVC.svelte">]
let TodoMvc() = React.imported()

[<SvelteComponent "./Dragging.svelte">]
let Dragging() = React.imported()

[<ReactComponent>]
let Index() = React.fragment [
    Html.h1 "Feliz ❤️ Svelte"
    Html.p "Easily embed Svelte components inside Feliz applications"
    Html.ul [
        Html.li [
            Html.a [
                prop.href (Router.format "todo")
                prop.text "/todo"
            ]
        ]

        Html.li [
            Html.a [
                prop.href (Router.format "dragging")
                prop.text "/dragging"
            ]
        ]
    ]
    TimeFliesElmish.TimeFliesElmish "Time flies, hurry up!"
]

[<ReactComponent>]
let Application() =
    let currentUrl, setUrl = React.useState(Router.currentUrl())
    React.router [
        router.onUrlChanged setUrl
        router.children [
            match currentUrl with
            | [ ] -> Index()
            | [ "todo" ] -> TodoMvc()
            | [ "dragging" ] -> Dragging()
            | _ -> Html.h1 "Not found"
        ]
    ]
