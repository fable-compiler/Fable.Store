module App

open Feliz
open Feliz.Router
open Feliz.Svelte

[<SvelteComponent "./TodoMvc.svelte">]
let TodoMvc() = Html.none

[<SvelteComponent "./Dragging.svelte">]
let Dragging() = Html.none

[<ReactComponent>]
let Index() = React.fragment [
    Html.h1 "Feliz ❤️ Svelte"
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
