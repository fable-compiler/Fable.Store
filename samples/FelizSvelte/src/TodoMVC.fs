module TodoMVC

open System
open Fable.Core
open Browser

type Entry =
    { description : string
      completed : bool
      id : Guid }

type Model =
    { entries : Entry[] }

type Msg =
    | Add of text: string
    | Delete of id: Guid
    | Check of id: Guid * completed: bool

let newEntry desc =
  { description = desc
    completed = false
    id = Guid.NewGuid() }

let init() =
  let todos =
    match localStorage.getItem("svelte-todos") with
    | null | "" -> [||]
    | json -> JS.JSON.parse(json) :?> Entry[]
  { entries = todos }

let saveModel (model: Model) =
  localStorage.setItem("svelte-todos", JS.JSON.stringify(model.entries))
  model

let update (msg:Msg) (model:Model) =
    match msg with
    | Add field ->
        let xs = if String.IsNullOrEmpty field then model.entries
                 else Array.append model.entries [|newEntry field |]
        { model with entries = xs } |> saveModel

    | Delete id ->
        { model with entries = Array.filter (fun t -> t.id <> id) model.entries }
        |> saveModel

    | Check (id,isCompleted) ->
        let updateEntry t =
          if t.id = id then { t with completed = isCompleted } else t
        { model with entries = Array.map updateEntry model.entries }
        |> saveModel

let makeStore () =
  let store, dispatch = SvelteStore.makeElmishSimple init update ignore ()
  store, SvelteStore.makeDispatcher dispatch
