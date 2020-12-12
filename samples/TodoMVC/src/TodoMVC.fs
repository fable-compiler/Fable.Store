module TodoMVC

open Fable.Core
// open Browser.Types
// open Browser

[<StringEnum>]
type WhatIsVisible =
   | All
   | Active
   | Completed

// MODEL
type Entry =
    { description : string
      completed : bool
      editing : bool
      id : int }

// The full application state of our todo app.
type Model =
    { entries : Entry[]
      uid : int
      visibility : WhatIsVisible }

let emptyModel () =
    { entries = [||]
      visibility = All
      uid = 0 }

let newEntry desc id =
  { description = desc
    completed = false
    editing = false
    id = id }

// UPDATE

(** Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
*)
type Msg =
    | Edit of id: int * isEditing: bool
    | Update of id: int * text: string
    | Add of text: string
    | Delete of id: int
    | DeleteComplete
    | Check of id: int * completed: bool
    | CheckAll of completed: bool
    | ChangeVisibility of visibility: WhatIsVisible

// How we update our Model on a given Msg?
let update (msg:Msg) (model:Model) =
    match msg with
    | Add field ->
        let xs = if System.String.IsNullOrEmpty field then
                    model.entries
                 else
                    Array.append model.entries [|newEntry field model.uid|]
        { model with
            uid = model.uid + 1
            entries = xs }

    | Edit (id,isEditing) ->
        let updateEntry t =
          if t.id = id then { t with editing = isEditing } else t
        { model with entries = Array.map updateEntry model.entries }

    | Update (id,task) ->
        let updateEntry t =
          if t.id = id then { t with description = task; editing = false } else t
        { model with entries = Array.map updateEntry model.entries }

    | Delete id ->
        { model with entries = Array.filter (fun t -> t.id <> id) model.entries }

    | DeleteComplete ->
        { model with entries = Array.filter (fun t -> not t.completed) model.entries }

    | Check (id,isCompleted) ->
        let updateEntry t =
          if t.id = id then { t with completed = isCompleted } else t
        { model with entries = Array.map updateEntry model.entries }

    | CheckAll isCompleted ->
        let updateEntry t = { t with completed = isCompleted }
        { model with entries = Array.map updateEntry model.entries }

    | ChangeVisibility visibility ->
        { model with visibility = visibility }


let store = emptyModel() |> Fable.Svelte.makeStore
let dispatch = Fable.Svelte.makeDispatcher update store
