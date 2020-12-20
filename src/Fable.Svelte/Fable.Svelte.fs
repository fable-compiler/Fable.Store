module Fable.Svelte

open System
open Fable.AST
open Fable.Core

type Dispatch<'Msg> = 'Msg -> unit

type Cmd<'Msg> = (Dispatch<'Msg> -> unit) list

type Subscribe<'Model> = 'Model -> unit

type Dispose = delegate of unit -> unit

type Dispatcher<'Msg> =
    abstract dispatch: 'Msg -> unit

type ReadableStore<'Model> =
    abstract subscribe: Subscribe<'Model> -> Dispose

type WritableStore<'Model> =
    inherit ReadableStore<'Model>
    abstract update: ('Model -> 'Model) -> unit
    abstract set: 'Model -> unit

type DispatchStore<'Msg, 'Model> =
    inherit WritableStore<'Model>
    inherit Dispatcher<'Msg>

type Initialize<'Props, 'Model> = delegate of 'Props -> ReadableStore<'Model>

[<Import("readable", from="svelte/store")>]
let makeReadableStore (init: 'Model) (start: ('Model -> unit) -> Dispose): ReadableStore<'Model> = jsNative

[<Import("writable", from="svelte/store")>]
let makeWritableStore (init: 'Model) (start: ('Model -> unit) -> Dispose): WritableStore<'Model> = jsNative

let inline makeStore (init: unit->'Model) (dispose: unit->unit) =
    makeWritableStore Unchecked.defaultof<_> (fun set ->
        init() |> set
        Dispose(fun () -> dispose()))

let makeStoreRec (init: WritableStore<'Model> -> 'Props -> 'Model * IDisposable) =
    let mutable _store: WritableStore<_> option = None
    Initialize(fun props ->
        match _store with
        | Some store -> upcast store
        | None ->
            let store = makeWritableStore Unchecked.defaultof<_> (fun set ->
                let model, disp = init _store.Value props
                set model
                Dispose(fun () ->
                    _store <- None
                    disp.Dispose()))
            _store <- Some store
            upcast store)

let makeElmishStore
        (init: unit -> 'Model * Cmd<'Msg>)
        (update: 'Msg -> 'Model -> 'Model * Cmd<'Msg>)
        (dispose: 'Model -> unit) =

    let mutable dispatch = Unchecked.defaultof<_>
    let mutable store: WritableStore<_> = Unchecked.defaultof<_>

    let handleCmds = function
        | [] -> ()
        | cmds ->
            cmds
            |> List.map (fun cmd ->
                Promise.create(fun resolve _reject ->
                    cmd (dispatch >> resolve))
            )
            |> Promise.Parallel
            |> Promise.start

    dispatch <- fun msg ->
        let mutable _cmds = []
        store.update(fun model ->
            let model, cmds = update msg model
            _cmds <- cmds
            model)
        handleCmds _cmds

    store <- makeWritableStore Unchecked.defaultof<_> (fun set ->
        let model, cmds = init()
        set model
        handleCmds cmds
        Dispose(fun () -> store.update(fun model ->
            dispose model
            model)))

    JS.Constructors.Object.assign(store,
        {| dispatch = dispatch |}) :?> DispatchStore<'Msg, 'Model>

[<SveltePlugins.Dispatcher>]
let makeDispatcher (dispatch: 'Msg -> unit): obj = failwith "never"
