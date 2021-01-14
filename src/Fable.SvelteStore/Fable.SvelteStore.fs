[<RequireQualifiedAccess>]
module SvelteStore

open System
open Fable.Core

type Subscribe<'Model> = 'Model -> unit
type Dispose = delegate of unit -> unit

type ReadableStore<'Model> =
    abstract subscribe: Subscribe<'Model> -> Dispose

type WritableStore<'Model> =
    inherit ReadableStore<'Model>
    abstract update: ('Model -> 'Model) -> unit
    abstract set: 'Model -> unit

type Initialize<'Props, 'Model> = delegate of 'Props -> ReadableStore<'Model>

[<Import("readable", from="svelte/store")>]
let private makeReadableStore (init: 'Model) (start: ('Model -> unit) -> Dispose): ReadableStore<'Model> = jsNative

[<Import("writable", from="svelte/store")>]
let private makeWritableStore (init: 'Model) (start: ('Model -> unit) -> Dispose): WritableStore<'Model> = jsNative

let private storeCons value dispose =
    let mutable store = Unchecked.defaultof<WritableStore<'Model>>
    store <- makeWritableStore value (fun _set ->
        Dispose(fun () -> store.update(fun model ->
            dispose model
            model)))
    store, store.update

let make init dispose: 'Props -> WritableStore<'Model> =
    Store.makeWithCons init dispose storeCons

let makeElmish (init: 'Props -> 'Model * Store.Cmd<'Msg>)
               (update: 'Msg -> 'Model -> 'Model * Store.Cmd<'Msg>)
               (dispose: 'Model -> unit)
               : 'Props -> WritableStore<'Model> * Store.Dispatch<'Msg> =

    Store.makeElmishWithCons init update dispose storeCons
