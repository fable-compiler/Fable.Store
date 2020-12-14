module Fable.Svelte

open System
open Fable.AST
open Fable.Core

type Subscribe<'Model> = 'Model -> unit
type Dispose = delegate of unit -> unit

type Updatable<'Model> =
    abstract update: ('Model -> 'Model) -> unit

type ReadableStore<'Model> =
    abstract subscribe: Subscribe<'Model> -> Dispose

type WritableStore<'Model> =
    inherit ReadableStore<'Model>
    inherit Updatable<'Model>
    abstract set: 'Model -> unit

type Dispatcher<'Msg> =
    interface end

[<Import("readable", from="svelte/store")>]
let makeReadableStore (init: 'Model) (start: ('Model -> unit) -> Dispose): ReadableStore<'Model> = jsNative

[<Import("writable", from="svelte/store")>]
let makeWritableStore (init: 'Model) (start: ('Model -> unit) -> Dispose): WritableStore<'Model> = jsNative

let inline makeStore (init: unit->'Model) (dispose: unit->unit) =
    makeWritableStore Unchecked.defaultof<_> (fun set ->
        init() |> set
        Dispose(fun () -> dispose()))

[<SveltePlugins.Dispatcher>]
let __makeDispatcher (dispatch: 'Msg -> unit): Dispatcher<'Msg> = failwith "never"

let inline makeDispatcher (update: 'Msg -> 'Model -> 'Model) (updatable: Updatable<'Model>): Dispatcher<'Msg> =
    __makeDispatcher (fun msg -> updatable.update(update msg))
