module Fable.SvelteStore

open System
open Fable.Core
open Fable.Store

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

let makeSvelteStore init dispose: 'Props -> WritableStore<'Model> =
    makeStoreWithCons init dispose (fun init dispose ->
        let mutable store = Unchecked.defaultof<WritableStore<'Model>>
        store <- makeWritableStore Unchecked.defaultof<_> (fun set ->
            init() |> set
            Dispose(fun () -> store.update(fun model ->
                dispose model
                model)))
        store, store.update
    )
