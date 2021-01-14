[<RequireQualifiedAccess>]
module SvelteStore

open System
open Fable.Core

type Subscribe<'Value> = 'Value -> unit
type Dispose = delegate of unit -> unit

type IReadableStore<'Value> =
    abstract subscribe: Subscribe<'Value> -> Dispose

type IWritableStore<'Value> =
    inherit IReadableStore<'Value>
    abstract update: ('Value -> 'Value) -> unit
    abstract set: 'Value -> unit

type Initialize<'Props, 'Value> = delegate of 'Props -> IReadableStore<'Value>

[<Import("readable", from="svelte/store")>]
let private makeReadableStore (init: 'Value) (start: ('Value -> unit) -> Dispose): IReadableStore<'Value> = jsNative

[<Import("writable", from="svelte/store")>]
let private makeWritableStore (init: 'Value) (start: ('Value -> unit) -> Dispose): IWritableStore<'Value> = jsNative

let private storeCons value dispose =
    let mutable store = Unchecked.defaultof<IWritableStore<'Value>>
    store <- makeWritableStore value (fun _set ->
        Dispose(fun () -> store.update(fun model ->
            dispose model
            model)))
    store, store.update

let make init dispose props: IWritableStore<'Model> =    
    Store.makeWithCons init dispose storeCons props

let makeRec (init: IWritableStore<'Model> -> 'Props -> 'Model * IDisposable) =
    fun (props: 'Props) ->
        let mutable store = Unchecked.defaultof<IWritableStore<'Model>>
        store <- makeWritableStore Unchecked.defaultof<'Model> (fun set ->
            let v, disp = init store props
            set v
            Dispose(fun () -> disp.Dispose()))
        store

let makeElmish (init: 'Props -> 'Value * Store.Cmd<'Msg>)
               (update: 'Msg -> 'Value -> 'Value * Store.Cmd<'Msg>)
               (dispose: 'Value -> unit)
               (props: 'Props): IWritableStore<'Value> * Store.Dispatch<'Msg> =

    Store.makeElmishWithCons init update dispose storeCons props

let makeElmishSimple (init: 'Props -> 'Value)
                     (update: 'Msg -> 'Value -> 'Value)
                     (dispose: 'Value -> unit)
                     (props: 'Props): IWritableStore<'Value> * Store.Dispatch<'Msg> =

    Store.makeElmishWithCons
        (fun p -> init p, [])
        (fun msg model -> update msg model, [])
        dispose
        storeCons
        props

[<Fable.SveltePlugins.Dispatcher>]
let makeDispatcher (dispatch: 'Msg -> unit): obj = failwith "never"
