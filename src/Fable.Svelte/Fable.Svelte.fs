module Fable.Svelte

open System
open Fable.AST
open Fable.Core

type Subscription<'Model> = 'Model -> unit
type Disposable = unit -> unit

type Store<'Model> =
    abstract subscribe: Subscription<'Model> -> Disposable

type WritableStore<'Model> =
    inherit Store<'Model>
    abstract update: ('Model -> 'Model) -> unit

type Dispatcher<'Msg> =
    interface end

[<Import("writable", from="svelte/store")>]
let makeStore (init: 'Model): WritableStore<'Model> = jsNative

[<Fable.SveltePlugins.Dispatcher>]
let makeDispatcher (update: 'Msg -> 'Model -> 'Model) (store: WritableStore<'Model>): Dispatcher<'Msg> =
    obj() :?> _
