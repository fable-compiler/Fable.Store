[<RequireQualifiedAccessAttribute>]
module Store

open System
open Fable
open ElmishStore

let subscribeImmediate (cb: 'Value -> unit) (obs: IObservable<'Value>) =
    let mutable initValue = None
    let disp = obs.Subscribe(fun v ->
        match initValue with
        | None -> initValue <- Some v
        | Some _ -> cb v)

    match initValue with
    | None -> failwith "Observable doesn't report value immediately upon subscription"
    | Some v -> v, disp

let makeWithCons (init: 'Props -> 'Value)
                 (dispose: 'Value -> unit)
                 (cons: StoreCons<'Value, 'Store>)
                 (props: 'Props): 'Store =
    let value = init props
    let store, _ = cons value dispose
    store

let private makeCons i d =
    let s = new Store<_>(i, d)
    s :> IStore<'Value>, s.Update

let make (init: 'Props -> 'Value) (dispose: 'Value -> unit) (props: 'Props): IStore<'Value> =
    makeWithCons init dispose makeCons props

let makeElmishWithCons (init: 'Props -> 'Value * Cmd<'Value, 'Msg>)
                       (update: 'Msg -> 'Value -> 'Value * Cmd<'Value, 'Msg>)
                       (dispose: 'Value -> unit)
                       (cons: StoreCons<'Value, 'Store>)
                       (props: 'Props): 'Store * Dispatch<'Msg> =

    let mutable _cmdHandler = Unchecked.defaultof<Helpers.CmdHandler<'Value, 'Msg>>

    let initValue, initCmd = init props

    let store, storeUpdate = cons initValue (fun m ->
        _cmdHandler.Dispose()
        dispose m)

    let dispatch msg =
        let mutable _cmds = []
        storeUpdate(fun model ->
            let model, cmds = update msg model
            _cmds <- cmds
            model)
        _cmdHandler.Handle _cmds

    _cmdHandler <- Helpers.cmdHandler storeUpdate dispatch
    _cmdHandler.Handle initCmd

    store, dispatch

let makeElmish (init: 'Props -> 'Value * Cmd<'Value, 'Msg>)
               (update: 'Msg -> 'Value -> 'Value * Cmd<'Value, 'Msg>)
               (dispose: 'Value -> unit)
               (props: 'Props): IStore<'Value> * Dispatch<'Msg> =

    makeElmishWithCons init update dispose makeCons props
