[<RequireQualifiedAccessAttribute>]
module Store

open System

type Update<'Value> = ('Value -> 'Value) -> unit
type Dispatch<'Msg> = 'Msg -> unit
type Cmd<'Value, 'Msg> = ((Update<'Value> * Dispatch<'Msg>) -> unit) list

type IStore<'Value> =
    inherit IDisposable
    inherit IObservable<'Value>
    abstract Update: f:('Value -> 'Value) -> unit

module internal Helpers =
    type CmdHandler<'Value, 'Msg>(handler, ?dispose) =
        member _.Handle(cmd: Cmd<'Value, 'Msg>): unit = handler cmd
        member _.Dispose() = match dispose with Some d -> d () | None -> ()
        interface IDisposable with
            member this.Dispose() = this.Dispose()

    let disposable f =
        { new IDisposable with
            member _.Dispose() = f () }

#if FABLE_COMPILER
    open Fable.Core

    [<Emit("$0 === $1")>]
    let fastEquals (x: 'T) (y: 'T): bool = jsNative

    let cmdHandler (update: Update<'Value>) (dispatch: 'Msg -> unit): CmdHandler<'Value, 'Msg> =
        new CmdHandler<_, _>(List.iter (fun cmd ->
            JS.setTimeout (fun _ -> cmd (update, dispatch)) 0 |> ignore))
#else
    let fastEquals (x: 'T) (y: 'T): bool = Unchecked.equals x y

    let cmdHandler (update: Update<'Value>) (dispatch: 'Msg -> unit): CmdHandler<'Value, 'Msg> =
        let cts = new Threading.CancellationTokenSource()

        let mb = MailboxProcessor.Start(fun inbox -> async {
            while true do
                match! inbox.Receive() with
                | Choice1Of2 updater -> update updater
                | Choice2Of2 msg -> dispatch msg
        }, cts.Token)

        new CmdHandler<_,_>(List.iter (fun cmd -> cmd ((Choice1Of2 >> mb.Post), (Choice2Of2 >> mb.Post))), fun _ -> cts.Cancel())
#endif

type Store<'Value>(initValue: 'Value,  ?dispose: 'Value -> unit) =
    let mutable _uid = 0
    let mutable _disposed = false
    let mutable _value = initValue
    let subscribers = Collections.Generic.Dictionary<_, IObserver<'Value>>()

    member _.Dispose() =
        if not _disposed then
            subscribers.Clear()
            match dispose with Some d -> d _value | None -> ()

    member _.Update(f: 'Value -> 'Value) =
        _value <- f _value
        subscribers.Values
        |> Seq.iter (fun s -> s.OnNext(_value))

    member this.Subscribe(observer: IObserver<'Value>): IDisposable =
        if _disposed then
            failwith "The store has already been disposed"
        else
            // Immediately report current value
            observer.OnNext(_value)

            let id = _uid
            _uid <- _uid + 1
            subscribers.Add(id, observer)

            { new IDisposable with
                member _.Dispose() =
                    if subscribers.Remove(id) && subscribers.Count = 0 then
                        this.Dispose() }

    interface IStore<'Value> with
        member this.Dispose() = this.Dispose()
        member this.Update(f) = this.Update(f)
        member this.Subscribe(observer: IObserver<'Value>) = this.Subscribe(observer)

let subscribeImmediate (cb: 'Value -> unit) (obs: IObservable<'Value>) =
    let mutable initValue = None
    let disp = obs.Subscribe(fun v ->
        match initValue with
        | None -> initValue <- Some v
        | Some _ -> cb v)

    match initValue with
    | None -> failwith "Observable doesn't report value immediately upon subscription"
    | Some v -> v, disp

type StoreCons<'Value, 'Store> = 'Value -> ('Value -> unit) -> 'Store * Update<'Value>

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
