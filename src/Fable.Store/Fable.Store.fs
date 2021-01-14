[<RequireQualifiedAccessAttribute>]
module Store

open System

type Update<'Value> = ('Value -> 'Value) -> unit
type Dispatch<'Msg> = 'Msg -> unit
type Cmd<'Msg> = (Dispatch<'Msg> -> unit) list

type IStore<'Value> =
    inherit IObservable<'Value>
    abstract Value: 'Value
    abstract Update: f:('Value -> 'Value) -> unit

module internal Helpers =
    type CmdHandler<'Msg>(handler, ?dispose) =
        member _.Handle(cmd: Cmd<'Msg>): unit = handler cmd
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

    let cmdHandler (dispatch: 'Msg -> unit): CmdHandler<'Msg> =
        new CmdHandler<_>(List.iter (fun cmd -> JS.setTimeout (fun _ -> cmd dispatch) 0 |> ignore))
#else
    let fastEquals (x: 'T) (y: 'T): bool = Unchecked.equals x y

    let cmdHandler (dispatch: 'Msg -> unit): CmdHandler<'Msg> =
        let cts = new Threading.CancellationTokenSource()

        let mb = MailboxProcessor.Start(fun inbox -> async {
            while true do
                let! msg = inbox.Receive()
                dispatch msg
        }, cts.Token)

        new CmdHandler<_>(List.iter (fun cmd -> cmd mb.Post), fun _ -> cts.Cancel())
#endif

type Store<'Value, 'Msg>(initValue: 'Value,  ?dispose: 'Value -> unit) =
    let mutable _uid = 0
    let mutable _value = initValue

    let subscribers =
        Collections.Generic.Dictionary<_, IObserver<'Value>>()

    member _.Update(f: 'Value -> 'Value) =
        let newValue = f _value

        if not (Helpers.fastEquals _value newValue) then
            _value <- newValue

            subscribers.Values
            |> Seq.iter (fun s -> s.OnNext(_value))

    member _.Subscribe(observer: IObserver<'Value>): IDisposable =
        let id = _uid
        _uid <- _uid + 1
        subscribers.Add(id, observer)

        { new IDisposable with
            member _.Dispose() =
                if subscribers.Remove(id) then
                    match subscribers.Count, dispose with
                    | 0, Some dispose ->
                        dispose _value
                        _value <- initValue
                    | _ -> () }

    interface IStore<'Value> with
        member _.Value = _value
        member this.Update(f) = this.Update(f)
        member this.Subscribe(observer: IObserver<'Value>) = this.Subscribe(observer)

type StoreCons<'Value, 'Store> = 'Value -> ('Value -> unit) -> 'Store * Update<'Value>

let makeWithCons (init: 'Props -> 'Value)
                 (dispose: 'Value -> unit)
                 (cons: StoreCons<'Value, 'Store>)
                 (props: 'Props): 'Store =
    let value = init props
    let store, _ = cons value dispose
    store

let private makeCons i d =
    let s = Store(i, d)
    s :> IStore<'Value>, s.Update

let make (init: 'Props -> 'Value) (dispose: 'Value -> unit) (props: 'Props): IStore<'Value> =
    makeWithCons init dispose makeCons props

let makeElmishWithCons (init: 'Props -> 'Value * Cmd<'Msg>)
                       (update: 'Msg -> 'Value -> 'Value * Cmd<'Msg>)
                       (dispose: 'Value -> unit)
                       (cons: StoreCons<'Value, 'Store>)
                       (props: 'Props): 'Store * Dispatch<'Msg> =

    let mutable _cmdHandler = Unchecked.defaultof<Helpers.CmdHandler<'Msg>>

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

    _cmdHandler <- Helpers.cmdHandler dispatch
    _cmdHandler.Handle initCmd

    store, dispatch

let makeElmish (init: 'Props -> 'Value * Cmd<'Msg>)
               (update: 'Msg -> 'Value -> 'Value * Cmd<'Msg>)
               (dispose: 'Value -> unit)
               (props: 'Props): IStore<'Value> * Dispatch<'Msg> =

    makeElmishWithCons init update dispose makeCons props
