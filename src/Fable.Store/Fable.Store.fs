[<RequireQualifiedAccessAttribute>]
module Store

open System

type Update<'Model> = ('Model -> 'Model) -> unit
type Dispatch<'Msg> = 'Msg -> unit
type Cmd<'Msg> = (Dispatch<'Msg> -> unit) list

type IStore<'Model> =
    inherit IObservable<'Model>
    abstract Update: f:('Model -> 'Model) -> unit
// abstract SubscribeImmediate: observer: IObserver<'Model> -> 'Model * IDisposable

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

type Store<'Model>(init: unit -> 'Model, dispose: 'Model -> unit) =
    let mutable uid = 0
    let mutable model = Unchecked.defaultof<_>

    let subscribers =
        Collections.Generic.Dictionary<_, IObserver<'Model>>()

    member _.Update(f: 'Model -> 'Model) =
        if subscribers.Count > 0 then
            let newModel = f model

            if not (Helpers.fastEquals model newModel) then
                model <- newModel

                subscribers.Values
                |> Seq.iter (fun s -> s.OnNext(model))

    member _.Subscribe(observer: IObserver<'Model>): IDisposable =
        if subscribers.Count = 0 then model <- init ()
        let id = uid
        uid <- uid + 1
        subscribers.Add(id, observer)

        { new IDisposable with
            member _.Dispose() =
                if subscribers.Remove(id) && subscribers.Count = 0 then
                    dispose model
                    model <- Unchecked.defaultof<_> }

    // member _.SubscribeImmediate(observer: IObserver<'Model>): 'Model * IDisposable =
    //     if subscribers.Count = 0 then
    //         model <- init()
    //     let id = uid
    //     uid <- uid + 1
    //     subscribers.Add(id, observer)
    //     let disposable =
    //         { new IDisposable with
    //             member _.Dispose() =
    //                 if subscribers.Remove(id) && subscribers.Count = 0 then
    //                     dispose model
    //                     model <- Unchecked.defaultof<_> }
    //     model, disposable

    // member this.SubscribeImmediate(observer: 'Model -> unit): 'Model * IDisposable =
    //     let observer =
    //         { new IObserver<_> with
    //             member _.OnNext(value) = observer value
    //             member _.OnCompleted() = ()
    //             member _.OnError(_error: exn) = () }
    //     this.SubscribeImmediate(observer)

    interface IStore<'Model> with
        member this.Update(f) = this.Update(f)
        member this.Subscribe(observer: IObserver<'Model>) = this.Subscribe(observer)

type StoreCons<'Model, 'Store> = (unit -> 'Model) -> ('Model -> unit) -> 'Store * Update<'Model>

let makeWithCons (init: 'Props -> 'Model)
                 (dispose: 'Model -> unit)
                 (cons: StoreCons<'Model, 'Store>)
                 : 'Props -> 'Store =

    let mutable _store: 'Store option = None

    fun props ->
        match _store with
        | Some store -> store
        | None ->
            let dispose m =
                _store <- None
                dispose m

            let store, _ = cons (fun _ -> init props) dispose
            _store <- Some(store)
            store

let make (init: 'Props -> 'Model) (dispose: 'Model -> unit): 'Props -> IStore<'Model> =
    makeWithCons init dispose (fun i d ->
        let s = Store(i, d)
        upcast s, s.Update)   

let makeRecWithCons (init: 'Store -> 'Props -> 'Model * IDisposable)
                         (cons: StoreCons<'Model, 'Store>)
                         : 'Props -> 'Store =

    let mutable _disp: IDisposable = null
    let mutable _store: 'Store option = None

    fun props ->
        match _store with
        | Some store -> store
        | None ->
            let dispose _ =
                let d = _disp
                _disp <- null
                _store <- None
                d.Dispose()

            let init () =
                let model, disp = init _store.Value props
                _disp <- disp
                model

            let store, _ = cons init dispose
            _store <- Some(store)
            store

let makRec (init: IStore<'Model> -> 'Props -> 'Model * IDisposable): 'Props -> IStore<'Model> =
    makeRecWithCons init (fun i d ->
        let s = Store(i, d)
        upcast s, s.Update)

let makeElmishWithCons (init: 'Props -> 'Model * Cmd<'Msg>)
                       (update: 'Msg -> 'Model -> 'Model * Cmd<'Msg>)
                       (dispose: 'Model -> unit)
                       (cons: StoreCons<'Model, 'Store>)
                       : 'Props -> 'Store * Dispatch<'Msg> =

    let mutable _storeDispatch: ('Store * Dispatch<'Msg>) option = None

    let mutable _cmdHandler =
        Unchecked.defaultof<Helpers.CmdHandler<'Msg>>

    fun props ->
        match _storeDispatch with
        | Some storeDispatch -> storeDispatch
        | None ->
            let store, storeUpdate =
                cons
                    (fun () ->
                        let m, cmd = init props
                        _cmdHandler.Handle cmd
                        m)
                    (fun m ->
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
            _storeDispatch <- Some(store, dispatch)
            store, dispatch

let makeElmish (init: 'Props -> 'Model * Cmd<'Msg>)
               (update: 'Msg -> 'Model -> 'Model * Cmd<'Msg>)
               (dispose: 'Model -> unit)
               : 'Props -> IStore<'Model> * Dispatch<'Msg> =

    makeElmishWithCons init update dispose (fun i d ->
        let s = Store(i, d)
        upcast s, s.Update)
