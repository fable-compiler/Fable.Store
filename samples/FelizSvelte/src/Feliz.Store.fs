module Feliz.Store

open System
open Fable.Core
open Feliz

let disposable f =
    { new IDisposable with
        member _.Dispose() = f() }

type Store<'Model>(init: unit -> 'Model, dispose: 'Model -> unit) =
    let mutable uid = 0
    let mutable model = Unchecked.defaultof<_>
    let subscribers = Collections.Generic.Dictionary<_, IObserver<'Model>>()
    
    member _.Update(f: 'Model->'Model) =
        if subscribers.Count > 0 then
            let newModel = f model
            if not (obj.ReferenceEquals(model, newModel)) then
                model <- newModel
                subscribers.Values |> Seq.iter (fun s -> s.OnNext(model))
    
    member _.SubscribeImmediate(observer: IObserver<'Model>): 'Model * IDisposable = 
        if subscribers.Count = 0 then
            model <- init()
        let id = uid
        uid <- uid + 1
        subscribers.Add(id, observer)
        model, disposable(fun () ->
            if subscribers.Remove(id) && subscribers.Count = 0 then
                dispose model
                model <- Unchecked.defaultof<_>)

    member this.SubscribeImmediate(observer: 'Model -> unit): 'Model * IDisposable = 
        let observer =
            { new IObserver<_> with
                member _.OnNext(value) = observer value
                member _.OnCompleted() = ()
                member _.OnError(_error: exn) = () }
        this.SubscribeImmediate(observer)

    interface IObservable<'Model> with
        member this.Subscribe(observer: IObserver<'Model>): IDisposable = 
            this.SubscribeImmediate(observer) |> snd

type DispatchStore<'Msg, 'Model>(init: unit -> 'Model, dispose: 'Model -> unit, dispatch: 'Msg -> unit) =
    inherit Store<'Model>(init, dispose)
    member _.Dispatch(msg) = dispatch msg

let useStore (store: Store<'Model>) =
    let mutable _disp: IDisposable = Unchecked.defaultof<_>
    let _setState: ('Model->unit) ref = ref Unchecked.defaultof<_>    
    let state, setState = React.useState(fun () ->
        let model, disp = store.SubscribeImmediate(fun v -> _setState.contents v)
        _disp <- disp
        model)
    _setState := setState
    React.useEffect((fun () ->
        disposable(fun () -> _disp.Dispose())), [||])
    state

let useDispatchStore (store: DispatchStore<'Msg, 'Model>) =
    let mutable _disp: IDisposable = Unchecked.defaultof<_>
    let _setState: ('Model->unit) ref = ref Unchecked.defaultof<_>    
    let state, setState = React.useState(fun () ->
        let model, disp = store.SubscribeImmediate(fun v -> _setState.contents v)
        _disp <- disp
        model)
    _setState := setState
    React.useEffect((fun () ->
        disposable(fun () -> _disp.Dispose())), [||])
    state, store.Dispatch

type Cmd<'Msg> = (('Msg -> unit) -> unit) list

let makeElmishStore
        (init: unit -> 'Model * Cmd<'Msg>)
        (update: 'Msg -> 'Model -> 'Model * Cmd<'Msg>)
        (dispose: 'Model -> unit) =

    let mutable dispatch = Unchecked.defaultof<_>
    let mutable store: DispatchStore<_,_> = Unchecked.defaultof<_>

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
        store.Update(fun model ->
            let model, cmds = update msg model
            _cmds <- cmds
            model)
        handleCmds _cmds

    let init () =
        let model, cmds = init()
        JS.setTimeout (fun () -> handleCmds cmds) 0 |> ignore
        model

    store <- DispatchStore(init, dispose, dispatch)

    store

let makeProjectedStore
    (project: 'Model1 -> 'Model2)
    (dispatch: 'Msg2 -> 'Msg1)
    (store: DispatchStore<'Msg1, 'Model1>) =

    let mutable _disp: IDisposable = Unchecked.defaultof<_>
    let mutable projectedStore: DispatchStore<'Msg2, 'Model2> = Unchecked.defaultof<_>

    let init() =
        let model, disp = store.SubscribeImmediate(fun v ->
            projectedStore.Update(fun _ -> project v))
        _disp <- disp
        project model

    projectedStore <- DispatchStore(init, (fun _ -> _disp.Dispose()), dispatch >> store.Dispatch)
    projectedStore
