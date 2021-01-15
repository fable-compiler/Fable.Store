[<RequireQualifiedAccess>]
module ReactStore

open System
open Fable.Core

type Dispose = delegate of unit -> unit

type ReactRef<'Value> =
    abstract current: 'Value with get, set

[<ImportMember("react")>]
let private useState(f: unit -> 'State): 'State * ('State -> unit) = jsNative

[<ImportMember("react")>]
let private useEffect(f: unit -> Dispose, deps: obj[]): unit = jsNative

[<ImportMember("react")>]
let private useRef(v: 'Value): ReactRef<'Value> = jsNative

// Don't remove the lambda here because Fable may try to uncurry the function
let inline private makeDispose (d: IDisposable) =
    Dispose(fun () -> d.Dispose())

let useObservable (initValue: 'Value) (obs: IObservable<'Value>) =
    let state, setState = useState(fun () -> initValue)

    useEffect((fun () ->
        let disp = obs.Subscribe(setState)
        makeDispose disp), [||])

    state

let useStore (store: Store.IReadStore<'Value>) =
    useObservable store.Value store

let useStoreLazy (init: unit -> Store.IStore<'Value>): 'Value * Store.Update<'Value> =
    let _store = useRef Unchecked.defaultof<_>

    let state, setState = useState(fun () ->
        _store.current <- init()
        _store.current.Value)

    useEffect((fun () ->
        let disp = _store.current.Subscribe(setState)
        makeDispose disp), [||])

    state, fun f -> _store.current.Update(f)

// Inline to avoid problems locating react package in the sample
#if DEBUG
let inline useElmishStore (init: 'Props -> 'Value * Store.Cmd<'Msg>)
#else
let useElmishStore (init: 'Props -> 'Value * Store.Cmd<'Msg>)
#endif
                   (update: 'Msg -> 'Value -> 'Value * Store.Cmd<'Msg>)
                   (dispose: 'Value -> unit)
                   (props: 'Props): 'Value * Store.Dispatch<'Msg> =

    let _store = useRef Unchecked.defaultof<_>
    let _dispatch = useRef Unchecked.defaultof<_>

    let state, setState = useState(fun () ->
        let store, dispatch = Store.makeElmish init update dispose props
        _store.current <- store
        _dispatch.current <- dispatch
        store.Value)

    useEffect((fun () ->
        let disp = _store.current.Subscribe(setState)
        makeDispose disp), [||])

    state, fun msg -> _dispatch.current msg 

let useElmishStoreSimple (init: 'Props -> 'Value)
                         (update: 'Msg -> 'Value -> 'Value)
                         (dispose: 'Value -> unit)
                         (props: 'Props): 'Value * Store.Dispatch<'Msg> =

    let init p = init p, []
    let update m v = update m v, []
    useElmishStore init update dispose props
