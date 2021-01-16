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


let inline private useStateWithDisposable (f) =
    let _disp = useRef Unchecked.defaultof<IDisposable>
    let _setState = useRef Unchecked.defaultof<('Value -> unit) option>

    let state, setState = useState(fun () ->
        let initState, disp = f _setState
        _disp.current <- disp
        initState)

    useEffect((fun () ->
        _setState.current <- Some setState
        Dispose(fun () ->
            _setState.current <- None
            _disp.current.Dispose()
        )), [||])

    state

let useObservable (obs: IObservable<'Value>) =
    useStateWithDisposable (fun setState ->
        obs |> Store.subscribeImmediate(fun v ->
            setState.current |> Option.iter (fun f -> f v)))

let useStoreLazy (init: unit -> Store.IStore<'Value>): 'Value * Store.Update<'Value> =
    let _store = useRef Unchecked.defaultof<_>

    let state = useStateWithDisposable (fun setState ->
        _store.current <- init()
        _store.current |> Store.subscribeImmediate(fun v ->
            setState.current |> Option.iter (fun f -> f v)))

    state, fun f -> _store.current.Update(f)

let useElmishStore (init: 'Props -> 'Value * Store.Cmd<'Msg>)
                   (update: 'Msg -> 'Value -> 'Value * Store.Cmd<'Msg>)
                   (dispose: 'Value -> unit)
                   (props: 'Props): 'Value * Store.Dispatch<'Msg> =

    let _dispatch = useRef Unchecked.defaultof<_>

    let state = useStateWithDisposable (fun setState ->
        let store, dispatch = Store.makeElmish init update dispose props
        _dispatch.current <- dispatch
        store |> Store.subscribeImmediate(fun v ->
            setState.current |> Option.iter (fun f -> f v)))

    state, fun msg -> _dispatch.current msg 

let useElmishStoreSimple (init: 'Props -> 'Value)
                         (update: 'Msg -> 'Value -> 'Value)
                         (dispose: 'Value -> unit)
                         (props: 'Props): 'Value * Store.Dispatch<'Msg> =

    let init p = init p, []
    let update m v = update m v, []
    useElmishStore init update dispose props
