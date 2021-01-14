[<RequireQualifiedAccess>]
module ReactStore

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

let useStore (store: Store.IStore<'Value>) =
    let _disp = useRef Unchecked.defaultof<_>
    let _setState = useRef Unchecked.defaultof<'Value -> unit>

    let state, setState = useState(fun () ->
        _disp.current <- store.Subscribe(fun v -> _setState.current v)
        store.Value)

    useEffect((fun () ->
        _setState.current <- setState
        Dispose(_disp.current.Dispose)), [||])

    state

// Inline to avoid problems locating react package in the sample
#if DEBUG
let inline useElmishStore (init: 'Props -> 'Value * Store.Cmd<'Msg>)
#else
let useElmishStore (init: 'Props -> 'Value * Store.Cmd<'Msg>)
#endif
                   (update: 'Msg -> 'Value -> 'Value * Store.Cmd<'Msg>)
                   (dispose: 'Value -> unit)
                   (props: 'Props): 'Value * Store.Dispatch<'Msg> =

    let _disp = useRef Unchecked.defaultof<_>
    let _setState = useRef Unchecked.defaultof<'Value -> unit>
    let _dispatch = useRef Unchecked.defaultof<_>

    let state, setState = useState(fun () ->
        let store, dispatch = Store.makeElmish init update dispose props
        _dispatch.current <- dispatch
        _disp.current <- store.Subscribe(fun v -> _setState.current v)
        store.Value)

    useEffect((fun () ->
        _setState.current <- setState
        Dispose(fun () -> _disp.current.Dispose())), [||])

    state, fun msg -> _dispatch.current msg

let useElmishStoreSimple (init: 'Props -> 'Value)
                         (update: 'Msg -> 'Value -> 'Value)
                         (dispose: 'Value -> unit)
                         (props: 'Props): 'Value * Store.Dispatch<'Msg> =

    let init p = init p, []
    let update m v = update m v, []
    useElmishStore init update dispose props
