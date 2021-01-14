module Dragging

open System
open Fable.Core
open Util

type Msg =
    | MouseDown of x:float * y:float * offsetX:float * offsetY:float
    | MouseMove of x:float * y:float
    | MouseUp

type Model =
    { position: float*float
      offset: float*float }

let rec draggingLoop (stream: IObservable<_>) = async {
    let! msg, dispatch = Async.AwaitObservable(stream)

    match msg with
    | MouseMove(x, y) ->
        dispatch(fun model -> { model with position=(x, y) })
        return! draggingLoop stream

    // If the left button is up, we don't do anything so we leave the dragging loop
    | MouseUp -> ()
    
    // Stay within the loop without changing the state
    | _ -> return! draggingLoop stream
}

let rec waitingLoop (stream: IObservable<_>) = async {
    let! msg, dispatch = Async.AwaitObservable(stream)

    match msg with
    | MouseDown(x, y, offsetX, offsetY) ->
        dispatch(fun _ -> { position = (x, y)
                            offset = (offsetX, offsetY) })
        do! draggingLoop stream
    | _ -> ()

    return! waitingLoop stream
}

let makeStore() =
    let stream = SingleObservable()
    let tcs = new Threading.CancellationTokenSource()

    let init () =
        Async.StartImmediate(waitingLoop stream, tcs.Token)
        { position=(0.,50.); offset=(0.,0.) }

    let dispose _ = tcs.Cancel()
    let store = SvelteStore.make init dispose ()    
    let dispatch msg = stream.Trigger(msg, store.update)

    store, SvelteStore.makeDispatcher dispatch
