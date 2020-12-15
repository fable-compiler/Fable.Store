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

let stream = SingleObservable()

let rec draggingLoop () = async {
    let! msg, dispatch = Async.AwaitObservable(stream)

    match msg with
    | MouseMove(x, y) ->
        dispatch(fun model -> { model with position=(x, y) })
        return! draggingLoop()

    // If the left button is up, we don't do anything so we leave the dragging loop
    | MouseUp -> ()

    // Stay within the loop without changing the state
    | _ -> return! draggingLoop()
}

let rec waitingLoop () = async {
    let! msg, dispatch = Async.AwaitObservable(stream)

    match msg with
    | MouseDown(x, y, offsetX, offsetY) ->
        dispatch(fun _ -> { position = (x, y)
                            offset = (offsetX, offsetY) })
        do! draggingLoop()
    | _ -> ()

    return! waitingLoop()
}

let store =
    let mutable tcs = Unchecked.defaultof<_>
    Fable.Svelte.makeStore
        (fun () ->
            tcs <- new Threading.CancellationTokenSource()
            Async.StartImmediate(waitingLoop(), tcs.Token)
            { position=(0.,0.); offset=(0.,0.) })
        (fun () -> tcs.Cancel())

let dispatch =
    Fable.Svelte.makeDispatcher (fun msg ->
        stream.Trigger(msg, store.update))
