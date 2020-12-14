module App

open System
open Fable
open Fable.Core
open Fable.Reaction
open FSharp.Control
open Browser

type Letter = { char: string; x: int; y: int }

type Model =
    { letters: Map<int, Letter>
      fps: int
      second: float
      count: int }

type Msg = Letter of int * string * int * int

let update (model: Model) (msg: Msg): Model =
    let second =
        DateTimeOffset.Now.ToUnixTimeSeconds() |> float

    match msg with
    | Letter (i, c, x, y) ->
        { model with
              letters = Map.add i { char = c; x = x; y = y } model.letters
              second = second
              fps = if second > model.second then model.count else model.fps
              count = if second > model.second then 0 else model.count + 1 }

let initialModel: Model =
    { letters = Map.empty
      count = 0
      second = 0.
      fps = 0 }

let getOffset (element: Browser.Types.Element) =
    let doc = element.ownerDocument
    let docElem = doc.documentElement
    let clientTop = docElem.clientTop
    let clientLeft = docElem.clientLeft
    let scrollTop = window.pageYOffset
    let scrollLeft = window.pageXOffset

    int (scrollTop - clientTop), int (scrollLeft - clientLeft)

let stream (container: Browser.Types.Element) =
    let top, left = getOffset container

    asyncRx {
        let chars =
            Seq.toList "TIME FLIES LIKE AN ARROW"
            |> Seq.mapi (fun i c -> i, c)

        let! i, c = AsyncRx.ofSeq chars

        yield!
            AsyncRx.ofMouseMove ()
            |> AsyncRx.delay (100 * i)
            |> AsyncRx.requestAnimationFrame
            |> AsyncRx.map (fun m -> Letter(i, string c, int m.clientX + i * 10 + 15 - left, int m.clientY - top))
    }

let store =
    let mutable disp: IAsyncRxDisposable = Unchecked.defaultof<_>
    let mutable store: Svelte.WritableStore<_> = Unchecked.defaultof<_>

    store <-
        Svelte.makeStore
            (fun () ->
                async {
                    let msgObserver n =
                        async {
                            match n with
                            | OnNext msg -> store.update (fun model -> update model msg)
                            | OnError e -> JS.console.error (e)
                            | OnCompleted -> ()
                        }

                    let msgs = stream document.body
                    let! d = msgs.SubscribeAsync msgObserver
                    disp <- d
                }
                |> Async.StartImmediate

                initialModel)

            (fun () -> disp.DisposeAsync() |> Async.StartImmediate)

    store
