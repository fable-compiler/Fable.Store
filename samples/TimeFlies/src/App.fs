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

let update (msg: Msg) (model: Model): Model =
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

let startStream (dispatch: Msg -> unit) (text: string) =
    let container = document.body
    let top, left = getOffset container

    let msgObserver n =
        async {
            match n with
            | OnNext msg -> dispatch msg
            | OnError e -> JS.console.error (e)
            | OnCompleted -> ()
        }

    let stream =
        asyncRx {
            let chars =
                Seq.toList text |> Seq.mapi (fun i c -> i, c)

            let! i, c = AsyncRx.ofSeq chars

            yield!
                AsyncRx.ofMouseMove ()
                |> AsyncRx.delay (100 * i)
                |> AsyncRx.requestAnimationFrame
                |> AsyncRx.map (fun m -> Letter(i, string c, int m.clientX + i * 10 + 15 - left, int m.clientY - top))
        }

    stream.SubscribeAsync(msgObserver)

let store =
    let mutable disp = Unchecked.defaultof<_>

    Svelte.makeStoreRec
        (fun store ->
            async {
                let dispatch msg = store.update (update msg)
                let! stream = startStream dispatch "TIME FLIES LIKE AN ARROW"
                disp <- stream
            }
            |> Async.StartImmediate

            initialModel)

        (fun () -> disp.DisposeAsync() |> Async.StartImmediate)
