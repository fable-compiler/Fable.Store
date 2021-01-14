module TimeFlies

open System
open Fable
open Fable.Reaction
open FSharp.Control
open Browser

type Letter = { char: string; x: int; y: int }

type Model =
    { letters: Map<int, Letter>
      fps: int
      second: float
      count: int
      text: string }

type Msg = Letter of int * string * int * int

let init txt: Model =
    { letters = Map.empty
      count = 0
      second = 0.
      fps = 0
      text = txt }

let getOffset (element: Browser.Types.Element) =
    let doc = element.ownerDocument
    let docElem = doc.documentElement
    let clientTop = docElem.clientTop
    let clientLeft = docElem.clientLeft
    let scrollTop = window.pageYOffset
    let scrollLeft = window.pageXOffset

    int (scrollTop - clientTop), int (scrollLeft - clientLeft)

let startStream (update: Model -> unit) (model: Model) =
    let container = document.body
    let top, left = getOffset container

    asyncRx {
        let chars =
            Seq.toList model.text
            |> Seq.mapi (fun i c -> i, c)

        let! i, c = AsyncRx.ofSeq chars

        yield!
            AsyncRx.ofMouseMove ()
            |> AsyncRx.delay (100 * i)
            |> AsyncRx.requestAnimationFrame
            |> AsyncRx.map (fun m -> Letter(i, string c, int m.clientX + i * 10 + 15 - left, int m.clientY - top))
    }
    |> AsyncRx.toObservable
    |> Observable.scan
        (fun (model: Model) (Letter (i, c, x, y)) ->
            let second =
                DateTimeOffset.Now.ToUnixTimeSeconds() |> float

            { model with
                  letters = Map.add i { char = c; x = x; y = y } model.letters
                  second = second
                  fps = if second > model.second then model.count else model.fps
                  count = if second > model.second then 0 else model.count + 1 })
        model
    |> Observable.subscribe update

let store =
    Svelte.makeStoreRec
        (fun store txt ->
            let update m = store.update (fun _ -> m)
            let model = init txt
            model, startStream update model)
