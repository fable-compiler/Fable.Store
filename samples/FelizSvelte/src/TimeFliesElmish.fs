module TimeFliesElmish

open System
open Fable
open Fable.Core
open Fable.Reaction
open FSharp.Control
open Browser

module Cmd =
    type Cmd<'Msg> = (('Msg -> unit) -> unit) list

    let ofMsg msg: Cmd<'Msg> = [ fun d -> d msg ]

    let ofAsync (action: _ -> Async<'Msg>): Cmd<'Msg> =
        [ fun dispatch ->
            async {
                let! msg = action dispatch
                dispatch msg
            }
            |> Async.StartImmediate ]

type Letter = { char: char; x: int; y: int }

type Model =
    { letters: Map<int, Letter>
      fps: int
      second: float
      count: int
      message: string
      stream: IDisposable option }

type Msg =
    | Letter of index: int * char: char * x: int * y: int
    | Message of string
    | Stream of IDisposable

let getOffset (element: Browser.Types.Element) =
    let doc = element.ownerDocument
    let docElem = doc.documentElement
    let clientTop = docElem.clientTop
    let clientLeft = docElem.clientLeft
    let scrollTop = window.pageYOffset
    let scrollLeft = window.pageXOffset

    int (scrollTop - clientTop), int (scrollLeft - clientLeft)

let startStream (text: string) (dispatch: Msg -> unit) =
    let container = document.body
    let top, left = getOffset container

    asyncRx {
        let chars =
            Seq.toList text
            |> Seq.mapi (fun i c -> i, c)

        let! i, c = AsyncRx.ofSeq chars

        yield!
            AsyncRx.ofMouseMove ()
            |> AsyncRx.delay (100 * i)
            |> AsyncRx.requestAnimationFrame
            |> AsyncRx.map (fun m -> Letter(i, c, int m.clientX + i * 10 + 15 - left, int m.clientY - top))
    }
    |> AsyncRx.toObservable
    |> Observable.subscribe dispatch

let disposeStream (model: Model) =
    model.stream |> Option.iter (fun d -> d.Dispose())

let update (msg: Msg) (model: Model) =
    match msg with
    | Letter (i, c, x, y) ->
        let second = DateTimeOffset.Now.ToUnixTimeSeconds() |> float

        { model with
              letters = Map.add i { char = c; x = x; y = y } model.letters
              second = second
              fps = if second > model.second then model.count else model.fps
              count = if second > model.second then 0 else model.count + 1 },
        []

    | Message txt ->
        disposeStream model

        { model with
              letters = Map.empty
              message = txt },
        [fun dispatch ->
            startStream txt dispatch |> Stream |> dispatch]

    | Stream stream -> { model with stream = Some stream }, []

let init msg =
    { letters = Map.empty
      count = 0
      second = 0.
      fps = 0
      message = msg
      stream = None },
    Cmd.ofMsg (Message msg)

open Feliz

[<ReactComponent>]
let TimeFliesElmish (text: string) =
    let model, dispatch =
        ReactStore.useElmishStore init update disposeStream text

    Html.div [
        prop.style [
            style.fontFamily "Consolas, monospace"
            style.height 100
        ]
        prop.children [
            for KeyValue(_idx, letter) in model.letters do
                Html.span [
                    prop.style [
                        style.position.fixedRelativeToWindow
                        style.top letter.y
                        style.left letter.x
                    ]
                    prop.text (string letter.char)
                ]
            Html.input [
                prop.style [ style.width 300 ]
                prop.defaultValue model.message
                prop.onChange (fun (ev: Types.Event) ->
                    Message (ev.target :?> Types.HTMLInputElement).value |> dispatch)
            ]
            Html.p ("fps: " + string model.fps)
        ]
    ]
