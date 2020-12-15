module App

open System
open Fable
open Fable.Core
open Fable.Reaction
open FSharp.Control
open Browser

module Cmd =
    let ofMsg msg: Svelte.Cmd<'Msg> = [ fun d -> d msg ]

    let ofAsync (action: _ -> Async<'Msg>): Svelte.Cmd<'Msg> =
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
      stream: IAsyncRxDisposable option }

type Msg =
    | Letter of index: int * char: char * x: int * y: int
    | Message of string
    | Stream of IAsyncRxDisposable

let getOffset (element: Browser.Types.Element) =
    let doc = element.ownerDocument
    let docElem = doc.documentElement
    let clientTop = docElem.clientTop
    let clientLeft = docElem.clientLeft
    let scrollTop = window.pageYOffset
    let scrollLeft = window.pageXOffset

    int (scrollTop - clientTop), int (scrollLeft - clientLeft)

let startStream (previous: IAsyncRxDisposable option) (text: string) (dispatch: Msg -> unit) =
    let container = document.body
    let top, left = getOffset container

    let msgObserver n =
        async {
            match n with
            | OnNext msg -> dispatch msg
            | OnError e -> JS.console.error (e)
            | OnCompleted -> ()
        }

    async {
        match previous with
        | Some d -> do! d.DisposeAsync()
        | None -> ()

        return!
            (asyncRx {
                let chars =
                    Seq.toList text |> Seq.mapi (fun i c -> i, c)

                let! i, c = AsyncRx.ofSeq chars

                yield!
                    AsyncRx.ofMouseMove ()
                    |> AsyncRx.delay (100 * i)
                    |> AsyncRx.requestAnimationFrame
                    |> AsyncRx.map (fun m -> Letter(i, c, int m.clientX + i * 10 + 15 - left, int m.clientY - top))
             }).SubscribeAsync msgObserver
    }

let update (msg: Msg) (model: Model) =
    match msg with
    | Letter (i, c, x, y) ->
        let second =
            DateTimeOffset.Now.ToUnixTimeSeconds() |> float

        { model with
              letters = Map.add i { char = c; x = x; y = y } model.letters
              second = second
              fps = if second > model.second then model.count else model.fps
              count = if second > model.second then 0 else model.count + 1 },
        []

    | Message txt ->
        { model with
              letters = Map.empty
              message = txt },
        Cmd.ofAsync
            (fun dispatch ->
                async {
                    let! stream = startStream model.stream txt dispatch
                    return Stream stream
                })

    | Stream stream -> { model with stream = Some stream }, []

let init () =
    { letters = Map.empty
      count = 0
      second = 0.
      fps = 0
      message = ""
      stream = None },
    Cmd.ofMsg (Message "TIME FLIES LIKE AN ARROW")

let store =
    Svelte.makeElmishStore
        init
        update
        (fun model ->
            model.stream
            |> Option.iter (fun d -> d.DisposeAsync() |> Async.StartImmediate))

let dispatch = Svelte.makeDispatcher store.dispatch
