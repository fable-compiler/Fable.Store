module TimeFliesElmish

open System
open Fable
open Fable.Core
open Fable.Core.JsInterop
open Fable.Reaction
open Fable.SimpleHttp
open FSharp.Control
open ElmishStore
open Browser

type Letter = { char: char; x: int; y: int }

type Model =
    { letters: Map<int, Letter>
      fps: int
      second: float
      count: int
      message: string
      quote: string
      isLoading: bool
      stream: IDisposable option }

type Msg =
    | Letter of index: int * char: char * x: int * y: int
    | Message of string
    | GetTodayQuote
    | OnError of exn

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

let getQuote () = async {
    let! response =
        Http.request "https://quotes.rest/qod.json?category=inspire"
        |> Http.send

    return
        match response.content with
        | ResponseContent.Text res ->
            let res = JS.JSON.parse(res)
            res?contents?quotes?(0)?quote: string
        | _ ->
            failwith "Cannot read response"
}

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
        [fun update dispatch ->
            let stream = startStream txt dispatch
            update (fun m -> { m with stream = Some stream })]

    | GetTodayQuote ->
        let onSuccess quote m =
            { m with isLoading = false; quote = quote }

        { model with isLoading = true },
        Cmd.OfAsync.update getQuote () onSuccess OnError

    | OnError exn ->
        console.error(exn)
        { model with isLoading = false }, []

let init msg =
    { letters = Map.empty
      count = 0
      second = 0.
      fps = 0
      message = msg
      quote = ""
      isLoading = false
      stream = None },
    Cmd.OfFunc.result (Message msg)

open Feliz

let theySaidSoAttribution() =
    Html.span [
        prop.style [
            style.zIndex 50
            style.fontSize(length.em 0.9)
            style.fontWeight.bold
        ]
        prop.children [
            Html.img [
                prop.src "https://theysaidso.com/branding/theysaidso.png"
                prop.height 20
                prop.width 20
                prop.alt "theysaidso.com"
            ]
            Html.a [
                prop.href "https://theysaidso.com"
                prop.title "Powered by quotes from theysaidso.com"
                prop.style [
                    style.color "#ccc"
                    style.marginLeft 4
                    style.verticalAlign.middle
                ]
                prop.text "They Said So®"
            ]
        ]
    ]

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
            Html.button [
                prop.disabled model.isLoading
                prop.children [
                    Html.text "Get today's quote from "
                    theySaidSoAttribution()
                ]
                prop.onClick (fun _ -> dispatch GetTodayQuote)
            ]
            Html.p (if model.isLoading then "Loading..." else model.quote)
        ]
    ]
