namespace ElmishStore

open System
open Fable

type Dispatch<'Msg> = 'Msg -> unit

type Cmd<'Value, 'Msg> = (StoreUpdate<'Value> -> Dispatch<'Msg> -> unit) list

module internal Helpers =
    type CmdHandler<'Value, 'Msg>(handler, ?dispose) =
        member _.Handle(cmd: Cmd<'Value, 'Msg>): unit = handler cmd
        member _.Dispose() = match dispose with Some d -> d () | None -> ()
        interface IDisposable with
            member this.Dispose() = this.Dispose()

#if FABLE_COMPILER
    open Fable.Core

    let cmdHandler (update: StoreUpdate<'Value>) (dispatch: 'Msg -> unit): CmdHandler<'Value, 'Msg> =
        new CmdHandler<_, _>(List.iter (fun cmd ->
            JS.setTimeout (fun _ -> cmd update dispatch) 0 |> ignore))
#else
    let cmdHandler (update: StoreUpdate<'Value>) (dispatch: 'Msg -> unit): CmdHandler<'Value, 'Msg> =
        let cts = new Threading.CancellationTokenSource()

        let mb = MailboxProcessor.Start(fun inbox -> async {
            while true do
                match! inbox.Receive() with
                | Choice1Of2 updater -> update updater
                | Choice2Of2 msg -> dispatch msg
        }, cts.Token)

        new CmdHandler<_,_>(List.iter (fun cmd -> cmd (Choice1Of2 >> mb.Post) (Choice2Of2 >> mb.Post)), fun _ -> cts.Cancel())
#endif

[<RequireQualifiedAccess>]
module Cmd =
    // /// Execute the commands using the supplied dispatcher
    // let internal exec onError update (dispatch: Dispatch<'msg>) (cmd: Cmd<'value, 'msg>) =
    //     cmd |> List.iter (fun call -> try call update dispatch with ex -> onError ex)

    /// None - no commands, also known as `[]`
    let none : Cmd<'value, 'msg> =
        []

    /// When emitting the message, map to another type
    let map (f: 'a -> 'msg) (cmd: Cmd<'value, 'a>) : Cmd<'value, 'msg> =
        cmd |> List.map (fun g -> (fun update dispatch -> g update (f >> dispatch)))

    /// Aggregate multiple commands
    let batch (cmds: #seq<Cmd<'value, 'msg>>) : Cmd<'value, 'msg> =
        cmds |> List.concat

    module OfFunc =
        /// Command to evaluate a simple function and map the result
        /// into success or error (of exception)
        let either (task: 'a -> _) (arg: 'a) (ofSuccess: _ -> 'msg) (ofError: _ -> 'msg) : Cmd<'value, 'msg> =
            let bind _update dispatch =
                try
                    task arg
                    |> (ofSuccess >> dispatch)
                with x ->
                    x |> (ofError >> dispatch)
            [bind]

        /// Command to evaluate a simple function and map the success to a message
        /// discarding any possible error
        let perform (task: 'a -> _) (arg: 'a) (ofSuccess: _ -> 'msg) : Cmd<'value, 'msg> =
            let bind _update dispatch =
                try
                    task arg
                    |> (ofSuccess >> dispatch)
                with x ->
                    ()
            [bind]

        /// Command to evaluate a simple function and map the error (in case of exception)
        let attempt (task: 'a -> unit) (arg: 'a) (ofError: _ -> 'msg) : Cmd<'value, 'msg> =
            let bind _update dispatch =
                try
                    task arg
                with x ->
                    x |> (ofError >> dispatch)
            [bind]

        /// Command to issue a specific message
        let result (msg:'msg) : Cmd<'value, 'msg> =
            [fun _update dispatch -> dispatch msg]

    module OfAsyncWith =
        /// Command that will evaluate an async block and update the model on succes
        /// or report a message on error
        let update (start: Async<unit> -> unit)
                   (task: 'a -> Async<'result>)
                   (arg: 'a)
                   (onSuccess: 'result -> 'model -> 'model)
                   (onError: exn -> 'msg) : Cmd<'model, 'msg> =
            let bind update dispatch =
                async {
                    let! r = task arg |> Async.Catch
                    match r with
                    | Choice1Of2 x -> update (onSuccess x)
                    | Choice2Of2 x -> onError x |> dispatch
                }
            [fun u d -> bind u d |> start]

        /// Command that will evaluate an async block and map the result
        /// into success or error (of exception)
        let either (start: Async<unit> -> unit)
                   (task: 'a -> Async<_>)
                   (arg: 'a)
                   (ofSuccess: _ -> 'msg)
                   (ofError: _ -> 'msg) : Cmd<'value, 'msg> =
            let bind _update dispatch =
                async {
                    let! r = task arg |> Async.Catch
                    dispatch (match r with
                             | Choice1Of2 x -> ofSuccess x
                             | Choice2Of2 x -> ofError x)
                }
            [fun u d -> bind u d |> start]

        /// Command that will evaluate an async block and map the success
        let perform (start: Async<unit> -> unit)
                    (task: 'a -> Async<_>)
                    (arg: 'a)
                    (ofSuccess: _ -> 'msg) : Cmd<'value, 'msg> =
            let bind _update dispatch =
                async {
                    let! r = task arg |> Async.Catch
                    match r with
                    | Choice1Of2 x -> dispatch (ofSuccess x)
                    | _ -> ()
                }
            [fun u d -> bind u d |> start]

        /// Command that will evaluate an async block and map the error (of exception)
        let attempt (start: Async<unit> -> unit)
                    (task: 'a -> Async<_>)
                    (arg: 'a)
                    (ofError: _ -> 'msg) : Cmd<'value, 'msg> =
            let bind _update dispatch =
                async {
                    let! r = task arg |> Async.Catch
                    match r with
                    | Choice2Of2 x -> dispatch (ofError x)
                    | _ -> ()
                }
            [fun u d -> bind u d |> start]

        /// Command that will evaluate an async block to the message
        let result (start: Async<unit> -> unit)
                   (task: Async<'msg>) : Cmd<'value, 'msg> =
            let bind _update dispatch =
                async {
                    let! r = task
                    dispatch r
                }
            [fun u d -> bind u d |> start]

    module OfAsync =
#if FABLE_COMPILER
        let start x = Async.StartImmediate x
#else
        let inline start x = Async.Start x
#endif

        /// Command that will evaluate an async block and update the model on succes
        /// or report a message on error
        let inline update (task: 'a -> Async<'result>)
                          (arg: 'a)
                          (onSuccess: 'result -> 'model -> 'model)
                          (onError: exn -> 'msg) : Cmd<'model, 'msg> =
            OfAsyncWith.update start task arg onSuccess onError

        /// Command that will evaluate an async block and map the result
        /// into success or error (of exception)
        let inline either (task: 'a -> Async<_>)
                          (arg: 'a)
                          (ofSuccess: _ -> 'msg)
                          (ofError: _ -> 'msg) : Cmd<'value, 'msg> =
            OfAsyncWith.either start task arg ofSuccess ofError

        /// Command that will evaluate an async block and map the success
        let inline perform (task: 'a -> Async<_>)
                           (arg: 'a)
                           (ofSuccess: _ -> 'msg) : Cmd<'value, 'msg> =
            OfAsyncWith.perform start task arg ofSuccess

        /// Command that will evaluate an async block and map the error (of exception)
        let inline attempt (task: 'a -> Async<_>)
                           (arg: 'a)
                           (ofError: _ -> 'msg) : Cmd<'value, 'msg> =
            OfAsyncWith.attempt start task arg ofError

        /// Command that will evaluate an async block to the message
        let inline result (task: Async<'msg>) : Cmd<'value, 'msg> =
            OfAsyncWith.result start task
