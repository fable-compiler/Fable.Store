module Util

open System

type Dispatch<'T> = ('T->'T) -> unit

type SingleObservable<'T>() =
    let mutable listener: IObserver<'T> option = None
    member _.Trigger v =
        match listener with
        | None -> ()
        | Some lis -> lis.OnNext v
    interface IObservable<'T> with
        member _.Subscribe w =
            listener <- Some w
            { new IDisposable with
                member _.Dispose() =
                    listener <- None }

type Async with
    static member AwaitObservable(obs: IObservable<'T>) =
        Async.FromContinuations( fun (cont,_,_) ->
            let mutable disp: IDisposable = Unchecked.defaultof<_>
            disp <- obs.Subscribe(fun v ->
                disp.Dispose()
                cont(v))
        )

type Time =
    | Hour of int
    | Minute of int
    | Second of int
    member this.Value =
        match this with
        | Hour n -> n
        | Second n -> n
        | Minute n -> n

    member this.ClockPercentage =
        (float this.Value) / this.FullRound

    member this.Stroke =
        match this with
        | Hour _ -> "lightgreen"
        | Minute _ -> "white"
        | Second _ -> "#023963"

    member this.StrokeWidth =
        match this with
        | Hour _ | Minute _ -> 2
        | Second _ -> 1

    member this.Length =
        match this with
        | Hour _ -> 25.
        | Minute _ -> 35.
        | Second _ -> 40.

    member this.FullRound =
        match this with
        | Hour _ -> 12.
        | Second _ | Minute _ -> 60.

type System.DateTime with
    member this.AsHour = Hour this.Hour
    member this.AsMinute = Minute this.Minute
    member this.AsSecond = Second this.Second