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
