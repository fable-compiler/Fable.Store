namespace Fable

open System

type StoreUpdate<'Value> = ('Value -> 'Value) -> unit

type StoreCons<'Value, 'Store> = 'Value -> ('Value -> unit) -> 'Store * StoreUpdate<'Value>

type IStore<'Value> =
    inherit IDisposable
    inherit IObservable<'Value>
    abstract Update: f:('Value -> 'Value) -> unit

type Store<'Value>(initValue: 'Value,  ?dispose: 'Value -> unit) =
    let mutable _uid = 0
    let mutable _disposed = false
    let mutable _value = initValue
    let subscribers = Collections.Generic.Dictionary<_, IObserver<'Value>>()

    member _.Dispose() =
        if not _disposed then
            subscribers.Clear()
            match dispose with Some d -> d _value | None -> ()

    member _.Update(f: 'Value -> 'Value) =
        _value <- f _value
        subscribers.Values
        |> Seq.iter (fun s -> s.OnNext(_value))

    member this.Subscribe(observer: IObserver<'Value>): IDisposable =
        if _disposed then
            failwith "The store has already been disposed"
        else
            // Immediately report current value
            observer.OnNext(_value)

            let id = _uid
            _uid <- _uid + 1
            subscribers.Add(id, observer)

            { new IDisposable with
                member _.Dispose() =
                    if subscribers.Remove(id) && subscribers.Count = 0 then
                        this.Dispose() }

    interface IStore<'Value> with
        member this.Dispose() = this.Dispose()
        member this.Update(f) = this.Update(f)
        member this.Subscribe(observer: IObserver<'Value>) = this.Subscribe(observer)
