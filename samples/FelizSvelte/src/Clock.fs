module Clock

open System
open Fable.Core
open Fable.React
open Feliz
open Util
open Lit

let private clockHand (time: Time) =
    let length = time.Length
    let angle = 2.0 * Math.PI * time.ClockPercentage
    let handX = (50.0 + length * cos (angle - Math.PI / 2.0))
    let handY = (50.0 + length * sin (angle - Math.PI / 2.0))
    svg $"""
<line
  x1="50"
  y1="50"
  x2={handX}
  y2={handY}
  stroke={time.Stroke}
  stroke-width={time.StrokeWidth}>
</line>   
"""

let private handTop (time: Time) =
    let length = time.Length
    let revolution = float time.Value
    let angle = 2.0 * Math.PI * (revolution / time.FullRound)
    let handX = (50.0 + length * cos (angle - Math.PI / 2.0))
    let handY = (50.0 + length * sin (angle - Math.PI / 2.0))
    svg $"""
<circle
  cx={handX}
  cy={handY}
  r="2"
  fill={time.Stroke}>
</circle>   
"""

[<ReactComponent>]
let Clock () =
    let time = Hooks.useState DateTime.Now

    Hooks.useEffectDisposable((fun () ->
        let id = JS.setInterval (fun _ -> DateTime.Now |> time.update) 1000
        { new IDisposable with
            member _.Dispose() =
                JS.clearInterval id }
    ), [||])

    React.Lit.useLit time.current <| fun time -> html $"""
<svg viewBox="0 0 100 100"
     width="350px">
  <circle
    cx="50"
    cy="50"
    r="45"
    fill="#0B79CE"></circle>

  {clockHand time.AsHour}
  {handTop time.AsHour}

  {clockHand time.AsMinute}
  {handTop time.AsMinute}

  {clockHand time.AsSecond}
  {handTop time.AsSecond}

  <circle
    cx="50"
    cy="50"
    r="3"
    fill="#0B79CE"
    stroke="#023963"
    stroke-width="1">  
  </circle>   
</svg>
"""