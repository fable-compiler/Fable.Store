namespace Fable.SveltePlugins

open Fable.AST

[<assembly:Fable.ScanForPlugins>]
do()

type Helper =
    static member PrintType(h: Fable.PluginHelper, i: int, t: Fable.Type) =
        let rec nestedLambda acc = function
            | Fable.LambdaType(arg, returnType) ->
                nestedLambda (arg::acc) returnType
            | returnType -> List.rev acc, returnType

        let printArg (i: int) arg = $"a{i}: {Helper.PrintType(h, i, arg)}"
        let printFn args t = $"""(({args |> List.mapi printArg |> String.concat ", "}) => {Helper.PrintType(h, i, t)}"""
        let printRecord fields =
            [
                "{"
                for (name, t) in fields do
                    $"""{String.replicate 4 " "}{name}: {Helper.PrintType(h, i + 1, t)},"""
                "}"
            ] |> String.concat System.Environment.NewLine

        match t with
        | Fable.Any -> "any"
        | Fable.Unit -> "undefined"
        | Fable.Boolean -> "boolean"
        | Fable.Char
        | Fable.String -> "string"
        | Fable.Regex -> "RegExp"
        | Fable.Enum _
        | Fable.Number _ -> "number"
        | Fable.Option t -> $"({Helper.PrintType(h, i, t)}|undefined)"
        | Fable.Tuple ts -> $"""[{ts |> List.map (fun t -> Helper.PrintType(h, i, t)) |> String.concat ", "}]"""
        | Fable.Array t -> Helper.PrintType(h, i, t) + "[]"
        | Fable.List t -> $"Iterable<{Helper.PrintType(h, i, t)}>"
        | Fable.DelegateType(args, ret) -> printFn args ret
        | Fable.LambdaType(arg, ret) -> nestedLambda [arg] ret ||> printFn
        | Fable.GenericParam name -> "any" // name // TODO: print generic args in function/type
        | Fable.DeclaredType(e, _genArgs) ->
            let e = h.GetEntity(e)
            if e.IsFSharpRecord then
                e.FSharpFields |> List.map (fun f -> f.Name, f.FieldType) |> printRecord
            else "any"
        | Fable.AnonymousRecordType(fieldNames, genArgs) -> List.zip (List.ofArray fieldNames) genArgs |> printRecord
        | Fable.MetaType -> "any" // failwith "System.Type is not supported"

    static member MakeDispatcher(helper: Fable.PluginHelper, dispatchRef, msgEntityRef, msgGenArgs) =
        let lowerFirst (str: string) =
            str.[0].ToString().ToLower() + str.[1..]

        let makeMember uciTag (uci: Fable.UnionCase) =
            let outerArgs =
                uci.UnionCaseFields
                |> List.map (fun fi -> AstUtils.makeIdent fi.Name)

            let msgArgs = outerArgs |> List.map Fable.IdentExpr
            let msg = Fable.Value(Fable.NewUnion(msgArgs, uciTag, msgEntityRef, msgGenArgs), None)
            let dispatchAction = AstUtils.makeCall (Fable.IdentExpr dispatchRef) [msg]

            lowerFirst uci.Name, Fable.Delegate(outerArgs, dispatchAction, None)

        helper.GetEntity(msgEntityRef).UnionCases
        |> List.mapi makeMember
        |> AstUtils.objExpr

type DispatcherAttribute() =
    inherit Fable.MemberDeclarationPluginAttribute()
    override _.FableMinimumVersion = "3.0.0"
    override _.Transform(_,_,decl) = decl
    override _.TransformCall(helper, _member, expr) =
        let fail() =
            helper.LogError("Cannot generate dispatcher", ?range=expr.Range)
            expr

        match expr with
        | Fable.Call(_,info,_,_) ->
            match info.Args with
            | [dispatch] ->
                match dispatch.Type.Generics with
                | Fable.DeclaredType(msgEntityRef, msgGenArgs)::_ ->
                    let bindDispatch, dispatchRef =
                        match dispatch with
                        | Fable.IdentExpr ident -> false, ident
                        | _ -> true, AstUtils.makeIdent "$dispatch"

                    let dispatcher = Helper.MakeDispatcher(helper, dispatchRef, msgEntityRef, msgGenArgs)

                    if bindDispatch then Fable.Let(dispatchRef, dispatch, dispatcher)
                    else dispatcher
                | _ -> fail()
            | _ -> fail()
        | _ -> fail()


// type DispatcherConsAttribute() =
//     inherit Fable.MemberDeclarationPluginAttribute()
//     override _.FableMinimumVersion = "3.0.0"
//     override _.Transform(_,_,decl) = decl
//     override _.TransformCall(helper, memb, expr) =
//         let fail() =
//             helper.LogError("Cannot generate dispatcher", ?range=expr.Range)
//             expr

//         match expr.Type with
//         | Fable.LambdaType(_propsType, Fable.Tuple([_storeType; Fable.LambdaType(Fable.DeclaredType(msgEntityRef, msgGenArgs),_)])) ->
//             let dispatchRef = AstUtils.makeIdent "dispatch"
//             let dispatcher = Helper.MakeDispatcher(helper, dispatchRef, msgEntityRef, msgGenArgs)
//             AstUtils.emitJs """props => {
//                 const [store, dispatch] = $0(props);
//                 return [store, $1];
//             }""" [expr; dispatcher]
//         | _ -> fail()
