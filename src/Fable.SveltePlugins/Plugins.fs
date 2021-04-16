namespace SveltePlugins

open System
open System.Text.RegularExpressions
open Fable.AST

[<assembly:Fable.ScanForPlugins>]
do()

type DeclaredTypePrinter = int -> Fable.Entity -> (* genArgs *) Fable.Type list -> string

type Helper =
    static member LowerFirst (str: string) =
            str.[0].ToString().ToLower() + str.[1..]

    static member Indent(indent, str) =
        (String.replicate (4 * indent) " ") + str

    static member PrintFunction(h, p, i, args, ?t, ?arrow) =
        let printArg (name: string, arg) =
            $"{name}: {Helper.PrintType(h, p, i, arg)}"

        let separator = if defaultArg arrow true then " => " else ": "
        let t = match t with Some t -> Helper.PrintType(h, p, i, t) | None -> "void"

        $"""({args |> List.map printArg |> String.concat ", "}){separator}{t}"""

    static member PrintType(h: Fable.PluginHelper, p: DeclaredTypePrinter, indent: int, t: Fable.Type) =
        let makeArgs argTpypes =
            argTpypes |> List.mapi (fun i t -> $"a{i}", t)

        let printRecord fields =
            [
                "{"
                for (name, t) in fields do
                    $"""{Helper.Indent(indent + 1, name)}: {Helper.PrintType(h, p, indent + 1, t)},"""
                Helper.Indent(indent, "}")
            ] |> String.concat Environment.NewLine

        match t with
        | Fable.Any -> "any"
        | Fable.Unit -> "undefined"
        | Fable.Boolean -> "boolean"
        | Fable.Char
        | Fable.String -> "string"
        | Fable.Regex -> "RegExp"
        | Fable.Enum _
        | Fable.Number _ -> "number"
        | Fable.Option t -> $"({Helper.PrintType(h, p, indent, t)}|undefined)"
        | Fable.Tuple ts -> $"""[{ts |> List.map (fun t -> Helper.PrintType(h, p, indent, t)) |> String.concat ", "}]"""
        | Fable.Array t -> Helper.PrintType(h, p, indent, t) + "[]"
        | Fable.List t -> $"Iterable<{Helper.PrintType(h, p, indent, t)}>"
        | Fable.DelegateType(args, ret)
        | AstUtils.NestedLambdaType(args, ret) ->
            Helper.PrintFunction(h, p, indent, makeArgs args, ret)
        // This is caught by the active pattern above but we need it
        // for the pattern matching to be exhaustive
        | Fable.LambdaType _ -> failwith "unexpected"
        | Fable.GenericParam name -> $"/* {name} */ any" // name // TODO: print generic args in function/type
        | Fable.DeclaredType(e, genArgs) ->
            let e = h.GetEntity(e)
            if e.IsFSharpRecord then
                e.FSharpFields |> List.map (fun f -> f.Name, f.FieldType) |> printRecord
            else
                match e.FullName, genArgs with
                | "System.Guid", _ -> "string"
                | "Microsoft.FSharp.Collections.FSharpMap`2", [k; v] ->
                    $"Iterable<[{Helper.PrintType(h, p, indent, k)}, {Helper.PrintType(h, p, indent, v)}]>"
                | _ -> p indent e genArgs
        | Fable.AnonymousRecordType(fieldNames, genArgs) -> List.zip (List.ofArray fieldNames) genArgs |> printRecord
        | Fable.MetaType -> "any" // failwith "System.Type is not supported"

    static member MakeDispatcher(helper: Fable.PluginHelper, dispatchRef, msgEntityRef, msgGenArgs) =
        let makeMember uciTag (uci: Fable.UnionCase) =
            let outerArgs =
                uci.UnionCaseFields
                |> List.map (fun fi -> AstUtils.makeIdent fi.Name)

            let msgArgs = outerArgs |> List.map Fable.IdentExpr
            let msg = Fable.Value(Fable.NewUnion(msgArgs, uciTag, msgEntityRef, msgGenArgs), None)
            let dispatchAction = AstUtils.makeCall (Fable.IdentExpr dispatchRef) [msg]

            Helper.LowerFirst uci.Name, Fable.Delegate(outerArgs, dispatchAction, None)

        helper.GetEntity(msgEntityRef).UnionCases
        |> List.mapi makeMember
        |> AstUtils.objExpr

type DispatcherAttribute() =
    inherit Fable.MemberDeclarationPluginAttribute()
    override _.FableMinimumVersion = "3.1.15"
    override _.Transform(_,_,decl) = decl
    override _.TransformCall(helper, _member, expr) =
        let fail() =
            helper.LogError("Cannot generate dispatcher", ?range=expr.Range)
            expr

        match expr with
        | Fable.Call(_,info,t,_) ->
            match info.Args with
            | [dispatch] ->
                match dispatch.Type.Generics with
                | Fable.DeclaredType(msgEntityRef, msgGenArgs)::_ ->
                    let bindDispatch, dispatchRef =
                        match dispatch with
                        | Fable.IdentExpr ident -> false, ident
                        | _ -> true, AstUtils.makeIdent "$dispatch"

                    let dispatcher = Helper.MakeDispatcher(helper, dispatchRef, msgEntityRef, msgGenArgs)
                    let dispatcher =
                        if bindDispatch then Fable.Let(dispatchRef, dispatch, dispatcher)
                        else dispatcher
                    Fable.TypeCast(dispatcher, t, None)
                | _ -> fail()
            | _ -> fail()
        | _ -> fail()

type GenerateDeclarationAttribute() =
    inherit Fable.MemberDeclarationPluginAttribute()
    override _.FableMinimumVersion = "3.0.0"
    override _.TransformCall(_,_,expr) = expr
    override _.Transform(h, _, decl) =
        let rec printer indent (e: Fable.Entity) genArgs =
            match e.FullName, genArgs with
            | "SvelteStore.IDispatcher`1", [Fable.DeclaredType(e, _)] ->
                let e = h.GetEntity(e)
                [
                    "{"
                    for uci in e.UnionCases do
                        let args = uci.UnionCaseFields |> List.map (fun f -> f.Name, f.FieldType)
                        let fn = Helper.PrintFunction(h, printer, indent, args, arrow=false)
                        $"""{Helper.Indent(indent + 1, Helper.LowerFirst uci.Name)}{fn},"""
                    Helper.Indent(indent, "}")

                ] |> String.concat Environment.NewLine

            | "SvelteStore.IWritable`1", [t]
            | "SvelteStore.IReadable`1", [t] ->
                $"Readable<{Helper.PrintType(h, printer, indent, t)}>"
            | fullname, _ -> $"/* {fullname} */ any"

        let args, returnType =
            if decl.Info.IsValue then
                match decl.Body.Type with
                | AstUtils.NestedLambdaType(args, ret) ->
                    args |> List.mapi (fun i t -> $"a{i}", t), ret
                | _ -> failwith "Only functions are supported at the moment"
            else
                decl.Args |> List.map (fun a -> a.Name, a.Type), decl.Body.Type

        let args =
            match args with
            | [_, Fable.Unit] -> []
            | args -> args

        let path = Regex.Replace(h.GetOutputPath(), @"\.js$", ".d.ts")
        let text = $"""import {{ Readable }} from "svelte/store";

export function {decl.Name}{Helper.PrintFunction(h, printer, 0, args, returnType, arrow=false)};"""
        IO.File.WriteAllText(path, text)
        decl
