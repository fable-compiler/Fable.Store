namespace Fable.SveltePlugins

open Fable.AST

[<assembly:Fable.ScanForPlugins>]
do()

type Helper =
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
