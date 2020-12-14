namespace Fable.SveltePlugins

open Fable.AST

[<assembly:Fable.ScanForPlugins>]
do()

type DispatcherAttribute() =
    inherit Fable.MemberDeclarationPluginAttribute()
    override _.FableMinimumVersion = "3.0.0"
    override _.Transform(_,_,decl) = decl
    override _.TransformCall(helper, member_, expr) =
        let fail() =
            helper.LogError("Cannot generate dispatcher", ?range=expr.Range)
            expr

        match expr with
        | Fable.Call(_,info,_,_) ->
            match info.Args with
            | [dispatch] ->
                match dispatch.Type.Generics with
                | Fable.DeclaredType(msgEntityRef, msgGenArgs)::_ ->
                    let lowerFirst (str: string) =
                        str.[0].ToString().ToLower() + str.[1..]

                    let bindDispatch, dispatchRef =
                        match dispatch with
                        | Fable.IdentExpr ident -> false, ident
                        | _ -> true, AstUtils.makeIdent "$dispatch"

                    let makeMember uciTag (uci: Fable.UnionCase) =
                        let outerArgs =
                            uci.UnionCaseFields
                            |> List.map (fun fi -> AstUtils.makeIdent fi.Name)

                        let msgArgs = outerArgs |> List.map Fable.IdentExpr
                        let msg = Fable.Value(Fable.NewUnion(msgArgs, uciTag, msgEntityRef, msgGenArgs), None)
                        let dispatchAction = AstUtils.makeCall (Fable.IdentExpr dispatchRef) [msg]
            
                        lowerFirst uci.Name, Fable.Delegate(outerArgs, dispatchAction, None)

                    let dispatcher =
                        helper.GetEntity(msgEntityRef).UnionCases
                        |> List.mapi makeMember
                        |> AstUtils.objExpr

                    if bindDispatch then Fable.Let(dispatchRef, dispatch, dispatcher)
                    else dispatcher
                | _ -> fail()                
            | _ -> fail()
        | _ -> fail()
