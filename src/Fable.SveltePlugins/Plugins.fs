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
            | [Fable.Lambda(msgArg, Fable.Lambda(_, Fable.Call(update,_,_,_), None), None); store] ->
                match msgArg.Type with
                | Fable.DeclaredType(msgEntityRef, msgGenArgs) ->                    
                    let modelIdent = AstUtils.makeIdent "model"
                    let storeUpdateGetterKind = Fable.ByKey(Fable.ExprKey(AstUtils.makeStrConst "update"))
                    let storeUpdateGetter = Fable.Get(store, storeUpdateGetterKind, Fable.Any, None)
                    
                    let lowerFirst (str: string) =
                        str.[0].ToString().ToLower() + str.[1..]

                    let makeMember uciTag (uci: Fable.UnionCase) =
                        let outerArgs =
                            uci.UnionCaseFields
                            |> List.map (fun fi -> AstUtils.makeIdent fi.Name)

                        let msgArgs = outerArgs |> List.map Fable.IdentExpr
                        let msg = Fable.Value(Fable.NewUnion(msgArgs, uciTag, msgEntityRef, msgGenArgs), None)
                        
                        let elmishUpdate = AstUtils.makeCall (Fable.TypeCast(update, Fable.Any, None)) [msg; Fable.IdentExpr modelIdent]
                        let storeUpdate = AstUtils.makeCall storeUpdateGetter [Fable.Delegate([modelIdent], elmishUpdate, None)]
            
                        lowerFirst uci.Name, Fable.Delegate(outerArgs, storeUpdate, None)

                    helper.GetEntity(msgEntityRef).UnionCases
                    |> List.mapi makeMember
                    |> AstUtils.objExpr

                | _ -> fail()                
            | _ -> fail()
        | _ -> fail()
