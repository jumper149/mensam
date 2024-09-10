module ForbidSpecificModuleImports exposing (rule, Config)

{-|

@docs Config
@docs rule

-}

import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Rule, Error)


{-|-}
type alias Config =
    List ( String, List String )


type alias Context =
    String


{-| Reports when a module in a namespace imports a specific module.

    config : List Rule
    config =
        [ ForbidSpecificImports.rule
            [ ( "App.Data", [ "App.View" ] )
            ]
        ]


## Fail

This fails because we've forbidden modules in `App.Data` to import `App.View`.

    module App.Data.Image exposing (..)

    import App.View


## Success

This passes because we've only forbidden modules in `App.Data` to import `App.View`, not the
other way around.

    module App.View exposing (..)

    import App.Data.Image


## When (not) to enable this rule

If you want to rule out whole namespaces of modules you should rather use
This rule is useful when you have set up a namespace structure where you want your module
dependencies to form a [elm-review-forbid-specific-imports](https://package.elm-lang.org/packages/webbhuset/elm-review-forbid-specific-imports/latest/ForbidSpecificImports).

-}
rule : Config -> Rule
rule config =
    Rule.newModuleRuleSchema "ForbidSpecificModuleImports" ""
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor (importVisitor config)
        |> Rule.fromModuleRuleSchema


moduleDefinitionVisitor : Node Module -> Context -> ( List (Error {}), Context )
moduleDefinitionVisitor node context =
    ( []
    , Node.value node |> Module.moduleName |> String.join "."
    )


importVisitor : Config -> Node Import -> Context -> ( List (Error {}), Context )
importVisitor config node currentModule =
    let
        importedModule =
            Node.value node |> .moduleName |> Node.value |> String.join "."
    in
    ( List.foldl
        (\( modulePrefix, importModulesForbidden ) errors ->
            if String.startsWith modulePrefix currentModule then
                List.foldl
                    (\importModuleForbidden ->
                        if importModuleForbidden == importedModule then
                            (::)
                                (Rule.error
                                    { message = errorMessage currentModule importedModule
                                    , details = errorDetails modulePrefix importModuleForbidden
                                    }
                                    (Node.range node)
                                )

                        else
                            identity
                    )
                    errors
                    importModulesForbidden

            else
                errors
        )
        []
        config
    , currentModule
    )


errorMessage : String -> String -> String
errorMessage currentModule importedModule =
    "Module `" ++ currentModule ++ "` imports `" ++ importedModule ++ "`."


errorDetails : String -> String -> List String
errorDetails modulePrefix importedModule =
    [ "Modules in `" ++ modulePrefix ++ "` must not import `" ++ importedModule ++ "`."
    ]
