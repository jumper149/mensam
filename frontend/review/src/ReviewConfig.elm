module ReviewConfig exposing (config)

import ForbidSpecificModuleImports
import ForbidSpecificImports
import NoBooleanCase
import NoConfusingPrefixOperator
import NoDeprecated
import NoDuplicatePorts
import NoExposingEverything
import NoImportingEverything
import NoInconsistentAliases
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoModuleOnExposedNames
import NoPrematureLetComputation
import NoUnmatchedUnit
import NoUnsafePorts
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import NoUnusedPorts
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ ForbidSpecificImports.rule []
    , ForbidSpecificModuleImports.rule
        [ ( "Mensam"
          , [ "Element.Events"
            ]
          )
        ]
    , NoBooleanCase.rule
    , NoConfusingPrefixOperator.rule
    , NoDeprecated.rule NoDeprecated.defaults
    , NoDuplicatePorts.rule
    , NoImportingEverything.rule []
    , NoInconsistentAliases.rule <|
        NoInconsistentAliases.noMissingAliases <|
            NoInconsistentAliases.config
                [ ( "Json.Decode", "Decode" )
                , ( "Json.Encode", "Encode" )
                ]
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeExpose.rule
    , NoModuleOnExposedNames.rule
    , NoPrematureLetComputation.rule
    , NoUnmatchedUnit.rule
    , NoUnsafePorts.rule NoUnsafePorts.any
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , NoUnusedPorts.rule
    ]
