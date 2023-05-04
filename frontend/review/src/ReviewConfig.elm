module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import NoBooleanCase
import NoConfusingPrefixOperator
import NoDeprecated
import NoDuplicatePorts
import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
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
    [ NoBooleanCase.rule
    , NoConfusingPrefixOperator.rule
    , NoDeprecated.rule NoDeprecated.defaults
    , NoDuplicatePorts.rule
    , NoImportingEverything.rule []
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeExpose.rule
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
