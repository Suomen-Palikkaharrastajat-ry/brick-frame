module ReviewConfig exposing (config)

{-| elm-review configuration for this codebase.

## Goal: LLM coding agent optimisation

These rules enforce conventions that keep the codebase easy for LLM coding
agents to read, understand, and modify correctly. Each rule targets a specific
failure mode:

  - **NoTailwindRawStrings** — typed tokens are discoverable; raw strings are not
  - **RequireModuleDoc** — module docs give agents orientation without reading all code
  - **RequireTypeAnnotation** — type signatures are the contract; agents rely on them
  - **NoExposingEverything** — explicit export lists reveal the public API at a glance

-}

import LlmAgent.NoExposingEverything
import LlmAgent.NoTailwindRawStrings
import LlmAgent.RequireModuleDoc
import LlmAgent.RequireTypeAnnotation
import Review.Rule as Rule exposing (Rule)


-- Directories containing auto-generated Elm code that should not be linted.
generatedDirectories : List String
generatedDirectories =
    [ ".elm-tailwind/" ]


-- Individual generated files that should not be linted.
generatedFiles : List String
generatedFiles =
    [ "src/Data.elm" ]


config : List Rule
config =
    [ -- Enforce typed Tailwind tokens over opaque raw strings.
      -- Typed tokens are statically verifiable and consistently named.
      LlmAgent.NoTailwindRawStrings.rule

    -- Every module must have a {-| ... -} documentation comment.
    -- The module doc is the first thing an agent reads; it sets context for
    -- everything that follows.
    , LlmAgent.RequireModuleDoc.rule

    -- Every top-level function/value must have a type annotation.
    -- Type annotations are the primary interface contract. Agents use them to
    -- understand inputs and outputs without tracing through implementations.
    , LlmAgent.RequireTypeAnnotation.rule

    -- Explicit export lists over exposing (..).
    -- Wildcard exports conceal the public/private boundary from agents.
    , LlmAgent.NoExposingEverything.rule
    ]
        |> List.map (Rule.ignoreErrorsForDirectories generatedDirectories)
        |> List.map (Rule.ignoreErrorsForFiles generatedFiles)
