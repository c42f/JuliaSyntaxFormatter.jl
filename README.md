# JuliaSyntaxFormatter

[![Build Status](https://github.com/c42f/JuliaSyntaxFormatter.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/c42f/JuliaSyntaxFormatter.jl/actions/workflows/CI.yml?query=branch%3Amain)

A nascent package to format `JuliaLowering.SyntaxTree` as Julia source code
with support for "semantic colouring". For example, one can run JuliaLowering's
scope analysis pass to determine the identity of each name in the code (tagging
it with a `var_id`). Then color this accordingly.

To try this out, you need the `main` (ie development) branch of JuliaSyntax.jl

Currently a pile of hacks, probably broken in various ways. Use at your own
risk, etc :)
