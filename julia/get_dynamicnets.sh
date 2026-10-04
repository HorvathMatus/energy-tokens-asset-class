#!/usr/bin/env bash
# Downloads the TVP-VAR connectedness functions of Ellington, Hanus and Barunik
# from https://github.com/barunik/DynamicNets.jl, pinned to the commit used in
# the paper. The file is not redistributed here because the upstream
# repository carries no licence.
set -euo pipefail
cd "$(dirname "$0")"
COMMIT=6426d8bc0a88d5cc30726860127b89bd74163e6b
curl -fsSL -o DynamicNets_functions.jl \
  "https://raw.githubusercontent.com/barunik/DynamicNets.jl/${COMMIT}/DynamicNets_functions.jl"
echo "Saved julia/DynamicNets_functions.jl (commit ${COMMIT})"
