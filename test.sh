#!/bin/bash
cabal new-run -v0 -- spec --color
cabal new-run -v0 -- doctests
