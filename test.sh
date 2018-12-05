#!/bin/bash
cabal new-run spec -- --format=progress --color
cabal new-run doctest
