#!/bin/sh

cd "${0%/*}"
exec erl -pa "$PWD/ebin" -boot start_sasl -sname czech -s czech_app "$@"
