#!/bin/sh
rm -rf Mnesia.nonode@nohost
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -run erl_reader create_db -run init stop -noshell
