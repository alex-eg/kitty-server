#!/bin/sh

erl -noshell -pa ebin -s kitty start server -s init stop
