#!/bin/sh

erl -noshell -pa ebin -s kitty start client -s init stop
