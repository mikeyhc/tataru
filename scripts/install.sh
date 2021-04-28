#!/bin/sh

erl -name "tataru@$(hostname -f)" \
    -eval 'mnesia:create_schema([node()]), halt()' 1>/dev/null
