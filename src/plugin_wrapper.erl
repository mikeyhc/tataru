-module(plugin_wrapper).

-export([install/1]).

install({M, F, A}) ->
    apply(M, F, A).
