-module(crews_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:reply(404, #{}, <<>>, Req0),
    {ok, Req, State}.
