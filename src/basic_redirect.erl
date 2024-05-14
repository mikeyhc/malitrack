-module(basic_redirect).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State=#{path := Path}) ->
    Req = cowboy_req:reply(maps:get(code, State, 302),
                           #{<<"Location">> => Path},
                           <<>>,
                           Req0),
    {ok, Req, State}.
