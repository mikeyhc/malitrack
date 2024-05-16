-module(unit_handler).
-behaviour(cowboy_rest).

-export([init/2, resource_exists/2, to_html/2]).

init(Req, State) ->
    {cowboy_rest, Req, State#{unit => false}}.

resource_exists(Req, State0) ->
    UnitName = binary_to_list(cowboy_req:binding(name, Req)),
    {Found, State1} = case unit_db:get_unit(UnitName) of
                          {ok, Unit} -> {true, State0#{unit => Unit}};
                          {error, not_found} -> {false, State0}
                      end,
    {Found, Req, State1}.

to_html(Req, State=#{unit := Unit=#{name := Name,
                                    keywords := Keywords,
                                    factions := Factions},
                     template := Template}) ->
    Body = Template(#{title => Name,
                      unit => Unit#{keywords => comma_separate(Keywords),
                                    factions => comma_separate(Factions)
                                   }}),
    {Body, Req, State}.

comma_separate([H|T]) ->
    lists:foldl(fun(V, Acc) -> <<Acc/binary, ",", V/binary>> end, H, T).
