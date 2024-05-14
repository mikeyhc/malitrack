-module(card_handler).
-behaviour(cowboy_rest).

-export([init/2, resource_exists/2, to_html/2]).

init(Req, State) ->
    {cowboy_rest, Req, State#{card => false}}.

resource_exists(Req, State0) ->
    CardName = binary_to_list(cowboy_req:binding(name, Req)),
    {Found, State1} = case card_db:get_card(CardName) of
                          {ok, Card} -> {true, State0#{card => Card}};
                          {error, not_found} -> {false, State0}
                      end,
    {Found, Req, State1}.

to_html(Req, State=#{card := Card=#{name := Name,
                                    keywords := Keywords,
                                    factions := Factions,
                                    boxes := Boxes},
                     template := Template}) ->
    Body = Template(#{title => Name,
                      card => Card#{keywords => comma_separate(Keywords),
                                    factions => comma_separate(Factions)
                                   },
                      box_count => max(length(Boxes), 1)}),
    {Body, Req, State}.

comma_separate([H|T]) ->
    lists:foldl(fun(V, Acc) -> <<Acc/binary, ",", V/binary>> end, H, T).
