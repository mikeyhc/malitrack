-module(card_db).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, get_card/1, default_card_file/0,
         get_keyword/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-record(card, {name :: string(),
               factions :: string(),
               keywords :: [string()],
               versatile :: boolean(),
               station :: string(),
               limit :: pos_integer(),
               boxes :: [string()]
              }).
-type card() :: #card{}.

-record(state, {card_file    :: string(),
                cards=#{}    :: #{string() => card()},
                keywords=#{} :: #{string() => [string()]}}).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> gen_server:start_ret().
start_link() ->
    start_link(default_card_file()).

-spec start_link(string()) -> gen_server:start_ret().
start_link(CardFile) ->
    gen_server:start_link(?MODULE, #{card_file => CardFile}, []).

-spec get_card(string() | binary()) -> {ok, #{atom() => any()}} |
                                       {error, not_found}.
get_card(Name) when is_list(Name) ->
    get_card(list_to_binary(Name));
get_card(Name) ->
    gen_server:call(malitrack_sup:get_card_db(), {get_card, Name}).

-spec get_keyword(string() | binary()) -> {ok, [string()]} |
                                          {error, not_found}.
get_keyword(Name) when is_list(Name) ->
    get_keyword(list_to_binary(Name));
get_keyword(Name) ->
    gen_server:call(malitrack_sup:get_card_db(), {get_keyword, Name}).

-spec default_card_file() -> string().
default_card_file() ->
    {ok, CardFileEnv} = application:get_env(malitrack, card_file),
    filename:join([code:priv_dir(malitrack), CardFileEnv]).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(#{card_file := CardFile}) ->
    gen_server:cast(self(), load),
    {ok, #state{card_file=CardFile}}.

handle_call({get_card, Name}, _From, State) ->
    case maps:get(string:lowercase(Name), State#state.cards, false) of
        false -> {reply, {error, not_found}, State};
        Card -> {reply, {ok, card_to_map(Card)}, State}
    end;
handle_call({get_keyword, Keyword}, _From, State) ->
    case maps:get(string:lowercase(Keyword), State#state.keywords, false) of
        false -> {reply, {error, not_found}, State};
        Cards -> {reply, {ok, Cards}, State}
    end.

handle_cast(load, S0=#state{card_file=CardFile}) ->
    Cards = load_cards(CardFile),
    logger:info("loaded ~p cards~n", [maps:size(Cards)]),
    Keywords = generate_keyword_map(Cards),
    logger:info("loaded ~p keywords~n", [maps:size(Keywords)]),
    {noreply, S0#state{cards=Cards, keywords=Keywords}}.

%%====================================================================
%% helper methods
%%====================================================================

load_cards(CardFile) ->
    {ok, JsonData} = file:read_file(CardFile),
    #{<<"units">> := Units} = jsone:decode(JsonData),
    CardList = lists:map(fun({_, Card}) -> read_card(Card) end,
                         maps:to_list(Units)),
    lists:foldl(fun(C, M) -> M#{make_key(C#card.name) => C} end, #{}, CardList).

read_card(CardData) ->
    #{<<"name">> := Name,
      <<"factions">> := Factions,
      <<"keywords">> := Keywords,
      <<"station">> := Station} = CardData,
    logger:debug("loaded card: ~s", [Name]),
    Card = #card{name=Name,
                 factions=Factions,
                 keywords=Keywords,
                 versatile=maps:get(<<"versatile">>, CardData, false),
                 station=Station,
                 limit=maps:get(<<"limit">>, CardData, 1),
                 boxes=[]},
    case validate_card(Card) of
        [] -> ok;
        Errors ->
            ErrorStr = string:join(Errors, "; "),
            logger:warning("~s failed validate: ~s", [Name, ErrorStr])
    end,
    Card.

generate_keyword_map(Cards) ->
    Fn = fun({Key, #card{keywords=Keywords}}, Acc) ->
                 IKW = fun(KW, A) ->
                               maps:update_with(make_key(KW),
                                                fun(V) -> [Key|V] end,
                                                [], A)
                       end,
                 lists:foldl(IKW, Acc, Keywords)
         end,
    Map = lists:foldl(Fn, #{}, maps:to_list(Cards)),
    lists:foreach(fun(Key) -> logger:debug("loaded keyword: ~s", [Key]) end,
                  maps:keys(Map)),
    Map.


card_to_map(#card{name=Name, factions=Factions, keywords=Keywords,
                  versatile=Versatile, station=Station, limit=Limit,
                  boxes=Boxes}) ->
    #{name => Name, factions => Factions, keywords => Keywords,
      versatile => Versatile, station => Station, limit => Limit,
      boxes => Boxes}.

validate_card(Card) ->
    Results = lists:map(fun(Check) -> Check(Card) end,
                        [fun valid_name/1,
                         fun valid_factions/1,
                         fun valid_keyword/1,
                         fun valid_versatile/1,
                         fun valid_station/1,
                         fun valid_boxes/1]),
    lists:filter(fun(R) -> R =/= ok end, Results).

valid_name(#card{name=Name}) when is_binary(Name) -> ok;
valid_name(_Card) -> "invalid name type".

valid_factions(#card{factions=Factions}) ->
    FactionList = [<<"Guild">>,
                   <<"Resurrectionist">>,
                   <<"Neverborn">>,
                   <<"Arcanists">>,
                   <<"Outcasts">>,
                   <<"Bayou">>,
                   <<"Ten Thunders">>,
                   <<"Explorer's Society">>,
                   <<"Dead Man's Hand">>],
    InFaction = fun(F) -> lists:any(fun(G) -> F =:= G end, FactionList) end,
    case lists:all(InFaction, Factions) of
        true -> ok;
        false ->
            lists:flatten(io_lib:format("invalid factions: ~s", [Factions]))
    end.

valid_keyword(_Keywords) -> ok.

valid_versatile(#card{versatile=Versatile}) when is_boolean(Versatile) -> ok;
valid_versatile(_Card) -> "invalid versatile type".

valid_station(#card{station= <<"Master">>}) -> ok;
valid_station(#card{station= <<"Henchman">>}) -> ok;
valid_station(#card{station= <<"Enforcer">>}) -> ok;
valid_station(#card{station= <<"Minion">>}) -> ok;
valid_station(#card{station=Station}) ->
    lists:flatten(io_lib:format("invalid station: ~s", [Station])).

valid_boxes(_Boxes) -> ok.

make_key(V0) ->
    V1 = string:lowercase(binary_to_list(V0)),
    V2 = lists:flatten(string:replace(V1, " ", "-")),
    list_to_binary(V2).
