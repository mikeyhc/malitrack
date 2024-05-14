-module(card_db).

-behaviour(gen_server).

%% API
-export([start_link/1, get_card/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-record(card, {name :: string(),
               faction :: string(),
               keywords :: [string()],
               versatile :: boolean(),
               role :: string(),
               limit :: pos_integer(),
               boxes :: [string()]
              }).
-type card() :: #card{}.

-record(state, {card_dir :: string(),
                cards=#{} :: #{string() => card()}}).

%%====================================================================
%% API
%%====================================================================

-spec start_link(string()) -> gen_server:start_ret().
start_link(CardDir) ->
    gen_server:start_link(?MODULE, #{dir => CardDir}, []).

-spec get_card(string() | binary()) -> {ok, #{atom() => any()}} |
                                       {error, not_found}.
get_card(Name) when is_list(Name) ->
    get_card(list_to_binary(Name));
get_card(Name) ->
    gen_server:call(malitrack_sup:get_card_db(), {get_card, Name}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(#{dir := Dir}) ->
    gen_server:cast(self(), load),
    {ok, #state{card_dir=Dir}}.

handle_call({get_card, Name}, _From, State) ->
    case maps:get(string:lowercase(Name), State#state.cards, false) of
        false -> {reply, {error, not_found}, State};
        Card -> {reply, {ok, card_to_map(Card)}, State}
    end.

handle_cast(load, S0=#state{card_dir=Dir}) ->
    Cards = load_cards(Dir),
    {noreply, S0#state{cards=Cards}}.

%%====================================================================
%% helper methods
%%====================================================================

load_cards(Dir0) ->
    {ok, Files} = file:list_dir(Dir0),
    Dir1 = case lists:suffix("/", Dir0) of
               true -> Dir0;
               false -> Dir0 ++ "/"
           end,
    CardList = lists:map(fun(F) -> read_card(Dir1 ++ F) end, Files),
    Lower = fun(V) -> list_to_binary(string:lowercase(binary_to_list(V))) end,
    lists:foldl(fun(C, M) -> M#{Lower(C#card.name) => C} end, #{}, CardList).

read_card(Path) ->
    {ok, JsonData} = file:read_file(Path),
    CardData = jsone:decode(JsonData),
    #{<<"name">> := Name,
      <<"faction">> := Faction,
      <<"keywords">> := Keywords,
      <<"versatile">> := Versatile,
      <<"role">> := Role,
      <<"boxes">> := Boxes} = CardData,
    logger:info("loaded ~s from ~s", [Name, Path]),
    Card = #card{name=Name,
                 faction=Faction,
                 keywords=Keywords,
                 versatile=Versatile,
                 role=Role,
                 limit=maps:get(<<"limit">>, CardData, 1),
                 boxes=Boxes},
    case validate_card(Card) of
        [] -> ok;
        Errors ->
            ErrorStr = string:join(Errors, "; "),
            logger:warning("~s failed validate: ~s", [Name, ErrorStr])
    end,
    Card.

card_to_map(#card{name=Name, faction=Faction, keywords=Keywords,
                  versatile=Versatile, role=Role, limit=Limit,
                  boxes=Boxes}) ->
    #{name => Name, faction => Faction, keywords => Keywords,
      versatile => Versatile, role => Role, limit => Limit,
      boxes => Boxes}.

validate_card(Card) ->
    Results = lists:map(fun(Check) -> Check(Card) end,
                        [fun valid_name/1,
                         fun valid_faction/1,
                         fun valid_keyword/1,
                         fun valid_versatile/1,
                         fun valid_role/1,
                         fun valid_boxes/1]),
    lists:filter(fun(R) -> R =/= ok end, Results).

valid_name(#card{name=Name}) when is_binary(Name) -> ok;
valid_name(_Card) -> "invalid name type".

valid_faction(#card{faction= <<"Guild">>}) -> ok;
valid_faction(#card{faction= <<"Resurrectionists">>}) -> ok;
valid_faction(#card{faction= <<"Neverborn">>}) -> ok;
valid_faction(#card{faction= <<"Arcanists">>}) -> ok;
valid_faction(#card{faction= <<"Outcasts">>}) -> ok;
valid_faction(#card{faction= <<"Bayou">>}) -> ok;
valid_faction(#card{faction= <<"Ten Thunders">>}) -> ok;
valid_faction(#card{faction= <<"Explorer's Society">>}) -> ok;
valid_faction(#card{faction=Faction}) ->
    lists:flatten(io_lib:format("invalid faction: ~s", [Faction])).

valid_keyword(_Keywords) -> ok.

valid_versatile(#card{versatile=Versatile}) when is_boolean(Versatile) -> ok;
valid_versatile(_Card) -> "invalid versatile type".

valid_role(#card{role= <<"Master">>}) -> ok;
valid_role(#card{role= <<"Henchman">>}) -> ok;
valid_role(#card{role= <<"Enforcer">>}) -> ok;
valid_role(#card{role= <<"Minion">>}) -> ok;
valid_role(#card{role=Role}) ->
    lists:flatten(io_lib:format("invalid role: ~s", [Role])).

valid_boxes(_Boxes) -> ok.
