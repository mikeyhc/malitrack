-module(unit_db).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, get_unit/1, default_unit_file/0,
         get_keyword/1, reload/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-record(unit, {name :: binary(),
               factions :: binary(),
               keywords :: [binary()],
               versatile :: boolean(),
               station :: binary(),
               limit :: pos_integer(),
               errors=[] :: [binary()]
              }).
-type unit() :: #unit{}.

-record(state, {unit_file    :: string(),
                units=#{}    :: #{string() => unit()},
                keywords=#{} :: #{string() => [string()]}}).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> gen_server:start_ret().
start_link() ->
    start_link(default_unit_file()).

-spec start_link(string()) -> gen_server:start_ret().
start_link(UnitFile) ->
    gen_server:start_link(?MODULE, #{unit_file => UnitFile}, []).

-spec get_unit(string() | binary()) -> {ok, #{atom() => any()}} |
                                       {error, not_found}.
get_unit(Name) when is_list(Name) ->
    get_unit(list_to_binary(Name));
get_unit(Name) ->
    gen_server:call(malitrack_sup:get_unit_db(), {get_unit, Name}).

-spec get_keyword(string() | binary()) -> {ok, [string()]} |
                                          {error, not_found}.
get_keyword(Name) when is_list(Name) ->
    get_keyword(list_to_binary(Name));
get_keyword(Name) ->
    gen_server:call(malitrack_sup:get_unit_db(), {get_keyword, Name}).

-spec default_unit_file() -> string().
default_unit_file() ->
    {ok, UnitFileEnv} = application:get_env(malitrack, unit_file),
    filename:join([code:priv_dir(malitrack), UnitFileEnv]).

-spec reload() -> ok.
reload() ->
    gen_server:cast(malitrack_sup:get_unit_db(), load).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(#{unit_file := UnitFile}) ->
    gen_server:cast(self(), load),
    {ok, #state{unit_file=UnitFile}}.

handle_call({get_unit, Name}, _From, State) ->
    case maps:get(make_key(Name), State#state.units, false) of
        false -> {reply, {error, not_found}, State};
        Unit -> {reply, {ok, unit_to_map(Unit)}, State}
    end;
handle_call({get_keyword, Keyword}, _From, State) ->
    case maps:get(make_key(Keyword), State#state.keywords, false) of
        false -> {reply, {error, not_found}, State};
        Units -> {reply, {ok, Units}, State}
    end.

handle_cast(load, S0=#state{unit_file=UnitFile}) ->
    Units = load_units(UnitFile),
    logger:info("loaded ~p units", [maps:size(Units)]),
    Keywords = generate_keyword_map(Units),
    logger:info("loaded ~p keywords", [maps:size(Keywords)]),
    {noreply, S0#state{units=Units, keywords=Keywords}}.

%%====================================================================
%% helper methods
%%====================================================================

load_units(UnitFile) ->
    {ok, JsonData} = file:read_file(UnitFile),
    #{<<"units">> := Units} = jsone:decode(JsonData),
    UnitList = lists:map(fun({_, Unit}) -> read_unit(Unit) end,
                         maps:to_list(Units)),
    lists:foldl(fun(C, M) -> M#{make_key(C#unit.name) => C} end, #{}, UnitList).

read_unit(UnitData) ->
    #{<<"name">> := Name,
      <<"factions">> := Factions,
      <<"keywords">> := Keywords,
      <<"station">> := Station} = UnitData,
    logger:debug("loaded unit: ~s", [Name]),
    Unit = #unit{name=Name,
                 factions=Factions,
                 keywords=Keywords,
                 versatile=maps:get(<<"versatile">>, UnitData, false),
                 station=Station,
                 limit=maps:get(<<"limit">>, UnitData, 1)},
    case validate_unit(Unit) of
        [] -> Unit;
        Errors ->
            WarnFn = fun(Msg) ->
                             logger:warning("unit \"~s\" failed validation: ~s",
                                            [Name, Msg])
                     end,
            lists:foreach(WarnFn, Errors),
            Unit#unit{errors=Errors}
    end.

generate_keyword_map(Units) ->
    Fn = fun({Key, #unit{keywords=Keywords}}, Acc) ->
                 IKW = fun(KW, A) ->
                               maps:update_with(make_key(KW),
                                                fun(V) -> [Key|V] end,
                                                [Key], A)
                       end,
                 lists:foldl(IKW, Acc, Keywords)
         end,
    Map = lists:foldl(Fn, #{}, maps:to_list(Units)),
    lists:foreach(fun(Key) -> logger:debug("loaded keyword: ~s", [Key]) end,
                  maps:keys(Map)),
    Map.


unit_to_map(#unit{name=Name, factions=Factions, keywords=Keywords,
                  versatile=Versatile, station=Station, limit=Limit,
                  errors=Errors}) ->
    #{name => Name, factions => Factions, keywords => Keywords,
      versatile => Versatile, station => Station, limit => Limit,
      errors => Errors}.

validate_unit(Unit) ->
    Results = lists:map(fun(Check) -> Check(Unit) end,
                        [fun valid_name/1,
                         fun valid_factions/1,
                         fun valid_keyword/1,
                         fun valid_versatile/1,
                         fun valid_station/1,
                         fun valid_boxes/1]),
    lists:filter(fun(R) -> R =/= ok end, Results).

valid_name(#unit{name=Name}) when is_binary(Name) -> ok;
valid_name(_Unit) -> <<"invalid name type">>.

valid_factions(#unit{factions=Factions}) ->
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
            list_to_binary(
              lists:flatten(io_lib:format("invalid factions: ~s", [Factions])))
    end.

valid_keyword(_Keywords) -> ok.

valid_versatile(#unit{versatile=Versatile}) when is_boolean(Versatile) -> ok;
valid_versatile(_Unit) -> <<"invalid versatile type">>.

valid_station(#unit{station= <<"Master">>}) -> ok;
valid_station(#unit{station= <<"Henchman">>}) -> ok;
valid_station(#unit{station= <<"Enforcer">>}) -> ok;
valid_station(#unit{station= <<"Minion">>}) -> ok;
valid_station(#unit{station=Station}) ->
    list_to_binary(lists:flatten(io_lib:format("invalid station: ~s",
                                               [Station]))).

valid_boxes(#unit{name=Name}) ->
    case box_db:get_boxes_for_unit(Name) of
        [] ->
            list_to_binary(
              lists:flatten(io_lib:format("no boxes for ~s", [Name])));
        _ -> ok
    end.

make_key(V0) ->
    V1 = string:lowercase(binary_to_list(V0)),
    V2 = lists:flatten(string:replace(V1, " ", "-", all)),
    list_to_binary(V2).
