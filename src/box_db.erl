-module(box_db).

-behaviour(gen_server).

% API
-export([start_link/0, start_link/1, get_box/1, get_boxes_for_unit/1,
         default_box_file/0, validate_boxes/1, reload/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-record(box, {name :: binary(),
              contents :: #{binary() => pos_integer()},
              price :: number(),
              url :: binary()
             }).
-type box() :: #box{}.

-record(state, {box_file        :: string(),
                boxes=#{}       :: #{string() => box()},
                unit_to_box=#{} :: #{string() => string()}
               }).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> gen_server:start_ret().
start_link() ->
    start_link(default_box_file()).

-spec start_link(string()) -> gen_server:start_ret().
start_link(BoxFile) ->
    gen_server:start_link(?MODULE, #{box_file => BoxFile}, []).

-spec get_box(string() | binary()) -> {ok, #{atom() => any()}} |
                                      {error, not_found}.
get_box(Name) when is_list(Name) ->
    get_box(list_to_binary(Name));
get_box(Name) ->
    gen_server:call(malitrack_sup:get_box_db(), {get_box, Name}).

-spec get_boxes_for_unit(string() | binary()) -> {ok, #{atom() => any()}} |
                                                 {error, not_found}.
get_boxes_for_unit(UnitName) when is_list(UnitName) ->
    get_boxes_for_unit(list_to_binary(UnitName));
get_boxes_for_unit(UnitName) ->
    gen_server:call(malitrack_sup:get_box_db(), {get_boxes_for_unit, UnitName}).

-spec validate_boxes([string() | binary()]) -> ok.
validate_boxes(L=[H|_]) when is_list(H) ->
    validate_boxes(lists:map(fun list_to_binary/1, L));
validate_boxes(Units) ->
    gen_server:cast(malitrack_sup:get_box_db(), {validate, Units}).

-spec default_box_file() -> string().
default_box_file() ->
    {ok, BoxFileEnv} = application:get_env(malitrack, box_file),
    filename:join([code:priv_dir(malitrack), BoxFileEnv]).

-spec reload() -> ok.
reload() ->
    gen_server:cast(malitrack_sup:get_box_db(), load).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(#{box_file := BoxFile}) ->
    gen_server:cast(self(), load),
    {ok, #state{box_file=BoxFile}}.

handle_call({get_box, Name}, _From, State) ->
    Reply = case maps:get(make_key(Name), State#state.boxes, false) of
                false -> {error, not_found};
                Box -> {ok, box_to_map(Box)}
            end,
    {reply, Reply, State};
handle_call({get_boxes_for_unit, Unit}, _From, State) ->
    Reply = maps:get(make_key(Unit), State#state.unit_to_box, []),
    {reply, Reply, State}.

handle_cast(load, S0=#state{box_file=BoxFile}) ->
    Boxes = load_boxes(BoxFile),
    logger:info("loaded ~p boxes", [maps:size(Boxes)]),
    Units = generate_units_map(Boxes),
    logger:info("generated ~p unit -> box lookups", [maps:size(Units)]),
    {noreply, S0#state{boxes=Boxes, unit_to_box=Units}}.

%%====================================================================
%% helper methods
%%====================================================================

load_boxes(BoxFile) ->
    {ok, JsonData} = file:read_file(BoxFile),
    Boxes = lists:map(fun read_box/1, jsone:decode(JsonData)),
    lists:foldl(fun(B, A) -> A#{make_key(B#box.name) => B} end, #{}, Boxes).

read_box(#{<<"name">> := Name,
           <<"contents">> := Contents,
           <<"price">> := Price,
           <<"url">> := URL}) ->
    Box = #box{name=Name, contents=Contents, price=Price, url=URL},
    case validate_box(Box) of
        [] -> ok;
        Errors ->
            ErrorStr = string:join(Errors, "; "),
            logger:warning("box \"~s\" failed validation: ~s", [Name, ErrorStr])
    end,
    Box.

generate_units_map(Boxes) ->
    Fn0 = fun(Name, #box{contents=Contents}, Acc) ->
                  Fn1 = fun(Unit, Qty, A) ->
                                maps:update_with(make_key(Unit),
                                                 fun(V) -> [{Name, Qty}|V] end,
                                                 [{Name, Qty}], A)
                        end,
                  maps:fold(Fn1, Acc, Contents)
          end,
    maps:fold(Fn0, #{}, Boxes).

validate_box(Box) ->
    Results = lists:map(fun(Check) -> Check(Box) end,
                        [fun valid_name/1,
                         fun valid_contents/1,
                         fun valid_price/1,
                         fun valid_url/1
                        ]),
    lists:filter(fun(R) -> R =/= ok end, Results).

valid_name(#box{name=Name}) when is_binary(Name) -> ok;
valid_name(_Box) -> "invalid name type".

valid_contents(#box{contents=Contents}) ->
    Valid = fun({Name, Qty}) -> not (is_binary(Name) andalso is_integer(Qty)
                                     andalso Qty > 0);
               (_) -> true
            end,
    case lists:filter(Valid, maps:to_list(Contents)) of
        [] -> ok;
        Invalid ->
            lists:flatten(io_lib:format("invalid contents: ~p", [Invalid]))
    end.

valid_price(#box{price=Price}) when is_number(Price) andalso Price > 0 -> ok;
valid_price(#box{price=Price}) ->
    lists:flatten(io_lib:format("invalid price: ~p", [Price])).

valid_url(#box{url=URL}) when is_binary(URL) -> ok;
valid_url(_Box) -> "invalid url type".

box_to_map(#box{name=Name, contents=Contents, price=Price}) ->
    #{name => Name, contents => Contents, price => Price}.

make_key(V0) ->
    V1 = string:lowercase(binary_to_list(V0)),
    V2 = lists:flatten(string:replace(V1, " ", "-", all)),
    list_to_binary(V2).
