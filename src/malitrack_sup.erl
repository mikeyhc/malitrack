%%%-------------------------------------------------------------------
%% @doc malitrack top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(malitrack_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, get_card_db/0]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

get_card_db() ->
    get_child_pid(card_db).

%%====================================================================
%% supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => card_db,
                    start => {card_db, start_link, []}}
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% helper methods
%%====================================================================

get_child_pid(ChildId) ->
    Children = supervisor:which_children(?SERVER),
    {_Id, Pid, _Type, _Modules} = lists:keyfind(ChildId, 1, Children),
    Pid.
