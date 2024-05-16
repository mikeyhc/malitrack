%%%-------------------------------------------------------------------
%% @doc malitrack top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(malitrack_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, get_unit_db/0, get_box_db/0]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

get_unit_db() ->
    get_child_pid(unit_db).

get_box_db() ->
    get_child_pid(box_db).

%%====================================================================
%% supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => box_db,
                    start => {box_db, start_link, []}},
                  #{id => unit_db,
                    start => {unit_db, start_link, []}}
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% helper methods
%%====================================================================

get_child_pid(ChildId) ->
    Children = supervisor:which_children(?SERVER),
    {_Id, Pid, _Type, _Modules} = lists:keyfind(ChildId, 1, Children),
    Pid.
