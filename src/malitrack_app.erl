%%%-------------------------------------------------------------------
%% @doc malitrack public API
%% @end
%%%-------------------------------------------------------------------

-module(malitrack_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Layout = mst:compile(layout),
    {ok, CardDirEnv} = application:get_env(malitrack, card_dir),
    CardDir = filename:join([code:priv_dir(malitrack), CardDirEnv]),
    Routes = [{'_', [{"/", basic_redirect, #{path => <<"/crews">>,
                                             code => 302}},
                     {"/card/:name", card_handler,
                      #{template => wrap_template(card, Layout)}},
                     {"/crews", crews_handler, []},
                     {"/static/[...]", cowboy_static,
                      {priv_dir, malitrack, "static"}}]}],
    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_clear(malitrack_listener,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch}}),
    malitrack_sup:start_link(CardDir).

stop(_State) ->
    ok.

%% internal functions

wrap_template(TemplateId, Layout) ->
    Template = mst:compile(TemplateId),
    fun(Data) -> mst:render(Layout, Data, #{body => Template}) end.
