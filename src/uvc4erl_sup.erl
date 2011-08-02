
-module(uvc4erl_sup).

-behaviour(supervisor).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([start_link/0, start_uvc4erl/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    uvc4erl_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_uvc4erl(Name, Config) ->
  DVBS2 = {
    {Name, sup},
    {uvc4erl, start_link ,[Name, Config]},
    permanent,
    10000,
    worker,
    [uvc4erl]
  },
  supervisor:start_child(?MODULE, DVBS2).



%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

