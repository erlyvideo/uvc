
-module(uvc_sup).

-behaviour(supervisor).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([start_link/0, start_uvc/2, start_uvc/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    uvc_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_uvc(Name, Config) ->
  DVBS2 = {
    {Name, sup},
    {uvc, start_link ,[Name, Config]},
    permanent,
    10000,
    worker,
    [uvc]
  },
  supervisor:start_child(?MODULE, DVBS2).

start_uvc(Config) ->
  supervisor:start_child(uvc_capture_sup, [Config]).
  


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([uvc]) ->
  {ok, {{simple_one_for_one, 50, 100}, [
    {   undefined,                               % Id       = internal id
    {uvc,start_link,[]},                  % StartFun = {M, F, A}
    temporary,                               % Restart  = permanent | transient | temporary
    2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
    worker,                                  % Type     = worker | supervisor
    []                                       % Modules  = [Module] | dynamic
    }
  ]}};
  

init([]) ->
  Supervisors = [
  {   uvc_capture_sup,
      {supervisor,start_link,[{local, uvc_capture_sup}, ?MODULE, [uvc]]},
      permanent,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
  }
  ],
  {ok, { {one_for_one, 5, 10}, Supervisors} }.

