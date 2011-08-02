%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2011 Max Lapshin
%%% @doc        Video 4 linux
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlyvideo is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlyvideo is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlyvideo.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(uvc4erl).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
-behaviour(gen_server).


-export([start_link/1, start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([subscribe/2]).
-export([unsubscribe/2]).

-export([open/1]).

-record(uvc4erl, {
  uvc,
  clients
}).

start_link(Name, Config) ->
  gen_server:start_link({local, Name}, ?MODULE, [Config], []).

start_link(Config) ->
  gen_server:start_link(?MODULE, [Config], []).

subscribe(UVC, Socket) ->
  gen_server:call(UVC, {subscribe, self(), Socket}).

unsubscribe(UVC, Socket) ->
  gen_server:call(UVC, {unsubscribe, self(), Socket}).

init([Config]) ->
  {ok, UVC} = open(Config),
  {ok, #uvc4erl{uvc = UVC, clients = []}}.

handle_call({subscribe, Pid, Port}, _From, #uvc4erl{clients = Clients} = State) ->
  case lists:keyfind(Port, 2, Clients) of
    {_, _} ->
      {reply, {error, already_subscribe}, State};
    false ->
      Ref = erlang:monitor(process, Pid),
      {reply, ok, State#uvc4erl{clients = [{Pid, Port}|Clients]}}
  end;

handle_call({unsubscribe, _Pid, Port}, _From, #uvc4erl{clients = Clients} = State) ->
  Clients1 = lists:keydelete(Port, 2, Clients),
  {reply, ok, State#uvc4erl{clients = Clients1}};

handle_call(_Request, _From, State) ->
  {stop, {unknown_call, _Request}, State}.

handle_cast(_Msg, State) ->
  {stop, {unknown_cast, _Msg}, State}.

handle_info({inet_reply,_Port,_Reply}, State) ->
  {noreply, State};

handle_info({uvc_ready, UVC}, #uvc4erl{uvc = UVC} = State) ->
  {noreply, State};

handle_info({uvc, UVC, Bin}, #uvc4erl{uvc = UVC, clients = Clients} = State) ->
  lists:foreach(fun
    ({_Pid, Port}) when is_port(Port) -> (catch port_command(Port, Bin, [nosuspend]));
    ({_Pid, Pid}) when is_pid(Pid) -> Pid ! {uvc, self(), Bin}
  end, Clients),
  {noreply, State};
  
handle_info(_Info, State) ->
  ?D({unknown_msg, _Info}),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


-define(CMD_OPEN, 1).
-define(CMD_GET_PID, 2).
-define(CMD_START_INPUT, 3).


open(Options) ->
  ?D({load, Options}),
  Path = case code:lib_dir(uvc4erl,priv) of
    {error, _} -> "priv";
    Else -> Else
  end,
  case erl_ddll:load_driver(Path, uvc4erl_drv) of
  	ok -> ok;
  	{error, already_loaded} -> ok;
  	{error, Error} -> exit({error, {could_not_load_driver,erl_ddll:format_error(Error)}})
  end,
  ?D({open, Options}),
  UVC = open_port({spawn, uvc4erl_drv}, [binary]),
  ?D({configure, Options}),
  
  Device = proplists:get_value(device, Options, 0),
  {Width,Height} = proplists:get_value(size, Options, {640,360}),
  FPS = proplists:get_value(fps, Options, 20),
  <<"ok">> = port_control(UVC, ?CMD_OPEN, <<Device, Width:16/little, Height:16/little, FPS>>),
  io:format("Opening capture ~p~n", [UVC]),
  {ok, UVC}.


