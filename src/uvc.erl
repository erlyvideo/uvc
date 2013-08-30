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
-module(uvc).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
-behaviour(gen_server).


-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-export([open/1, capture/1, stop/1]).

-record(uvc, {
  uvc,
  format,
  consumer,
  config
}).

start_link(Config) ->
  proc_lib:start_link(?MODULE, init, [Config]).


%%
%% Valid options:
%% device — Number of /dev/videoN
%% size = {Width, Height} — capture size
%% fps — FPS
%% format — yuv/jpeg. If stream is sent with jpeg, you may ask to decode it
%% consumer — pid of consumer
%%

capture(Options) ->
  application:start(uvc),
  uvc_sup:start_uvc(Options).


stop(Capture) ->
  Capture ! stop.

init(Config) ->
  Format = proplists:get_value(format, Config, yuv),
  Consumer = proplists:get_value(consumer, Config),
  erlang:monitor(process, Consumer),
  case open(Config) of
    {ok, UVC} ->
      proc_lib:init_ack({ok, self()}),
      gen_server:enter_loop(?MODULE, [], #uvc{format = Format, consumer = Consumer, config = Config, uvc = UVC});
    {error, Error} ->
      lager:info("Failed to start UVC: ~p", [Error]),
      proc_lib:init_ack({error, Error})
  end.


handle_call(_Request, _From, State) ->
  {stop, {unknown_call, _Request}, State}.

handle_cast(_Msg, State) ->
  {stop, {unknown_cast, _Msg}, State}.

handle_info({uvc, _UVC, Codec, PTS, RAW}, #uvc{format = Format, consumer = Consumer} = State) ->
  {Out, Codec1} = case Codec of
    jpeg when Format == yuv -> {Y, _Width, _Height} = jpeg:decode_yuv(RAW), {Y, yuv};
    _ -> {RAW, Codec}
  end,
  Consumer ! {uvc, self(), Codec1, PTS, Out},
  {noreply, State};

handle_info({'DOWN', _, process, _Client, _Reason}, State) ->
  {stop, normal, State};

handle_info(stop, State) ->
  {stop, normal, State};

handle_info(_Info, State) ->
  {stop, {unknown_msg, _Info}, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


-define(CMD_OPEN, 1).


open(Options) ->
  try open0(Options)
  catch
    throw:E -> E
  end.

open0(Options) ->
  Path = case code:lib_dir(uvc,priv) of
    {error, _} -> "priv";
    Else -> Else
  end,

  Device = proplists:get_value(device, Options, 0),
  {Width,Height} = proplists:get_value(size, Options, {640,360}),
  FPS = proplists:get_value(fps, Options, 20),
  lager:info("Opening capture device=~p size=~Bx~B, fps=~p", [Device, Width, Height, FPS]),

  case erl_ddll:load_driver(Path, uvc_drv) of
    ok -> ok;
    {error, already_loaded} -> ok;
    {error, LoadError} -> throw({error, {could_not_load_driver,erl_ddll:format_error(LoadError)}})
  end,
  
  UVC = open_port({spawn, uvc_drv}, [binary]),
  case port_control(UVC, ?CMD_OPEN, <<Device, Width:16/little, Height:16/little, FPS>>) of
    <<"ok">> -> ok;
    PortError -> throw({error,{uvc_init,PortError}})
  end,
  lager:info("Opened capture for ~p", [Device]),
  {ok, UVC}.


