-module(uvc_publisher).

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("rtmp/include/rtmp.hrl").

-define(D(X), io:format("~p:~p ~p~n", [?MODULE, ?LINE, X])).

-export([publish/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(publisher, {
  url,
  options,
  uvc,
  x264,
  audio,
  faac,
  width,
  height,
  rtmp,
  base_vpts,
  start,
  stream
}).

publish(URL, Options) ->
  gen_server:start_link(?MODULE, [URL, Options], []).




init([URL, Options]) ->
  {ok, UVC} = uvc4erl:open(Options),
  
  {W,H} = proplists:get_value(size, Options),
  {ok, RTMP} = rtmp_socket:connect(URL),
  Stream = receive
    {rtmp, RTMP, connected} ->
      rtmp_socket:setopts(RTMP, [{active, true}]),
      rtmp_lib:connect(RTMP, [{app, <<"live">>}, {tcUrl, <<"rtmp://localhost/live/a">>}]),
      Stream1 = rtmp_lib:createStream(RTMP),
      {rtmp, _UserInfo, _Host, _Port, [$/ | Path], _Query} = http_uri2:parse(URL),
      rtmp_lib:publish(RTMP, Stream1, Path),
      Stream1
  after
    1000 ->
      erlang:exit(rtmp_timeout)
  end,
  
  SampleRate = 32000,
  Channels = 2,
  {ok, Capture} = audiocapture:start(-1, SampleRate, Channels),
  {ok, AACEnc, AAConfig} = ems_sound2:init_faac(<<SampleRate:32/little, Channels:32/little>>),
  
  send_frame(RTMP, Stream, #video_frame{
    content = audio,
    flavor = config,
    codec = aac,
    sound = {stereo, bit16, rate44},
    pts = 0, 
    dts = 0,
    body = AAConfig
  }),
  
  {ok, X264, VConfig} = ems_video:init_x264([{width,W},{height,H},{config,"h264/priv/encoder.preset"},{annexb,false}]),
  send_frame(RTMP, Stream, #video_frame{
    content = video,
    flavor = config,
    codec = h264,
    pts = 0, 
    dts = 0,
    body = VConfig
  }),
  
  
  {ok, #publisher{
    url = URL,
    options = Options,
    uvc = UVC,
    audio = Capture,
    faac = AACEnc,
    width = W,
    height = H,
    rtmp = RTMP,
    x264 = X264,
    stream = Stream,
    start = erlang:now()
  }}.
  
drop() ->
  drop(0).

drop(Count) ->
  receive
    {uvc4erl, _UVC, _Codec, _PTS, _Jpeg} -> drop(Count + 1)
  after
    0 -> Count
  end.


channel_id(#video_frame{content = metadata}) -> 4;
channel_id(#video_frame{content = audio}) -> 5;
channel_id(#video_frame{content = video}) -> 6.


rtmp_message(#video_frame{dts = DTS, content = Type} = Frame, StreamId) ->
  #rtmp_message{
    channel_id = channel_id(Frame), 
    timestamp = DTS,
    type = Type,
    stream_id = StreamId,
    body = flv_video_frame:encode(Frame)}.

send_frame(RTMP, Stream, Frame) ->
  Message = rtmp_message(Frame, Stream),
	rtmp_socket:send(RTMP, Message).

handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.

handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.

% handle_info({uvc4erl, UVC, Codec, PTS1, RAW}, #publisher{base_vpts = undefined} = State) ->
%   handle_info({uvc4erl, UVC, Codec, 0, RAW}, State#publisher{base_vpts = PTS1});

handle_info({uvc4erl, UVC, Codec, _PTS1, Jpeg}, #publisher{base_vpts = BaseVPTS, width = Width, height = Height, x264 = X264} = State) ->
  % PTS = PTS1 - BaseVPTS,
  Drop = drop(),

  T1 = erlang:now(),
  PTS = timer:now_diff(T1, State#publisher.start) div 1000,
  YUV = case Codec of
    jpeg -> {Y, Width, Height} = jpeg:decode(Jpeg), Y;
    yuv -> Jpeg
  end,
  T2 = erlang:now(),
  case ems_video:yuv_x264(X264, YUV, PTS) of
    ok -> ok;
    {ok, Flavor, PTSEnc, DTSEnc, H264} ->
      T3 = erlang:now(),
      io:format("~4s ~5B ~px~p, (~p, ~p), ~p~n", [h264, DTSEnc, Width, Height, timer:now_diff(T2,T1) div 1000, timer:now_diff(T3,T2) div 1000, Drop]),
    
      send_frame(State#publisher.rtmp, State#publisher.stream, #video_frame{
        content = video,
        flavor = Flavor,
        codec = h264,
        pts = PTSEnc,
        dts = DTSEnc,
        body = H264
      })
  
    % file:write(get(file), H264)
  end,
  
  {noreply, State};


handle_info({audiocapture, Capture, DTS, PCM}, #publisher{faac = AACEnc} = State) ->
  T1 = erlang:now(),
  case ems_sound2:pcm_aac(AACEnc, PCM) of
    undefined -> ok;
    AAC -> 
      T2 = erlang:now(),
      io:format("~4s ~5B (~p)~n", [aac, DTS, timer:now_diff(T2,T1) div 1000]),
      send_frame(State#publisher.rtmp, State#publisher.stream, #video_frame{
        content = audio,
        flavor = frame,
        codec = aac,
        sound = {stereo, bit16, rate44},
        pts = DTS,
        dts = DTS,
        body = AAC
      })
  end,
  {noreply, State};

handle_info({rtmp, _, _} = Msg, State) ->
  io:format("rtmp: ~p~n", [Msg]),
  {noreply, State};

handle_info(Else, State) ->
  {stop, {undefined_message,Else}, State}.

terminate(_Reason, _State) -> ok.

