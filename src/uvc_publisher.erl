-module(uvc_publisher).

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("rtmp/include/rtmp.hrl").

-export([publish/2]).

publish(URL, Options) ->
  {ok, UVC} = uvc4erl:open(Options),
  {W,H} = proplists:get_value(size, Options),
  put(width, W),
  put(height, H),
  {ok, RTMP} = rtmp_socket:connect(URL),
  receive
    {rtmp, RTMP, connected} ->
      rtmp_socket:setopts(RTMP, [{active, true}]),
      rtmp_lib:connect(RTMP, [{app, <<"live">>}, {tcUrl, <<"rtmp://localhost/live/a">>}]),
      Stream = rtmp_lib:createStream(RTMP),
      put(stream, Stream),
      {rtmp, _UserInfo, _Host, _Port, [$/ | Path], _Query} = http_uri2:parse(URL),
      rtmp_lib:publish(RTMP, Stream, Path),
      put(rtmp, RTMP)
  after
    1000 ->
      erlang:exit(rtmp_timeout)
  end,
  io:format("Proceeding to loop~n"),
  % timer:send_interval(2000, stop),
  put(counter, 0),
  loop(),
  ok.
  
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

send_frame(RTMP, Frame) ->
  Message = rtmp_message(Frame, get(stream)),
	rtmp_socket:send(RTMP, Message).

loop() ->
  receive
    stop -> ok;
    {uvc4erl, UVC, Codec, PTS1, Jpeg} ->
      PTS = case get(base_vpts) of
        undefined -> put(base_vpts, PTS1), 0;
        BaseVPTS -> PTS1 - BaseVPTS
      end,
      Drop = drop(),
      Name = lists:flatten(io_lib:format("frame-~4..0B.jpg", [get(counter)])),
      T1 = erlang:now(),
      {YUV, Width, Height} = case Codec of
        jpeg -> jpeg:decode(Jpeg);
        yuv -> {Jpeg, get(width), get(height)}
      end,
      T2 = erlang:now(),
      X264 = case get(encoder) of
        undefined ->
          {ok, X, Config} = ems_video:init_x264([{width,Width},{height,Height},{config,"h264/priv/encoder.preset"},{annexb,false}]),
          send_frame(get(rtmp), #video_frame{
            content = video,
            flavor = config,
            codec = h264,
            pts = 0, 
            dts = 0,
            body = Config
          }),
          io:format("cfg: ~p~n", [Config]),
          % {ok, F} = file:open("a.264", [write,binary]),
          % put(file, F),
          % file:write(F, [NALS]),
          put(encoder, X),
          X;
        X -> X
      end,
      case ems_video:yuv_x264(X264, YUV, PTS) of
        ok -> ok;
        {ok, Flavor, PTSEnc, DTSEnc, H264} -> 
          send_frame(get(rtmp), #video_frame{
            content = video,
            flavor = Flavor,
            codec = h264,
            pts = PTSEnc,
            dts = DTSEnc,
            body = H264
          })
        
        % file:write(get(file), H264)
      end,
      T3 = erlang:now(),
      io:format("~p ~s: ~px~p, (~p, ~p), ~p~n", [Codec, Name, Width, Height, timer:now_diff(T2,T1) div 1000, timer:now_diff(T3,T2) div 1000, Drop]),
      % file:write_file(Name, Jpeg),
      put(counter, get(counter) + 1),
      loop();
    {rtmp, _, _} = Msg ->
      io:format("msg: ~p~n", [Msg]),
      loop();
    Else ->
      io:format("undefined: ~p~n", [Else])  
  end.

  