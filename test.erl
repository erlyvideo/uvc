#!/usr/bin/env escript

main([]) ->
  code:add_pathz("ebin"),
  code:add_pathz("../jpeg/ebin"),
  Device = 0,
  Size = {1280,720},
  FPS = 20,
  Debug = true,
  {ok, UVC} = uvc:open([{device,Device},{debug,Debug},{size,Size},{fps,FPS}]),
  % timer:send_after(2000, stop),
  loop().


loop() ->
  receive
    stop -> ok;
    {uvc, _UVC, Codec, PTS, Jpeg} ->
      % io:format("~p ~p ~p~n", [Codec, PTS, size(Jpeg)]),
      {Raw, W, H} = jpeg:decode_yuv(Jpeg),
      io:format("~p ~p ~p(~px~p)~n", [Codec, PTS, size(Raw), W, H]),
      loop()
  end.
