%%% File    : image_gif.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : GIF image processing
%%% Created :  5 Mar 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(image_gif).

-include("erl_img.hrl").

-include("api.hrl").

-include("dbg.hrl").

-import(lists, [reverse/1]).


-define(APP_EXTENSION, 16#ff).
-define(COM_EXTENSION, 16#fe).
-define(CTL_EXTENSION, 16#f9).
-define(TXT_EXTENSION, 16#01).

-define(EXTENSION, 16#21).   %% $!
-define(IMAGE,   16#2c).     %% $,
-define(TRAILER, 16#3b).     %% $;

%% Read magic info check MAGIC type and width and height (depth)
%% of image
-define(MAGIC87, $G,$I,$F,$8,$7,$a).
-define(MAGIC89, $G,$I,$F,$8,$9,$a).

magic(<<?MAGIC87,_/binary>>) -> true;
magic(<<?MAGIC89,_/binary>>) -> true;
magic(_) -> false.

mime_type() -> "image/gif".

extensions() -> [ ".gif" ].


read_info(Fd) ->
    case file:read(Fd, 10) of
        {ok, <<?MAGIC87,
              Width:16/little-unsigned-integer,
              Height:16/little-unsigned-integer,_/binary>>} ->
            {ok,#erl_image { type = ?MODULE,
                           width = Width,
                           height = Height%,
                           %format = palette8,
                           %order = left_to_right,
                           %depth = 8 
                           }};
        {ok, <<?MAGIC89,
              Width:16/little-unsigned-integer,
              Height:16/little-unsigned-integer,_/binary>>} ->
            {ok,#erl_image { type = ?MODULE,
                           width = Width,
                           height = Height%,
                           %format = palette8,
                           %order = left_to_right,
                           %depth  = 8 
                           }};
        {ok, _} ->
            {error, bad_magic};
        Error ->
            Error
    end.


read(Fd,IMG,RowFun,St0) ->
    file:position(Fd, 6),
    case file:read(Fd, 7) of
        {ok, <<_Width:16/little, _Hight:16/little,
              _Map:1, _Cr:3, _Sort:1, _Pix:3,
              _Background:8,
              _AspectRatio:8>>} ->
            %Palette = read_palette(Fd, Map, Pix+1),
            %?dbg("sizeof(palette)=~p Map=~w, Cr=~w, Sort=~w, Pix=~w\n",
            %     [length(Palette),Map,_Cr,Sort,Pix]),
            %?dbg("Background=~w, AspectRatio=~w\n",
            %     [Background, AspectRatio]),
            % As = [{'Background',Background},
            %       {'AspectRatio',AspectRatio},
            %       {'Sort',Sort} | IMG#erl_image.attributes],
            %IMG1 = IMG#erl_image { attributes = As},
            read_data(Fd, IMG, RowFun, St0, []);
        Error ->
            Error
    end.


read(Fd, IMG) ->
    read(Fd, IMG,
         fun(_, Row, Ri, St) ->
                 ?dbg("gif: load row ~p\n", [Ri]),
                 [{Ri,Row}|St] end,
         []).


read_data(_Fd, IMG, _RowFun, _St0, _As) ->    
    {ok, IMG}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_dimensions_gif_test() ->
    {ok, Bytes} = file:read_file("../test_images/hairycat.gif"),
    {ok, Image} = erl_img:load(Bytes),
    ?assertEqual(350, Image#erl_image.width),
    ?assertEqual(263, Image#erl_image.height).


-endif.