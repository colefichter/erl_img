%%% File    : image_bmp.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : BMP Files
%%% Created :  5 Mar 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(image_bmp).

-include_lib("erl_img.hrl").
-include("api.hrl").
-include("dbg.hrl").

-import(lists, [reverse/1]).


-define(BMP_HEADER(FileSz, Offset),
        $B:8,$M:8,
        FileSz:32/little,
        0:16, 0:16,
        Offset:32/little).

-define(BMP_INFO(HSize,Width,Height,Planes,BitCount,Compression,
                 ImageSize,XRes,YRes,ColorsUsed,ImportantColors),
        HSize:32/little,
        Width:32/little,
        Height:32/little,
        Planes:16/little,
        BitCount:16/little,
        Compression:32/little,
        ImageSize:32/little,
        XRes:32/little,
        YRes:32/little,
        ColorsUsed:32/little,
        ImportantColors:32/little).

magic(<<$B,$M, _/binary>>) -> true;
magic(_) -> false.

mime_type() -> "image/bmp".

extensions() -> [".bmp" ].


read_info(Fd) ->
    case file:read(Fd, 54) of
        {ok, << ?BMP_HEADER(_Size,_Offset),
                ?BMP_INFO(_,Width,Height,_Planes,_BitCount,
                          _Compression,_,_,_,_,_) >> } ->
            {ok, #erl_image  { type      = ?MODULE,
                             width     = Width,
                             height    = Height%,
                             %depth     = Planes,
                             %format    = b8g8r8,
                             %bytes_pp  = 3,
                             %alignment = 4,
                             %order = left_to_right,
                             %attributes = [{'Compression',Compression}]
                            }};
        {ok, _} ->
            {error, bad_magic};
        Error ->
            Error
    end.


% write_info(_Fd, _IMG) ->
%     ok.

read(_Fd, IMG, _RowFun, _St0) ->
    %file:position(Fd, 54),
    % case read_pixels(Fd, IMG, RowFun, St0) of
    %     {ok,PIX} ->
    %         %{ok, IMG#erl_image { pixmaps = [PIX] }};
    %         {ok, IMG};
    %     Error -> Error
    % end.
    {ok, IMG}.

%% load image
read(Fd, IMG) ->
    read(Fd, IMG,
         fun(_, Row, Ri, St) ->
                 ?dbg("bmp: load row ~p\n", [Ri]),
                 [{Ri,Row}|St] end,
         []).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_dimensions_bmp_test() ->
    {ok, Bytes} = file:read_file("../test_images/cat.bmp"),
    {ok, Image} = erl_img:load(Bytes),
    ?assertEqual(500, Image#erl_image.width),
    ?assertEqual(330, Image#erl_image.height).


-endif.