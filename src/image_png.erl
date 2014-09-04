%%% File    : image_png.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : PNG Files
%%% Created :  5 Mar 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(image_png).

-include_lib("erl_img.hrl").
-include("api.hrl").

-include("dbg.hrl").

-import(lists, [reverse/1]).
-import(erl_img, [attribute/3, set_attribute/3]).

-define(MAGIC, 137,$P,$N,$G,$\r,$\n,26,$\n).

-define(IHDR, "IHDR"). %% image header
-define(PLTE, "PLTE"). %% palette
-define(IDAT, "IDAT"). %% image data
-define(IEND, "IEND"). %% image trailer

-define(bKGD, "bKGD"). %% background color
-define(cHRM, "cHRM"). %% primary chromaticites and white point
-define(gAMA, "gAMA"). %% Image gamma
-define(hIST, "hIST"). %% Image histogram
-define(pHYs, "pHYs"). %% Physical pixel dimensions
-define(sBIT, "sBIT"). %% Significant bits
-define(tEXt, "tEXt"). %% Textual data
-define(tIME, "tIME"). %% Image last modification time
-define(tRNS, "tRNS"). %% Transparency
-define(zTXt, "zTXt"). %% Compressed textual data

magic(<<?MAGIC, _/binary>>) -> true;
magic(_) -> false.

mime_type() -> "image/png".

extensions() -> [ ".png" ].

read_info(Fd) ->
    case file:read(Fd, 8) of
        {ok, << ?MAGIC >> } ->
            scan_info(Fd, #erl_image { type = ?MODULE }, true);
        {ok, _} ->
            {error, bad_magic};
        Error ->
            Error
    end.

scan_info(Fd, IMG, First) ->
    case read_chunk_hdr(Fd) of
        {ok, Length, Type} ->
            Z = zlib:open(),
            Res = scan_info(Fd, IMG, First, Type, Length, Z),
            zlib:close(Z),
			Res;
        Error ->
            Error
    end.

scan_info(Fd, IMG, true, ?IHDR, Length, Z) ->
    case read_chunk_crc(Fd, Length, Z) of
        {ok,  <<Width:32, Height:32, _BitDepth:8,
               ColorType:8, CompressionMethod:8,
               FilterMethod:8, InterlaceMethod:8, _/binary >>} ->
               %Format = format(ColorType,BitDepth),
            scan_info(Fd, IMG#erl_image {
                            width = Width,
                            height = Height,
                            %depth = BitDepth,
                            %format = Format,
                            %bytes_pp = bpp(Format),
                            %order  = left_to_right,
                            attributes =
                            [ {'ColorType', ColorType},
                              {'Compression', CompressionMethod},
                              {'Filter', FilterMethod },
                              {'Interlace', InterlaceMethod }]}, false);
        Error -> Error
    end;
scan_info(Fd, IMG, false, ?tEXt, Length, Z) ->
    case read_chunk_crc(Fd, Length, Z) of
        {ok, Bin} ->
            scan_info(Fd, update_txt(IMG, Bin), false);
        Error -> Error
    end;
scan_info(Fd, IMG, false, ?zTXt, Length, Z) ->
    case read_chunk_crc(Fd, Length, Z) of
        {ok, Bin} ->
            [Key, CompressedValue] = binary:split(Bin, <<0, 0>>),
            Value = zlib:uncompress(CompressedValue),
            scan_info(Fd, set_attribute(IMG, list_to_atom(binary_to_list(Key)), binary_to_list(Value)), false);
        Error -> Error
    end;
scan_info(Fd, IMG, false, ?bKGD, Length, Z) ->
    CT = attribute(IMG, 'ColorType', undefined),
    case read_chunk_crc(Fd, Length, Z) of
        {ok, <<Index:8>>} when CT==3 ->
            scan_info(Fd, set_attribute(IMG, 'Background', Index), false);
        {ok, <<Gray:16>>} when CT==0; CT==4 ->
            scan_info(Fd, set_attribute(IMG, 'Background', Gray), false);
        {ok, <<R:16,G:16,B:16>>} when CT==2; CT==6 ->
            scan_info(Fd, set_attribute(IMG, 'Background', {R,G,B}), false);
        {ok, _Data} ->
            ?dbg("bKGD other=~p\n", [_Data]),
            scan_info(Fd, IMG, false);
        Error -> Error
    end;
scan_info(Fd, IMG, false, ?tIME, Length, Z) ->
    case read_chunk_crc(Fd, Length, Z) of
        {ok, <<Year:16, Mon:8, Day:8, H:8, M:8, S:8>>} ->
            scan_info(Fd, IMG#erl_image { mtime = {{Year,Mon,Day},
                                                   {H,M,S}} }, false);
        {ok, _Data} ->
            ?dbg("tIME other=~p\n", [_Data]),
            scan_info(Fd, IMG, false);
        Error -> Error
    end;
scan_info(Fd, IMG, false, ?pHYs, Length, Z) ->
    case read_chunk_crc(Fd, Length, Z) of
        {ok, <<X:32, Y:32, _Unit:8>>} ->
            scan_info(Fd, set_attribute(IMG,'Physical',{X,Y,meter}),false);
        {ok, _Data} ->
            ?dbg("pHYs other=~p\n", [_Data]),
            scan_info(Fd, IMG, false);
        Error -> Error
    end;
scan_info(Fd, IMG, false, ?tRNS, Length, Z) ->
    CT = attribute(IMG, 'ColorType', undefined),
    case read_chunk_crc(Fd, Length, Z) of
        {ok, <<Gray:16>>} when CT == 0 ->
            scan_info(Fd, set_attribute(IMG, 'Transparent', Gray), false);
        {ok, <<R:16, B:16, G:16>>} when CT == 2 ->
            scan_info(Fd, set_attribute(IMG, 'Transparent', {R,G,B}), false);
        {ok, _Binary} when CT == 3 ->
            %scan_info(Fd, IMG#erl_image { alpha_table = binary_to_list(Binary) }, false);
            scan_info(Fd, IMG, false);
        Error -> Error
    end;
scan_info(_Fd, IMG, false, ?IEND, 0, _Z) ->
    {ok, IMG};
scan_info(Fd, IMG, false, _Type, Length, _Z) ->
    ?dbg("~s skipped=~p\n", [_Type,Length]),
    skip_chunk(Fd, Length),
    scan_info(Fd, IMG, false).

%% Update txt attributes
update_txt(IMG, Txt) ->
    case txt(binary_to_list(Txt), []) of
        {value,{Key,Value}} ->
            case Key of
                'Comment' ->
                    IMG#erl_image { comment = Value };
                _ ->
                    As = [{Key,Value} | IMG#erl_image.attributes],
                    IMG#erl_image { attributes = As }
            end;
        false ->
            IMG
    end.




%% process text chunk
txt([0|Value], RKey) ->
    {value, {list_to_atom(reverse(RKey)), Value}};
txt([C|Cs], RKey) ->
    txt(Cs,[C|RKey]);
txt([], _) ->
    false.




read(Fd, IMG) ->
    read(Fd, IMG,
         fun(_, Row, Ri, St) ->
                 ?dbg("png: load row ~p\n", [Ri]),
                 [{Ri,Row}|St] end,
         []).


read(_Fd, IMG, _RowFun, _St0) ->
    % file:position(Fd, 8), %% skip magic
    % Z = zlib:open(),
    % zlib:inflateInit(Z),
    % Resp = read_image(Fd, [], undefined, Z),
    % zlib:close(Z),
    % case Resp of
    %     {ok, Binary, Palette} ->
    %         {ok,Pixmap} = decode_pixmap(IMG, Binary, Palette, RowFun, St0),
    %         {ok, IMG#erl_image { pixmaps = [Pixmap],
    %                              palette = Palette }};
    %     Error -> Error
    % end.
    {ok, IMG}.

%%
%% Given chunk header read chunk and check crc
%%
read_chunk_crc(Fd, Length, Z) ->
    file:position(Fd, {cur,-4}),
    LengthWithType = Length+4,
    case file:read(Fd, LengthWithType+4) of
        {ok,<<TypeChunk:LengthWithType/binary, CRC:32>>} ->
            case valid_crc32(TypeChunk, CRC, Z) of
                true ->
                    <<_:32, Chunk/binary>> = TypeChunk,
                    {ok, Chunk};
                false ->
                    {error, bad_crc}
            end;
        {ok,_} ->
            {error, bad_chunk};
        Error ->
            Error
    end.

%%
%% Read the chunk header
%%

read_chunk_hdr(Fd) ->
    case file:read(Fd, 8) of
        {ok, <<Length:32, Type:4/binary>>} ->
            Tag = binary_to_list(Type),
            ?dbg("chunk: type = ~p, length=~p\n", [Tag,Length]),
            {ok, Length, Tag};
        Error ->
            Error
    end.


skip_chunk(Fd, Length) ->
    file:position(Fd, {cur,Length+4}).

valid_crc32(Binary, Value, Z) ->
    CRC32 = zlib:crc32(Z, Binary),
    ?dbg("crc check: ~p == ~p\n", [CRC32, Value]),
    CRC32 == Value.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%%
%% These tests use the pngsuite of test pngs from the libpng
%% website to verify all sorts of things.  The intentionally corrupt
%% pngs live in priv/pngsuite/corrupted because I didn't want
%% to deal with them right now, see should probably write
%% some more acceptance tests that ensures those fail for
%% specific reasons.
%%
png_suite_files() ->
    application:start(erl_img),
    PrivDir = code:priv_dir(erl_img),
    %% Skip tests for unsupported 1-bit, 2-bit, 4-bit color
    {ok, SkipRegex} = re:compile("0[124]\\.png"),
    lists:filter(
      fun (Fn) -> nomatch =:= re:run(Fn, SkipRegex) end,
      filelib:wildcard(PrivDir ++ "/pngsuite/*.png")).

%% Use a macro here so png_suite_*_test_ shows up in the eunit output.
-define(PNG_SUITE_TEST(TestFun),
        [{filename:basename(FName),
          fun() ->
                  TestFun(FName)
          end}
         || FName <- png_suite_files()]).

png_suite_read_test_() ->
    ?PNG_SUITE_TEST(
      fun(FileName) ->
              {ok, Fd} = file:open(FileName, [read, binary]),
              {ok, Info} = read_info(Fd),
              {ok, 0} = file:position(Fd, 0),
              ?assertMatch({ok, _}, read(Fd, Info))
      end).

png_suite_magic_test_() ->
    ?PNG_SUITE_TEST(
      fun(FileName) ->
              {ok, Bin} = file:read_file(FileName),
              ?assertEqual(true, magic(Bin))
      end).

non_png_magic_test() ->
    ?assertEqual(false, magic(<<"notapng">>)).

mime_type_test() ->
    ?assertEqual("image/png", mime_type()).

extensions_test() ->
    ?assertEqual([".png"], extensions()).



get_dimensions_png_test() ->
    {ok, Bytes} = file:read_file("../test_images/team.png"),
    {ok, Image} = erl_img:load(Bytes),
    ?assertEqual(234, Image#erl_image.width),
    ?assertEqual(132, Image#erl_image.height).


get_dimensions_png_2_test() ->
    {ok, Bytes} = file:read_file("../test_images/afsc.png"),
    {ok, Image} = erl_img:load(Bytes),
    ?assertEqual(727, Image#erl_image.width),
    ?assertEqual(409, Image#erl_image.height).

-endif.
