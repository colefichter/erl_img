%%% File    : erl_image_jpg.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : JPG image processing (Exif/JPG files)
%%% Created :  5 Mar 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(image_jpeg).

-export([magic/1, mime_type/0, extensions/0, read/2, read_info/1]).
-compile(export_all).
-import(lists, [reverse/1, map/2]).

-include("erl_img.hrl").
-include("jpeg.hrl").
-include("tiff.hrl").
-include("exif.hrl").

-include("dbg.hrl").


%% JPeg bit stream
-record(jfd,
	{
	  fd,     %% open file
	  bits,   %% bits buffer
	  bytes   %% bytes buffer
	 }).


%% YCbCr => RGB (JPG)
-define(R(Y,Cb,Cr), ((Y) + (1.402)*((Cr)-128))).
-define(G(Y,Cb,Cr), ((Y) - 0.34414*((Cb)-128) - 0.71414*((Cr)-128))).
-define(B(Y,Cb,Cr), ((Y) + 1.772*((Cb)-128))).

%% RGB => YCbCr
-define(Y(R,G,B), (0.299*(R) + 0.587*(G) + 0.114*(B))).
-define(Cb(R,G,B), (-0.1687*(R) - 0.3313*(G) + 0.5*(B) + 128)).
-define(Cr(R,G,B), (0.5*R - 0.4187*(G) - 0.0813*(B) + 128)).

magic(<<?M_MARK,?M_SOI,?M_MARK,?M_APP1,_Len:16,"Exif",0,0,_/binary>>) ->
    true;
magic(<<?M_MARK,?M_SOI,?M_MARK,?M_APP0,_Len:16,"JFIF",_,_,_/binary>>) -> 
    true;
magic(_) -> 
    false.

mime_type() -> "image/jpeg".

extensions() -> [".jpeg", ".jpg"].

read_info(Fd) ->
    JFd = jfd(Fd),
    case jfd_read_bytes(JFd, 2) of
	{JFd1, <<?M_MARK,?M_SOI>>} ->
	    read_segments(JFd1, 
			  #erl_image { type = ?MODULE%,
				     %order = left_to_right
				    });
	{ok,_} ->
	    {error, bad_magic};
	Error -> Error
    end.

read(_Fd,IMG) ->
    {ok,IMG}.

read(_Fd,IMG,_RowFun,_St0) ->
    {ok,IMG}.

read_segments(JFd0,Ei0) ->
    case jfd_skip(JFd0) of
	{JFd1,0} ->
	    read_segments(JFd1,Ei0);
	{_JFd1,?M_EOI} ->
	    ?dbg("EOI\n",[]),
	    {ok,Ei0};
	% {JFd1,?M_COM}->
	%     ?dbg("COM\n",[]),
	%     segment(JFd1,Ei0,
	% 	    fun(Bin,Ei) ->
	% 		    Ei#erl_image {comment=binary_to_list(Bin)}
	% 	    end);
	% {JFd1,?M_APP0} ->
	%     ?dbg("APP0\n",[]),
	%     segment(JFd1,Ei0,
	% 	    fun(<<"JFIF",0,Bin/binary>>,Ei) ->
	% 		    process_jfif(Bin,Ei);
	% 	       (_,Ei) ->
	% 		    Ei
	% 	    end);
	% {JFd1,?M_APP1} ->
	%     ?dbg("APP1\n",[]),
	%     segment(JFd1,Ei0,
	% 	    fun(<<"Exif",0,0,Bin/binary>>,Ei) ->
	% 		    process_exif(Bin,Ei);
	% 	       (_,Ei) ->
	% 		    Ei
	% 	    end);
	{JFd1,Marker=?M_SOF0} ->
	    ?dbg("SOF0\n",[]),
	    segment(JFd1,Ei0,
		    fun(Bin,Ei) ->
			    process_sofn(Marker, Bin,Ei)
		    end);
	{JFd1,Marker=?M_SOF1} ->
	    ?dbg("SOF1\n",[]),
	    segment(JFd1,Ei0,
		    fun(Bin,Ei) ->
			    process_sofn(Marker, Bin,Ei)
		    end);
	% {JFd1,?M_DHT} -> 
	%     ?dbg("DHT\n", []),
	%     segment(JFd1,Ei0,
	% 	    fun(Bin,Ei) ->
	% 		    decode_dht(Bin,Ei)
	% 	    end);

	% {JFd1,?M_DQT} ->
	%     ?dbg("DQT\n", []),
	%     segment(JFd1,Ei0,
	% 	    fun(Bin,Ei) ->
	% 		    decode_dqt(Bin, Ei)
	% 	    end);
	% {JFd1,?M_DRI} ->
	%     ?dbg("DRI\n", []),
	%     segment(JFd1,Ei0,
	% 	    fun(<<Interval:16,_/binary>>,Ei) ->
	% 		    ?dbg("DRI Interval=~w\n", [Interval]),
	% 		    erl_img:set_attribute(Ei, dri,Interval)
	% 	    end);
	{_JFd1, eof} -> 
	    % io:format("Warning: EOF found before EOI\n"),
	    {ok,Ei0};

	% {JFd1,?M_SOS} -> 
	%     ?dbg("SOS\n",[]),
	%     case jfd_read_bytes(JFd1,2) of
	% 	{JFd2,<<Len:16>>} ->
	% 	    case jfd_read_bytes(JFd2, Len-2) of
	% 		{_JFd3, eof} ->
	% 		    % io:format("Warning: EOF before EOI\n"),
	% 		    {ok,Ei0};
	% 		{_JFd3,<<Bin/binary>>} when size(Bin) == Len-2 ->
	% 		    % SOS = init_sos(Bin, Ei0),
	% 		    % {JFd4,Ei1} = read_sos(JFd3, SOS, Ei0),
	% 		    % read_segments(JFd4, Ei1);
	% 		    {ok,Ei0};
	% 		{_JFd3,_} ->
	% 		    % io:format("Warning: file truncated\n"),
	% 		    {ok,Ei0}
	% 	    end;
	% 	{_JFd2,eof} ->
	% 	    % io:format("Warning: EOF before EOI\n"),
	% 	    {ok,Ei0}
	%     end;

	{JFd1,_Marker} ->
	    ?dbg("Marker=~2.16.0B - skipped\n", [_Marker]),
	    segment(JFd1,Ei0, fun(_Bin,Ei) -> Ei end)
    end.

segment(JFd,Ei,Fun) ->
    case jfd_read_bytes(JFd,2) of
	{JFd1,<<Len:16>>} ->
	    case jfd_read_bytes(JFd1, Len-2) of
		{_JFd2, eof} ->
		    % io:format("Warning: EOF before EOI\n"),
		    {ok,Ei};
		{JFd2,Bin} when size(Bin) == Len-2 ->
		    Ei1 = Fun(Bin, Ei),
		    read_segments(JFd2, Ei1);
		{JFd1,_} ->
		    % io:format("Warning: file truncated\n"),
		    {ok,Ei}
	    end;
	{_JFd1,eof} ->
	    % io:format("Warning: EOF before EOI\n"),
	    {ok,Ei}
    end.


%% calculate sampling with and height
% component_vh(Comps, IMG) ->
%     component_vh(Comps, IMG, 0, 0).

% component_vh([{Format,_DC,_AC}|Cs], IMG, H, V) ->
%     ?dbg("component_vh: ~p\n", [{component,Format}]),
%     {_Q,H0,V0} = erl_img:attribute(IMG, {component,Format}, undefined),
%     component_vh(Cs, IMG, max(H,H0), max(V,V0));
% component_vh([], _IMG, H, V) ->
%     {H,V}.
%%
-record(comp,
	{
	  format,  %% y | cr | cb | i | q
	  n,       %% h*v
	  h,       %% horizontal
	  v,       %% vertical
	  dcd,     %% DCD huffman codec
	  acd,     %% ACD huffman codec
	  qt       %% Quantization table
	 }).

-define(align(X,N), (((X+(N-1)) div (N))*(N))).



jfd(Fd) ->
    #jfd { fd=Fd, bits=(<<>>), bytes=(<<>>) }.

jfd_skip(JFd) ->
    case jfd_read_bytes(JFd, 1) of
	{JFd1, <<?M_MARK>>} ->
	    jfd_skip2(JFd1);
	{JFd1, <<_B>>} ->
	    ?dbg("jfd_skip: Skip non marker byte ~w\n", [_B]),
	    jfd_skip(JFd1);
	Error ->
	    Error
    end.

jfd_skip2(JFd) ->
    case jfd_read_bytes(JFd,1) of
	{JFd1, <<?M_MARK>>} ->
	    ?dbg("jfd_skip2: Skip filler byte\n", []),
	    jfd_skip2(JFd1);
	{JFd1, <<Mark>>} ->
	    {JFd1, Mark};
	Error ->
	    Error
    end.
    

%% Run huffman decode on one the bit buffer
jfd_decode_bits(JFd, H) ->
    jfd_decode_bits_(JFd, [], H).

jfd_decode_bits_(JFd, Ds, H) ->
    jfd_decode_bits_(JFd#jfd.bits, H, Ds, JFd).

jfd_decode_bits_(<<0:1,Bits/bits>>, {L,_}, Ds, JFd) ->
    jfd_decode_bits_(Bits, L, [$0|Ds], JFd);
jfd_decode_bits_(<<1:1,Bits/bits>>, {_,R}, Ds, JFd) ->
    jfd_decode_bits_(Bits, R, [$1|Ds], JFd);
jfd_decode_bits_(<<>>, H, Ds, JFd) when is_tuple(H) ->
    JFd1 = jfd_load_bits(JFd#jfd{bits=(<<>>)},8),
    if bit_size(JFd1#jfd.bits) == 0 ->
	    ?dbg("~s => (<<>>)\n", [reverse(Ds)]),
	    erlang:error({error, not_a_code});
       true ->
	    jfd_decode_bits_(JFd1#jfd.bits, H, Ds, JFd1)
    end;
jfd_decode_bits_(Bits, Code, _Ds, JFd) when is_integer(Code) ->
    % io:format("~s => ~w\n", [reverse(_Ds), Code]),
    {JFd#jfd {bits=Bits }, Code};
jfd_decode_bits_(_Bits, _Code, _Ds, _JFd) ->
    % ?dbg("~s => ~w (~w)\n", [reverse(Ds), Code, Bits]),
    erlang:error({error, not_a_code}).

%% Byte align the the bit stream (ditch the bits)
jfd_byte_align(JFd) ->
    ?dbg("byte align bits=~w\n", [bit_size(JFd#jfd.bits)]),
    JFd#jfd { bits=(<<>>) }.

%% align the bit stream and read N bytes 
%% return {JFd', Bytes} | {JFd', eof}
jfd_read_bytes(JFd, N) ->
    JFd1 = jfd_byte_align(JFd),
    if N == 0 ->
	    {JFd1, <<>>};
       byte_size(JFd1#jfd.bytes) >= N ->
	    <<X:N/binary, Bytes/binary>> = JFd1#jfd.bytes,
	    {JFd1#jfd { bytes=Bytes }, X};
       true ->
	    JFd2 = jfd_load_bytes(JFd1, 2*N+2),
	    if byte_size(JFd2#jfd.bytes) >= N ->
		    <<X:N/binary, Bytes/binary>> = JFd2#jfd.bytes,
		    {JFd2#jfd { bytes = Bytes }, X};
	       true -> %% can not load M bits so return eof
		    {JFd2, eof}
	    end
    end.

jfd_load_bytes(JFd, N) ->
    case file:read(JFd#jfd.fd, N) of
	{ok,Bin} ->
	    Buf = <<(JFd#jfd.bytes)/binary, Bin/binary>>,
	    JFd#jfd { bytes = Buf };
	eof ->
	    JFd
    end.

%% read N bits
jfd_read_bits(JFd, N) ->
    if
	bit_size(JFd#jfd.bits) >= N ->
	    <<X:N/bits, Bits/bits>> = JFd#jfd.bits,
	    {JFd#jfd { bits=Bits}, X};
	true ->
	    JFd1 = jfd_load_bits(JFd,N),
	    if bit_size(JFd1#jfd.bits) >= N ->
		    <<X:N/bits, Bits/bits>> = JFd1#jfd.bits,
		    {JFd1#jfd { bits=Bits}, X};
	       true ->
		    {JFd1, eof}
	    end
    end.


%% Load at least N bits into bit buffer

jfd_load_bits(JFd, N) ->
    jfd_load_bits_(JFd, JFd#jfd.bits, JFd#jfd.bytes, N).

jfd_load_bits_(JFd, Bits, Bytes, N) ->
    if bit_size(Bits) >= N ->
	    JFd#jfd { bits=Bits, bytes=Bytes };
       true ->
	    case Bytes of
		<<16#FF,16#00,Bytes1/binary>> ->
		    jfd_load_bits_(JFd,<<Bits/bits,16#FF>>,Bytes1,N);
		<<16#FF,B,Bytes1/binary>> ->
		    ?dbg("unstuff: JPEG BUG found marker 16#FF~2.16.0B\n", [B]),
		    jfd_load_bits_(JFd,<<Bits/bits,B>>,Bytes1,N);
		<<16#FF>> ->
		    JFd1 = jfd_load_bytes(JFd#jfd { bytes=Bytes},64),
		    if byte_size(JFd1#jfd.bytes) >= 2 ->
			    jfd_load_bits_(JFd1,Bits,JFd1#jfd.bytes,N);
		       true ->
			    JFd1#jfd { bits=Bits }
		    end;
		<<B,Bytes1/binary>> ->
		    jfd_load_bits_(JFd,<<Bits/bits,B>>,Bytes1,N);
		<<>> ->
		    JFd1 = jfd_load_bytes(JFd#jfd{bytes=(<<>>)},64),
		    if byte_size(JFd1#jfd.bytes) >= 1 ->
			    jfd_load_bits_(JFd1,Bits,JFd1#jfd.bytes,N);
		       true ->
			    JFd1#jfd { bits=Bits }
		    end
	    end
    end.
	

% %% decode all DHT tables
% decode_dht(<<_:3,AC:1,Ti:4,Bin/binary>>, IMG) ->
%     {DHT,Bin1} = epx_huffman:decode_dht(Bin),
%     if AC==0 -> 
% 	    ?dbg("DHT: DC table=~p\n", [Ti]);
%        true ->
% 	    ?dbg("DHT: AC table=~p\n", [Ti])
%     end,
%     %emit_dht(DHT),
%     %IMG1 = erl_img:set_attribute(IMG, {dht,AC,Ti}, DHT),
%     decode_dht(Bin1, IMG);
% decode_dht(<<>>, IMG) ->
%     IMG.


% %% decode all DQT tables
% decode_dqt(<<0:4,Ti:4,Bits/bits>>, IMG) ->
%     ?dbg("DQT: Prec=~p,Ti=~p\n",[8,Ti]),
%     decode_dqt(8, Ti, 64, Bits, [], IMG);
% decode_dqt(<<Prec:4,Ti:4,Bits/bits>>, IMG) ->
%     ?dbg("DQT: Prec=~p,Ti=~p\n",[Prec+1,Ti]),
%     decode_dqt(Prec+1, Ti, 64, Bits, [], IMG);
% decode_dqt(<<>>, IMG) ->
%     IMG.

% decode_dqt(_Len, Ti, 0, Bits, Acc, IMG) ->
%     DQT = reverse(Acc),
%     %?dbg("DQT: Table ~w\n", [Ti]), emit_8x8(DQT),
%     %IMG1 = erl_img:set_attribute(IMG, {dqt,Ti}, DQT),
%     decode_dqt(Bits, IMG);
% decode_dqt(Len, Ti, I, Bits, Acc, IMG) ->
%     <<V:Len,Bits1/bits>> = Bits,
%     decode_dqt(Len, Ti, I-1, Bits1, [V|Acc], IMG).

dequantize([C|Cs], [Q|Qs]) ->
    [C*Q | dequantize(Cs, Qs)];
dequantize([], []) ->
    [].

quantize([C|Cs], [Q|Qs]) ->
    [trunc((C / Q)+0.5) | quantize(Cs, Qs)];
quantize([], []) ->
    [].
    

%%
%% JPEG Block A[64] (or A[8][8])  encoded as:
%%   0. Coefficent A[0] (DC) is coded with difference endcoding
%%   1. Rest of A is coded with RLC  (run length encoding) as:
%%      [ {N1,Byte1}, {N2,Byte2} ... {Nk,Bytek} ] where
%%        Ni is the number of zeros proceeding the Bytei
%%      {0,0} is special and mark end of block (if block is ending with zeros!)
%%        Ni < 16!
%%      {15,0} is special and means 16 zeros!!!
%%
%%   2. Each Bytei is then converted into {NBits,Bits}
%%      where NBits (category) is the smallest number used to represent the Byte
%%      Negative numbers are one complemented
%%
%%   3. Huffman encode the byte code <<Ni:4,NBits:4>> and store the
%%      bits followed by  <<Bits:NBits>>
%%

category_encode(X) when X < 0 ->
    N = nbits(-X),
    {N, (bnot (-X)) band ((1 bsl N)-1)};
category_encode(X) when X > 0 ->
    N = nbits(X),
    {N, X}.

%% determine number of bits (< 16) needed to represent X
nbits(X) when X > 16#ff, X =< 16#7fff -> nbits8(X bsr 8, 8);
nbits(X) when X > 0, X =< 16#7fff -> nbits8(X, 0).

nbits8(X, N) when X > 16#f -> nbits4(X bsr 4, N+4);
nbits8(X, N) -> nbits4(X, N).

nbits4(X, N) when X > 16#3 -> nbits2(X bsr 2, N+2);
nbits4(X, N) -> nbits2(X, N).

nbits2(0, N) -> N;
nbits2(1, N) -> N+1;
nbits2(2, N) -> N+2;
nbits2(3, N) -> N+2.

category_decode(N,V) ->
    if V band (1 bsl (N-1)) == 0 ->
	    -((bnot V) band ((1 bsl N) - 1));
       true  ->
	    V
    end.

%% decode 64 DCT valus
decode_block(JFd, Dc0, C) ->
    %% Handle DC  A[0]
    {JFd1,N} = jfd_decode_bits(JFd,C#comp.dcd),
    {JFd2, <<V:N>>} = jfd_read_bits(JFd1,N),
    Diff = category_decode(N,V),
    %% ?dbg("DC: Diff=~p, DC=~p\n", [Diff,Dc0+Diff]),
    %% Handle AC  A[1..63]
    decode_block_ac(JFd2, 63, [Dc0 + Diff], C).

decode_block_ac(JFd, 0, Acs, _C) ->
    {JFd, reverse(Acs)};
decode_block_ac(JFd, I, Acs, C) when I > 0 ->
    {JFd1,V} = jfd_decode_bits(JFd,C#comp.acd),
    S = V band 16#f,          %% size
    R = (V bsr 4) band 16#f,  %% run length
    if
	S == 0 ->
	    if 
		R == 0 ->
		    decode_eob(JFd1, I, Acs);
		R == 15 ->
		    Z1 = min(I, 16),
		    Acs1 = cat_zeros(Z1, Acs),
		    decode_block_ac(JFd1, I-Z1, Acs1, C);
		R > I ->
		    ?dbg("JPEG BUG R=~w >= I=~w!!!\n", [R,I]),
		    Acs1 = cat_zeros(I, Acs),
		    decode_block_ac(JFd1, 0, Acs1, C);
	       true ->
		    ?dbg("JPEG BUG R=~w >= I=~w!!!\n", [R,I]),
		    Acs1 = cat_zeros(R, Acs),
		    decode_block_ac(JFd1, I-R, Acs1, C)
	    end;
       R >= I ->
	    ?dbg("JPEG BUG: R=~w >= I=~w, S = ~w!!!\n", [R,I,S]),
	    Acs1 = cat_zeros(I, Acs),
	    decode_block_ac(JFd1, 0, Acs1, C);
       true ->
	    %% ?dbg("(~w,~w)\n", [R,S]),
	    Acs1 = cat_zeros(R, Acs),
	    {JFd2,<<Y:S>>} = jfd_read_bits(JFd1,S),
	    X = category_decode(S,Y),
	    decode_block_ac(JFd2, (I-R)-1, [X|Acs1], C)
    end.

cat_zeros(0, Acs) -> Acs;
cat_zeros(I, Acs) -> cat_zeros(I-1, [0|Acs]).

decode_eob(JFd,0,Acs) ->
    {JFd, reverse(Acs)};
decode_eob(JFd,I,Acs) ->
    decode_eob(JFd,I-1,[0|Acs]).


%% Start Of Frame
process_sofn(_M,<<_Depth:8,Height:16,Width:16,_NComp:8,_Bin/binary>>, IMG) ->
  %   ?dbg("SOF(~p), Depth=~p, Height=~p, Width=~p, Components=~p\n",
	 % [_M-?M_SOF0,Depth,Height,Width,NComp]),
    %% Depth: typically 8.. (12,16)
    IMG1 = IMG#erl_image { height=Height,width=Width },
    %process_sofn_component(NComp,Bin, IMG1).
    IMG1.

component_id(1) -> y;  %% lumincance
component_id(2) -> cb; %% 
component_id(3) -> cr; %% 
component_id(4) -> i;  %%
component_id(5) -> q.  %%
    
process_sofn_component(0, _Bin, IMG) ->
    IMG;
process_sofn_component(I, <<ID:8,H:4,V:4,Q:8,Bin/binary>>,IMG) ->
    Format = component_id(ID),
    IMG1 = erl_img:set_attribute(IMG, {component,Format}, {Q,H,V}),
    ?dbg("component ~p q=~p, h=~p, v=~p\n", [Format,Q,H,V]),
    process_sofn_component(I-1, Bin, IMG1).
    

% collect_exif(Fd, T, St) ->
%     ?dbg("EXIF(~s) ~p ~p ~p\n", 
% 	[T#tiff_entry.ifd,
% 	 exif:decode_tag(T#tiff_entry.tag),
% 	 T#tiff_entry.type, T#tiff_entry.value]),
%     case T#tiff_entry.tag of
% 	?ExifInteroperabilityOffset ->
% 	    [Offset] = T#tiff_entry.value,
% 	    %% could be handle by a collect_interop?
% 	    case image_tiff:scan_ifd(Fd, [$0,$.|T#tiff_entry.ifd],
% 				  Offset, T#tiff_entry.endian,
% 				  fun collect_exif/3, St) of
% 		{ok, St1} ->
% 		    St1;
% 		_Error ->
% 		    St
% 	    end;
% 	?MakerNote ->
% 	    case collect_maker(Fd, T, St) of
% 		{ok,St1} ->
% 		    St1;
% 		_Error ->
% 		    St
% 	    end;
% 	_ ->
% 	    St
%     end.


%% Image info collector functions
collect_tiff(_Fd, T, St) ->
    %Key = image_tiff:decode_tag(T#tiff_entry.tag),
    ?dbg("TIFF(~s) ~p ~p ~p\n", 
	[T#tiff_entry.ifd,Key,T#tiff_entry.type, T#tiff_entry.value]),
    case T#tiff_entry.tag of
		?ImageWidth ->
		    [Width] = T#tiff_entry.value,
		    St#erl_image { width = Width };
		?ImageLength ->
		    [Length] = T#tiff_entry.value,
		    St#erl_image { height = Length }
		% ?BitsPerSample ->
		%     %Bs = T#tiff_entry.value,
		    %St#erl_image { depth = lists:sum(Bs) };
		% ?ImageDescription ->
		%     [Value] = T#tiff_entry.value,
		%     St#erl_image { comment = Value };
		% ?DateTime ->
		%     [Value] = T#tiff_entry.value,
		%     case string:tokens(Value, ": ") of
		% 	[YYYY,MM,DD,H,M,S] ->
		% 	    DateTime = {{list_to_integer(YYYY),
		% 			 list_to_integer(MM),
		% 			 list_to_integer(DD)},
		% 			{list_to_integer(H),
		% 			 list_to_integer(M),
		% 			 list_to_integer(S)}},
		% 	    St#erl_image { itime = DateTime};
		% 	_ ->
		% 	    St
		%     end;
		% ?ExifOffset ->
		%     [Offset] = T#tiff_entry.value,
		%     case image_tiff:scan_ifd(Fd, [$0,$.|T#tiff_entry.ifd],
		% 			  Offset, T#tiff_entry.endian,
		% 			  fun collect_exif/3, St) of
		% 	{ok, St1} ->
		% 	    St1;
		% 	_Error ->
		% 	    St
		%     end;
		% _ ->
		%     Value = T#tiff_entry.value,
		%     As = St#erl_image.attributes,
		%     St#erl_image { attributes = [{Key,Value}|As]}
    end.

% process_jfif(Bin, IMG) ->
%     case Bin of
% 	<<_Version:16,_Units:8,
% 	 _Xdensity:16, _Ydensity:16,
% 	 _Xthumbnail:8, _Ythumbnail:8,_RGB/binary>> ->
% 	    IMG;
% 	_ ->
% 	    IMG
%     end.

process_exif(Bin, IMG) ->
    case image_tiff:scan_binary(Bin, fun collect_tiff/3, IMG) of
	{ok, IMG1} ->
	    IMG1;
	_Error ->
	    IMG
    end.




-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_dimensions_jpeg_test() ->
	{ok, Bytes} = file:read_file("../test_images/tree.jpg"),
	{ok, Image} = erl_img:load(Bytes),
	?assertEqual(183, Image#erl_image.width),
	?assertEqual(275, Image#erl_image.height).

%% For some reason we cannot get the dimensions of this file...
% get_dimensions_jpeg_2_test() ->
% 	{ok, Bytes} = file:read_file("../test_images/reddeer.jpg"),
% 	{ok, Image} = erl_img:load(Bytes),
% 	?assertEqual(1120, Image#erl_image.width),
% 	?assertEqual(410, Image#erl_image.height).

-endif.