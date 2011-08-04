%% This file was created from eweb
%% please do not edit 
%% Copyright (C) 1997-1999, Ericsson Telecom AB
-module(eweb).
-export([file/1, dir/0]).

-import(lists, [dropwhile/2, filter/2, foldl/3, foreach/2, 
		map/2, mapfoldl/3, member/2, reverse/1, 
		splitwith/2, sublist/3, suffix/2]).
file(File) ->
    LitFile = File ++ ".lit",
    io:fwrite("Processing:~p~n", [LitFile]),
    case (catch begin
	  clear_errors(),
	  Strings = read_file(LitFile),
	  throw_if_errors(),
	  {OutDir, OutFiles, Copy, Blocks0} = 
	      strings2blocks(Strings),
	  throw_if_errors(),
	  Blocks1 = number_blocks(Blocks0),
	  throw_if_errors(),
	  {Blocks, Dict} = number_lines(Blocks1),
	  throw_if_errors(),
	  eweave(OutDir, File, Blocks, Dict),
	  etangle(File, OutDir, OutFiles, Copy, Blocks)
	  end) of
	{errors, E} ->
	    report_errors(E),
	    error;
	{'EXIT', Why} ->
	    io:fwrite("EXIT ~p~n", [Why]),
	    error;
	Other ->
	    Other
    end.
read_file(File) ->
    case catch file2strings(File) of
	{'EXIT', Why}->
	    error({cannot_read,file,File,Why});
	Strings ->
	    Strings
    end.
strings2blocks([{_, [$# | _]} | T]) ->
    strings2blocks(T);
strings2blocks([{L, [$e, $w, $e, $b | S]} | T]) ->
    parse_header([{L, [$e, $w, $e, $b | S]} | T]);
strings2blocks(_) ->
    fatal({file,has,incorrect,header}).

parse_header([{_,[$e,$w,$e,$b,$1,$.,$0|_]} | T]) ->
    parse_header_1(T);
parse_header([{_,[$e,$w,$e,$b,$1,$.,$1|_]} | T]) ->
    parse_header_1(T);
parse_header(_) ->
    fatal({file,has,incorrect,header}).

parse_header_1([{_,[$o,$u,$t,$d,$i,$r,$=|OutDir]},
	        {_,[$o,$u,$t,$f,$i,$l,$e,$s,$=|OutFiles]}
	        |T]) ->
    {Copyright, Blocks} = parse_options(T),
    {OutDir, OutFiles, Copyright, Blocks};
parse_header_1(_) ->
    fatal({file,has,incorrect,header}).
parse_options([{_,[$c,$o,$p,$y,$r,$i,$g,$h,$t,$=|C]}
	       | T]) ->
	{C, parse1(T)};
parse_options(T) ->
	{"", parse1(T)}.
parse1(T) -> parse1(T, []).

parse1([], Blocks) ->
    reverse(Blocks);
parse1(T, Blocks) ->
    case get_block(T) of
	none ->
	    reverse(Blocks);
	{B, T1} ->
	    parse1(T1, [B|Blocks])
    end.
get_block([]) ->
    none;
get_block([{Line,[$<,$<|T1]}|T2]) ->
    Header = parse_block_header(Line, [$<,$<|T1]),
    {Lines, T3} = get_tagged_block_body(T2, []),
    {{block, Line, Header, Lines}, T3};
get_block([H|T]) ->
    {Lines, T1} = get_untagged_block(T, [H]),
    {{tex, Lines}, T1}.
get_tagged_block_body([], Lines) ->
    {reverse(Lines), []};
get_tagged_block_body([{_,[$>,$>|_]}|T], Lines) ->
    {reverse(Lines), T};
get_tagged_block_body([H|T], Lines) ->
    get_tagged_block_body(T, [parse_data(H)|Lines]).
get_untagged_block([H|T], Lines) ->
    case H of
	{_Line,[$<,$<|_]} ->
	    {reverse(Lines), [H|T]};
	_ ->
	    get_untagged_block(T, [H|Lines])
    end;
get_untagged_block([], Lines) ->
    {reverse(Lines), []}.
parse_block_header(Line, Str) ->
    case parse_tag(Str) of
	{ok, Hide, Tag, Str1} ->
	    case parse_after_tag(Str1) of
		{ok, Type} ->
		    {Hide, Tag, Type};
		error ->
		    error({badTagFollower,line,Line,
			   must,be,'+=','or',':='})
	    end;
	error ->
	    error({badTag,line,Line,is,Str})
    end.
parse_tag(Str1) ->
    case skip_white_space(Str1) of
	[$<,$<,$(,$h,$i,$d,$e,$)|Str2] ->
		case collect_name(Str2) of
		    {ok, Tag, Rest} ->
			{ok, hide, Tag, Rest};
		    error ->
			error
		end;
	[$<,$<|Str2] ->
	    case collect_name(Str2) of
		{ok, Tag, Rest} ->
		    {ok, show, Tag, Rest};
		error ->
		    error
	    end;
	_ ->
	    error
    end.
collect_name(Str) -> collect_name(Str, []).

collect_name([$>,$>|T], L) -> {ok, reverse(L), T};
collect_name([H|T], L)	   -> collect_name(T, [H|L]);
collect_name([], _)	   -> error.
parse_after_tag(Str) ->
    case skip_white_space(Str) of
	[$+,$=|_] -> {ok, continue};
	[$:,$=|_] -> {ok, define};
	_	  -> error
    end.
parse_data({Line, Str}) ->
    case (Str1 = skip_white_space(Str)) of
	[$<,$<|T] ->
	    case parse_tag(Str1) of
		{ok, show, Tag, _} ->
		    {Line, {include,Tag,Str}};
		_ ->
		    error({badTagRef,line,Line,"=",Str})
	    end;
	_ ->
	    {Line,Str}
    end.
number_blocks(Blocks) -> 
    Dict0 = dict:new(),
    {Blocks1,Dict} = n_pass1(Blocks, [], Dict0),
    reverse(map(fun(X) -> n_pass2(X,Dict) end, Blocks1)).
n_pass1([{block,Line,{Hide,Tag,define},Data}|T],L,Dict) ->
    case dict:find(Tag, Dict) of
	error ->
	    n_pass1(T,[{block2,Line,{Hide,Tag,1},Data}|L],
			   dict:store(Tag,1,Dict));
	{ok, _} ->
	    error({multiple_def,line,Line,tag,Tag}),
	    n_pass1(T, [error|L], Dict)
    end;
n_pass1([{block,Line,{Hide,Tag,continue},Data}|T],L,Dict) ->
    case dict:find(Tag, Dict) of
	{ok, N} ->
	    n_pass1(T,
		    [{block2,Line,{Hide,Tag,N+1},Data}|L],
		    dict:store(Tag, N+1, Dict));
	error ->
	    error({continuation_but_no_start,
		   line,Line,tag,Tag}),
	    n_pass1(T, [error|L], Dict)
    end;
n_pass1([H|T], Blocks, Dict) ->
    n_pass1(T, [H|Blocks], Dict);
n_pass1([], Blocks, Dict) ->
    {Blocks, Dict}.
n_pass2({block2, Line,{Hide,Tag,N}, Data}, Dict) ->
	Max = dict:fetch(Tag, Dict),
	{block3, Line,{Hide,Tag,{N,Max}},Data};
n_pass2(X, _) ->
    X.
number_lines(Blocks) ->
    {Blocks1, {_Max, Dict}} = 
	mapfoldl(fun number_lines/2, {1, []}, Blocks),
    {Blocks1, Dict}.

number_lines({block3, Line, {show,Tag,Indx}, Data}, A) ->
    {Data1, A1} = mapfoldl(fun number_code/2, A, Data),
    {{block4, Line, {show, Tag, Indx}, Data1}, A1};
number_lines(Other, A) ->
    {Other, A}.
number_code({_Line, {include,Tag,Str}},{N,Dict}) ->
    {{include1,N,Tag,Str},{N+1, Dict}};
number_code({_Line, Str}, {N, Dict}) ->
    case Str of
	[$%,$#|T] ->
	    Label = extract_label(T),
	    {label, {N, add_label(Label, N, Dict)}};
	_ ->
	    {{line,N,Str}, {N+1,Dict}}
    end.
extract_label([$\n|_]) -> [];
extract_label([$\t|_]) -> [];
extract_label([$ |_])  -> [];
extract_label([])      -> [];
extract_label([H|T])   -> [H|extract_label(T)].
add_label(Label, N, Dict) ->
    case lists:keysearch(Label, 1, Dict) of
	{value, _} ->
	    error({duplicate_label, Label}),
	    Dict;
	_ ->
	    [{Label,N}|Dict]
    end.
etangle(Src, OutDir, OutFiles, Copy, L) ->
    io:fwrite("Calling tangle~n", []),
    Files =  string:tokens(OutFiles, ","),
    AllBlocks = [{H,D} || {block4, _, H, D} <- L],
    UsedBlocks = 
	case Files of
	    [] ->
		io:fwrite("** Nothing to tangle~n", []),
		[];
	    _ ->
		foldl(fun(File, Used) ->
			  tangle_file(Src,OutDir,File,Copy, 
				      AllBlocks, Used)
		      end, [], Files)
    end,
    warn_unused(AllBlocks, UsedBlocks),
    Files.
warn_unused(All, Used) ->
    Defined = [{Tag,N,M} || {{_,Tag,{N,M}},_} <- All],
    NotUsed = filter(fun(X) -> 
			    not member(X, Used) 
		     end, Defined),
    case NotUsed of
	[] ->
	    true;
	_ ->
	    io:fwrite("*** warning some blocks unused..~n"
		      "~p~n", [NotUsed])
    end.
tangle_file(Src, OutDir, File, Copy, Blocks, Used) ->
    BaseName = case File of 
		   [$+|T] -> T;
		   T	  -> T
	       end,
    FullName = OutDir ++ "/" ++ BaseName,
    io:fwrite("Creating ~s~n", [FullName]),
    {ok, S} = file:open(FullName, write),
    case lists:suffix(".erl", File) of
	true ->
	    Warn = "%% This file was created from ~s~n"
		   "%% please do not edit ~n"
		   "%% ~s~n",
	    io:fwrite(S, Warn, [Src,Copy]);
	false ->
	    true
    end,
    Used1 = include_file(S, {start, BaseName}, 
			 Blocks, [], Used),
    file:close(S),
    case File of
	[$+|_] ->
	    os:cmd("chmod u+x " ++ BaseName);
	_ ->
	    true
    end,
    Used1.
include_file(S, Tag, Blocks, Path, Used) ->
    {Tag1, Data}  = find_block(Tag, Blocks),
    case member(Tag1, Path) of
	true ->
	    fatal({recursive_tangle, Tag, Path});
	false ->
	    untangle(S, Data, Tag1, Blocks,
		     [Tag1|Path], [Tag1|Used])
    end.

untangle(S,[{line,_,Str}|T], Tag,Blocks,Path,Used) ->
    io:fwrite(S, "~s~n", [Str]),
    untangle(S, T, Tag, Blocks, Path, Used);
untangle(S,[{include1,_,NextTag,_}|T],
	 Tag,Blocks,Path,Used) ->
    Used1 = include_file(S, {start, NextTag},
			 Blocks,Path,Used),
    untangle(S, T, Tag, Blocks, Path, Used1);
untangle(S, [label|T], Tag,Blocks,Path,Used) ->
    untangle(S, T, Tag, Blocks, Path, Used);
untangle(S, [], {Tag, Max, Max}, Blocks, Path, Used) ->
    Used;
untangle(S, [], {Tag, N, Max},
	 Blocks, Path, Used) ->
    include_file(S, {Tag,N+1,Max}, Blocks, Path, Used).
eweave(OutDir, File, L, Dict) ->
    FullName = OutDir ++ "/" ++ File ++ ".tex", 
    case file:open(FullName, write) of
	{ok, S} ->
	    foreach(fun(B) -> 
			   weave_block(B, Dict, S) 
		   end, L),
	    file:close(S);
	 {error, Why } ->
	    exit({cannot,open, FullName})
    end.
weave_block({tex, Lines}, Dict, S) ->
    output_tex_block(S, Dict, Lines);
weave_block({block4,Line,{show,Tag,{N,M}},Data},_,S) ->
    output_code_block(S, Tag,{N,M}, Data);
weave_block(_, _, _) -> 
    true.
output_tex_block(S, Dict, Lines) ->
    foreach(fun({_,Str}) -> 
		   put_tex_line(S, Dict, Str) 
	    end, Lines).
put_tex_line(S, Dict, [$#|_]) -> true;
put_tex_line(S, Dict, Str)    -> 
    Str1 = expand_labels(Str, Dict),
    io:fwrite(S, "~s~n", [detab(Str1)]).
expand_labels([$\\,$L,$N,${|T], Dict) ->
    {Label, T1} = get_label(T, []),
    Val = lookup(Label, Dict),
    integer_to_list(Val) ++ expand_labels(T1, Dict);
expand_labels([$\\,$S,$L,${|T], Dict) ->
    {Label, T1} = get_label(T, []),
    Val = lookup(Label, Dict),
    "{\\sl " ++ integer_to_list(Val) ++
        [$} | expand_labels(T1, Dict)];
expand_labels([H|T], Dict) ->
    [H|expand_labels(T, Dict)];
expand_labels([], Dict) ->
    [].
get_label([$}|T], L) -> {reverse(L), T};
get_label([H|T],  L) -> get_label(T, [H|L]);
get_label([], L)     -> {reverse(L), []}.
lookup(Label, [{Label,Val}|_]) -> Val;
lookup(Label, [_|T]) -> lookup(Label, T);
lookup(Label, [])    -> fatal({cannot,find,label,Label}).
output_code_block(S, Tag,{N,M}, Data) ->
    Label = mk_label(Tag, N, M),
    Refs  = mk_refs(Tag, N, M),
    io:fwrite(S, "\\begin{flushleft}~n"
		 "\\label{~s}~n", [Label]),
    io:fwrite(S, "\\noindent {\\small "
		 "{\\sl $<<$~s (~w/~w)$>>$} ~s ~s}\\\\*~n"
		 "\\tt~n",
	      [str2tex(Tag),N,M,symbol(N,M),Refs]),
    foreach(fun(X) -> put_code(S, X) end, Data),
    io:fwrite(S, "\\end{flushleft}~n", []).
put_code(S, {line,N,Str}) ->
    Str1 = verb_prepare(Str),
    io:fwrite(S, "~s\\verb&~s&\\\\~n",
	      [make_linehdr(N), detab(Str1)]);
put_code(S, {include1,N,Tag,Str}) ->
    {Blanks, _} = isolate_blanks(detab(Str)),
    DefRef = label(Tag,1,999999),
    io:fwrite(S, "~s\\verb&~s&"
		 "{\\sl $<<$~s \\pageref{~s} $>>$}\\\\~n",
	      [make_linehdr(N), Blanks,
	       detab(str2tex(Tag)), DefRef]);
put_code(S, label) ->
    void.

make_linehdr(N) ->
    io_lib:fwrite("\\noindent\\makebox[0pt][r]"
		  "{\\makebox[4.5em][l]{~w:}}%~n", [N]).
verb_prepare([$& | Cs]) ->
    [$&, $\\, $v, $e, $r, $b, $+, $&, $+,
     $\\, $v, $e, $r, $b, $& | verb_prepare(Cs)];
verb_prepare([C | Cs]) ->
    [C | verb_prepare(Cs)];
verb_prepare([]) ->
    [].
mk_label(Tag, N, M) ->
    label(Tag,N,M).

mk_refs(Tag, 1, 1) -> "";
mk_refs(Tag, 1, N) when N > 1 ->
    io_lib:fwrite("$\\bigtriangledown$ \\pageref{~s}", 
		  [label(Tag,2,N)]);
mk_refs(Tag, N, N) ->
    io_lib:fwrite("$\\bigtriangleup$ \\pageref{~s}", 
		  [label(Tag,N-1,N)]);
mk_refs(Tag, N, M) when N < M ->
    io_lib:fwrite("$\\bigtriangleup$ \\pageref{~s} \\ "
		  "$\\bigtriangledown$ \\pageref{~s}", 
		  [label(Tag,N-1,M),label(Tag,N+1,M)]);
mk_refs(_, _, _) ->
    "".

label(Tag,1,_) -> Tag ++ "_start";
label(Tag,N,M) -> io_lib:fwrite("~s_~w_~w", [Tag,N,M]).
isolate_blanks(L) -> splitwith(fun(X) -> X == 32 end, L).
symbol(1,N) -> ":=";
symbol(N,M) -> "+=".
find_block({start, Tag}, Blocks) ->
    find_block(Tag, 1, Blocks);
find_block({Tag,I,N}, Blocks) ->
    find_block(Tag, I, Blocks).

find_block(Tag, I, [{{_Hide,Tag,{I,N}},Data}|_]) ->
    {{Tag, I, N}, Data};
find_block(Tag, I, [H|Blocks]) ->
    find_block(Tag, I, Blocks);
find_block(Tag, I, []) ->
    fatal({cannot_find_block, I, Tag}).
clear_errors() -> put('error$list', []).
error(E) -> 
    put('error$list', [E|get('error$list')]), error.
throw_if_errors() ->
    case get('error$list') of
	[] ->
	    true;
	Errors ->
	    throw({errors, reverse(Errors)})
    end.
report_errors(Errors) ->
    map(fun display_error/1, Errors).

display_error(X) ->
    io:fwrite("Error:~p~n", [X]).
fatal(Error) ->
    throw({errors, [Error]}).
str2tex([$_|T]) -> [$\\,$_|str2tex(T)];
str2tex([H|T])	-> [H|str2tex(T)];
str2tex([])	-> [].
file2strings(File) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    binary2strings(Bin);
	{error, What} ->
	    exit({misc,file2strings,File})
    end.
binary2strings(Bin) when binary(Bin) ->
    binary2strings(binary_to_list(Bin), 1, [], []).

binary2strings([H|T], N, C, L) ->
    case H of
	$\n -> 
	    binary2strings(T,N+1,[],[{N,reverse(C)}|L]);
	_ -> 
	    binary2strings(T,N,[H|C],L)
    end;
binary2strings([], N, [], L) ->
    reverse(L);
binary2strings([], N, C, L) ->
    reverse([{N,reverse(C)}|L]).
skip_white_space(Str) ->
    dropwhile(fun($ )  -> true;
		 ($\n) -> true;
		 ($\t) -> true;
		 (_)   -> false 
	      end, Str).
		  
detab(L) -> detab(L, 1, []).

detab([$\t|T], N, L) when N rem 8 == 0 -> 
    detab(T, N+1, [$ |L]);
detab([$\t|T], N, L) -> 
    detab([$\t|T], N+1, [$ |L]);
detab([H|T], N, L) ->
    detab(T, N+1, [H|L]);
detab([], _, L) ->
    reverse(L).

dir() -> 
    foreach(fun file/1, 
	    find_out_of_date(".", ".lit", ".tex")).
find_out_of_date(Dir, In, Out) ->
    case file:list_dir(Dir) of
	{ok, Files0} ->
	    Files1 = filter(fun(F) -> 
				   suffix(In, F) 
			    end, Files0),
	    Files2 = map(fun(F) -> 
			   sublist(F,1,
				   length(F) - length(In)) 
			 end, Files1),
	    filter(fun(F) ->
			  update(F, In, Out) 
		   end, Files2);
	_ ->
	    []
    end.
update(File, In, Out) ->
    InFile  = File ++ In,
    OutFile = File ++ Out,
    case is_file(OutFile) of
	true ->
	    case writeable(OutFile) of
		true ->
		    outofdate(InFile, OutFile);
		false ->
		    %% can't write so we can't update
		    false
	    end;
	false ->
	    %% doesn't exist
	    true
    end.
is_file(File) ->
    case file:file_info(File) of
	{ok, _} ->
	    true;
	_ ->
	    false
    end.

writeable(F) ->
    case file:file_info(F) of
	{ok, {_,_,read_write,_,_,_,_}} -> true;
	{ok, {_,_,write	    ,_,_,_,_}} -> true;
	_ -> false
    end.

outofdate(In, Out) ->
    case {last_modified(In), last_modified(Out)} of
	{T1, T2} when T1 > T2 ->
	    true;
	_ ->
	    false
    end.

last_modified(F) ->
    case file:file_info(F) of
	{ok, {_, _, _, _, Time, _, _}} ->
	    Time;
	_ ->
	    exit({last_modified, F})
    end.
