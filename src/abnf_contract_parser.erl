%%% $Id$
%%% Description: abnf contract parser
%%%-------------------------------------------------------------------

-module(abnf_contract_parser).
-include("ubf_impl.hrl").
-include("abnf_impl.hrl").

-export([parse_transform/2,
         file/1
        ]).

parse_transform(In, _Opts) ->
    %% io:format("In:~p~n   Opts: ~p~n",[In, _Opts]),
    Imports = [X || {attribute, _, add_types, X} <- In],
    Out = case [X || {attribute, _, add_contract, X} <- In] of
              [File] ->
                  %% io:format("Contract: ~p ~p~n", [File, Imports]),
                  case file1(File ++ ".abnf", Imports) of
                      ok ->
                          io:format("THIS IS NOT FAIR!\n"),
                          exit(error);
                      {ok, Contract, _Header} ->
                          %% io:format("Contract added:~n"),
                          contract_parser:parse_transform_contract(In, Contract);
                      {error, Why} ->
                          io:format("Error in contract:~p~n", [Why]),
                          exit(error)
                  end;
              [] ->
                  In
          end,
    Out.

infileExtension()  -> ".abnf".

file(F) ->
    case {infileExtension(), filename:extension(F)} of
        {X, X} ->
            %% io:format("Parsing ~s~n", [F]),
            case file1(F) of
                {ok, Contract, _Header} ->
                    %% contract - buc
                    Enc = ubf:encode(Contract),
                    ok = file:write_file(filename:rootname(F) ++
                                         contract_parser:outfileExtension(),
                                         Enc),
                    Size = length(Enc),
                    Bsize = size(term_to_binary(Contract)),
                    {ok, {ubfSize,Size,bsize,Bsize}};
                Error ->
                    Error
            end;
        _ ->
            {error, bad_extension}
    end.

file1(F) ->
    file1(F,[]).

file1(F, Imports) ->
    case file:read_file(F) of
        {ok, Bin} ->
            case abnfc:parse(binary_to_list(Bin), []) of
                {ok, AST, _Rest} ->
                    Name = filename:basename(F, infileExtension()),
                    VSN = "",
                    Types = ast2ubf(abnfc_ast:ast_to_int_form(AST)),
                    %% io:format("~n~p~n", [Types]),
                    contract_parser:tags([{name,Name}, {vsn,VSN}, {types,Types}], Imports);
                E ->
                    E
            end;
        E ->
            E
    end.

%% -record(rule, {type, name, body, code}).
ast2ubf(#rule{type=def_rule, name=Name, body=Body, code=nocode}) ->
    {Name, ast2ubf(Body), ""};
%% -record(rulename, {name}).
ast2ubf(#rulename{name=Name}) ->
    {prim, 1, 1, Name};
%% -record(alt, {alts=list()}).
ast2ubf(#alt{alts=Alts}) ->
    {abnf_alt, [ ast2ubf(A) || A <- Alts ]};
%% -record(seq, {elements=list()}).
ast2ubf(#seq{elements=Elements}) ->
    {abnf_seq, [ ast2ubf(E) || E <- Elements ]};
%% -record(repeat, {min=0, max=infinity, body}).
ast2ubf(#repeat{min=Min, max=Max, body=Body}) ->
    {abnf_repeat, Min, Max, ast2ubf(Body)};
%% -record(char_range, {from, to}).
ast2ubf(#char_range{from=From, to=To}) ->
    {abnf_byte_range, From, To};
%% -record(char_alt, {alts=list()}).
ast2ubf(#char_alt{alts=Alts}) ->
    {abnf_byte_alt, [ ast2ubf(A) || A <- Alts ]};
%% -record(char_seq, {elements=list()}).
ast2ubf(#char_seq{elements=Elements}) ->
    {abnf_byte_seq, [ ast2ubf(E) || E <- Elements ]};
%% -record(char_val, {value=[]}).
ast2ubf(#char_val{value=Value}) ->
    {abnf_byte_val, Value};
ast2ubf(AST) when is_list(AST) ->
    ast2ubf(AST, []).

ast2ubf([], Acc) ->
    lists:reverse(Acc);
ast2ubf([H|T], Acc) ->
    ast2ubf(T, [ast2ubf(H)|Acc]).
