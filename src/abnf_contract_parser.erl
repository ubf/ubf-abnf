%%% Description: abnf contract parser
%%%-------------------------------------------------------------------

-module(abnf_contract_parser).
-include_lib("ubf/include/ubf_impl.hrl").
-include("abnf_impl.hrl").

-export([parse_transform/2]).

%%====================================================================
%% External Parse Transform
%%====================================================================

parse_transform(In, _Opts) ->
    %% io:format("In:~p~n   Opts: ~p~n",[In, _Opts]),
    Name = case [X || {attribute, _, module, X} <- In] of [M] -> atom_to_list(M) end,
    VSN = case [X || {attribute, _, vsn, X} <- In] of [V] -> V; _ -> "" end,
    Imports = [X || {attribute, _, add_types, X} <- In],
    Out = case [X || {attribute, _, add_contract, X} <- In] of
              [File] ->
                  case file(Name, VSN, Imports, File ++ infileExtension()) of
                      {ok, Contract, _Header} ->
                          %% io:format("Contract added: ~p~n", [Contract]),
                          contract_parser:parse_transform_contract(In, Contract);
                      {error, Why} ->
                          io:format("Error in contract:~p~n", [Why]),
                          erlang:error(Why)
                  end;
              [] ->
                  In
          end,
    Out.


%%====================================================================
%% External API
%%====================================================================


%%====================================================================
%% Internal functions
%%====================================================================

infileExtension()  -> ".abnf".

file(Name, VSN, Imports, F) ->
    %% io:format("~n~p ~p~n~p~n~p~n", [Name, VSN, Imports, F]),
    case file:read_file(F) of
        {ok, Bin} ->
            case abnfc:parse(binary_to_list(Bin), [{parser,abnfc_rfc4234ext}]) of
                {ok, AST, _Rest} ->
                    Types = ast2ubf(abnfc_ast:ast_to_int_form(AST)),
                    %% io:format("~n~p~n", [Types]),
                    contract_parser:tags([{name,Name}, {vsn,VSN}, {types,Types}], Imports);
                Err ->
                    Err
            end;
        Err ->
            Err
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
