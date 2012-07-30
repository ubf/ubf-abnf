%%% Description: eunit tests for ubf and abnf
%%%-------------------------------------------------------------------

-module(ubf_abnf_samples_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).


do_eunit() ->
    case eunit:test(?MODULE) of
        ok -> ok;
        _ -> erlang:halt(1)
    end.


%%%----------------------------------------------------------------------
%%% TESTS
%%%----------------------------------------------------------------------

all_tests_test_() ->
    {setup,
     fun test_setup/0,
     fun test_teardown/1,
     (all_actual_tests_rfc5234_core_all())(not_used) ++
     (all_actual_tests_rfc3986_all())(not_used)
    }.


%% rfc5234 core
all_actual_tests_rfc5234_core_all() ->
    fun (_) ->
            [?_test(test_wsp_1())
             , ?_test(test_wsp_2())
             , ?_test(test_wsp_1())
             , ?_test(test_wsp_2())
             , ?_test(test_wsp_3())
             , ?_test(test_wsp_4())
             , ?_test(test_wsp_5())
             , ?_test(test_crlf_1())
             , ?_test(test_crlf_2())
             , ?_test(test_crlf_3())
             , ?_test(test_crlf_4())
             , ?_test(test_crlf_5())
             , ?_test(test_crlf_6())
             , ?_test(test_lwsp_1())
             , ?_test(test_lwsp_2())
             , ?_test(test_lwsp_3())
             , ?_test(test_lwsp_4())
             , ?_test(test_lwsp_5())
            ]
    end.


%% rfc3986
all_actual_tests_rfc3986_all() ->
    fun (_) ->
            [?_test(test_uri_1())
             , ?_test(test_uri_2())
            ]
    end.


%% setup
test_setup() ->
    noop.


%% teardown
test_teardown(_) ->
    ok.


%%%-------------------------------------------------------------------
%% simple rfc5234 core smoke tests helpful to demonstrate the abnf
%% types contract checker
test_rfc5234_core(T,TypeName) ->
    Contract = abnf_rfc5234_core,
    {Type,_} = Contract:contract_type(TypeName),
    contracts:isType(Type, T, Contract).

test_wsp(T) ->
    test_rfc5234_core(T,'WSP').
test_crlf(T) ->
    test_rfc5234_core(T,'CRLF').
test_lwsp(T) ->
    test_rfc5234_core(T,'LWSP').


test_wsp_1() ->
    ?assertNot(test_wsp(<<"">>)).
test_wsp_2() ->
    ?assert(test_wsp(<<" ">>)).
test_wsp_3() ->
    ?assert(test_wsp(<<"\t">>)).
test_wsp_4() ->
    ?assertNot(test_wsp(<<"a">>)).
test_wsp_5() ->
    ?assertNot(test_wsp(<<" a">>)).

test_crlf_1() ->
    ?assertNot(test_crlf(<<"">>)).
test_crlf_2() ->
    ?assertNot(test_crlf(<<" ">>)).
test_crlf_3() ->
    ?assert(test_crlf(<<"\n">>)).
test_crlf_4() ->
    ?assertNot(test_crlf(<<"\r">>)).
test_crlf_5() ->
    ?assert(test_crlf(<<"\r\n">>)).
test_crlf_6() ->
    ?assertNot(test_crlf(<<"\n\r">>)).

test_lwsp_1() ->
    ?assert(test_lwsp(<<"">>)).
test_lwsp_2() ->
    ?assert(test_lwsp(<<" ">>)).
test_lwsp_3() ->
    ?assert(test_lwsp(<<"\t">>)).
test_lwsp_4() ->
    ?assertNot(test_lwsp(<<"a">>)).
test_lwsp_5() ->
    ?assertNot(test_lwsp(<<" a">>)).


%%%-------------------------------------------------------------------
%% simple rfc3986 smoke tests helpful to demonstrate the abnf types
%% contract checker
test_rfc3986(T,TypeName) ->
    Contract = abnf_rfc3986,
    {Type,_} = Contract:contract_type(TypeName),
    contracts:isType(Type, T, Contract).

test_uri(T) ->
    test_rfc3986(T,'URI').


test_uri_1() ->
    ?assert(test_uri(<<"http://github.com/ubf/ubf-abnf">>)).
test_uri_2() ->
    ?assertNot(test_uri(<<"ht/tp://github.com/ubf/ubf-abnf">>)).

