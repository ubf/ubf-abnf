%%% Description: rfc5234_core module
%%%-------------------------------------------------------------------

-module(abnf_rfc5234_core).

%% NOTE the following two lines
-compile({parse_transform,abnf_contract_parser}).
-add_contract("./test/eunit/rfc5234_core").
