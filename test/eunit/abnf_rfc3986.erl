%%% Description: rfc3986 module
%%%-------------------------------------------------------------------

-module(abnf_rfc3986).

%% NOTE the following two lines
-compile({parse_transform,abnf_contract_parser}).
-add_contract("./test/eunit/rfc3986").
-add_types({abnf_rfc5234_core, ['ALPHA','DIGIT','HEXDIG']}).
