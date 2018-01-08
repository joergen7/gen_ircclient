-module( cuneiform_irc_test ).

-include_lib( "eunit/include/eunit.hrl" ).


string_find_test() ->
  ?assertEqual( "a", string:find( "a", "a" ) ).

string_split_test() ->
  ?assertEqual( "bc", string:find( "abc", "b" ) ).

string_tokens_test() ->
  ?assertEqual( ["a", "b", "c"], string:tokens( "a b c", " " ) ).