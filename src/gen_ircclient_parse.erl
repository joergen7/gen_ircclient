-module( gen_ircclient_parse ).

-export( [parse_msg/1] ).

-include( "gen_ircclient.hrl" ).


parse_msg( S ) ->
  parse_msg( prefix, S, #msg{} ).

parse_msg( prefix, [$:|S], Msg ) ->
  [Prefix, Rest] = string:split( S, " " ),
  parse_msg( command, Rest, Msg#msg{ prefix = Prefix } );

parse_msg( prefix, S, Msg ) ->
  parse_msg( command, S, Msg );

parse_msg( command, S, Msg ) ->
  [Command, Rest] = string:split( S, " " ),
  parse_msg( arg_lst, Rest, Msg#msg{ command = Command } );

parse_msg( arg_lst, [$:|S], Msg = #msg{ arg_lst = ArgLst } ) ->
  Msg#msg{ arg_lst = ArgLst++[S] };

parse_msg( arg_lst, S, Msg = #msg{ arg_lst = ArgLst } ) ->
  case string:split( S, " " ) of
    [LastArg]   -> Msg#msg{ arg_lst = ArgLst++[LastArg] };
    [Arg, Rest] -> parse_msg( arg_lst, Rest, Msg#msg{ arg_lst = ArgLst++[Arg]} )
  end.
