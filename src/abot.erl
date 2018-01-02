-module( abot ).

-export( [init/1, handle_privmsg/4, handle_join/2, handle_part/2] ).

init( _Arg ) ->
  [].

handle_privmsg( public, _Sender, "hello", State ) ->
  {reply, "Hi there.", State};

handle_privmsg( private, _Sender, "hello", State ) ->
  {reply, "Psst!", State};

handle_privmsg( _, _, _, State ) ->
  {noreply, State}.

handle_join( _User, State ) ->
  State.

handle_part( _User, State ) ->
  State.