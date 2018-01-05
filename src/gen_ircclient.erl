%% -*- erlang -*-
%%
%% A scaffold for Erlang IRC bots.
%%
%% Copyright 2018 Jörgen Brandt
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @author Jörgen Brandt <joergen.brandt@onlinehome.de>
%% @version 0.1.1
%% @copyright 2018 Jörgen Brandt
%%
%% @end
%% -------------------------------------------------------------------

-module( gen_ircclient ).
-behaviour( gen_pnet ).

%%====================================================================
%% Exports
%%====================================================================

-export( [start_link/4, start_link/5, get_nick_name/1] ).

-export( [code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
          terminate/2, trigger/3] ).

-export( [place_lst/0, trsn_lst/0, init_marking/2, preset/1, is_enabled/3,
          fire/3] ).

%%====================================================================
%% Includes
%%====================================================================

-include( "gen_ircclient.hrl" ).

%%====================================================================
%% Callback definitions
%%====================================================================

-callback init( Arg :: _ ) -> State :: _.

-callback handle_privmsg( Mode :: private | public, Sender :: string(), Content :: string(), State :: _ ) ->
              {noreply, NewState :: _}                            % keep qiet
            | {reply, Reply :: string(), NewState :: _}           % immediately reply with Reply
            | {spawn, F :: fun( () -> string() ), NewState :: _}. % spawn long-running process F which returns the reply string

-callback handle_join( User :: string(), State :: _ ) -> _.

-callback handle_part( User :: string(), State :: _ ) -> _.

%%====================================================================
%% Record definitions
%%====================================================================

-record( irc_state, {socket, nick_name, user_name, real_name, channel,
                     usr_mod, usr_arg} ).

%%====================================================================
%% API functions
%%====================================================================


-spec start_link( ConnInfo, Channel, UsrMod, UsrArg ) ->
        gen_pnet:start_link_result()
when ConnInfo :: #conn_info{},
     Channel  :: string(),
     UsrMod   :: atom(),
     UsrArg   :: _.

start_link( ConnInfo, Channel, UsrMod, UsrArg ) ->
  gen_pnet:start_link( ?MODULE, {ConnInfo, Channel, UsrMod, UsrArg}, [] ).


-spec start_link( ServerName, ConnInfo, Channel, UsrMod, UsrArg ) -> gen_pnet:start_link_result()
when ServerName :: gen_pnet:server_name(),
     ConnInfo   :: #conn_info{},
     Channel    :: string(),
     UsrMod     :: atom(),
     UsrArg     :: _.

start_link( ServerName, ConnInfo, Channel, UsrMod, UsrArg ) ->
  gen_pnet:start_link( ServerName, ?MODULE, {ConnInfo, Channel, UsrMod, UsrArg}, [] ).


%%====================================================================
%% Interface callback functions
%%====================================================================

-spec code_change( OldVsn :: _, NetState :: _, Extra :: _ ) ->
        {ok, _} | {error, _}.

code_change( _OldVsn, NetState, _Extra ) -> {ok, NetState}.


-spec handle_call( Request :: _, From :: {pid(), _}, NetState :: _ ) ->
              {reply, _}
            | {reply, _, #{ atom() => [_] }, #{ atom() => [_] }}
            | noreply
            | {noreply, #{ atom() => [_] }, #{ atom() => [_] }}
            | {stop, _, _}.

handle_call( _Request, _From, _NetState ) -> {reply, {error, bad_msg}}.


-spec handle_cast( Request :: _, NetState :: _ ) ->
              noreply
            | {noreply, #{ atom() => [_] }, #{ atom() => [_] }}
            | {stop, _}.

handle_cast( T = {privmsg, _, _}, _NetState ) ->
  {noreply, #{}, #{ 'Outbox' => [T] }};

handle_cast( _Request, _NetState ) -> noreply.


-spec handle_info( Info :: _, NetState :: _ ) ->
              noreply
            | {noreply, #{ atom() => [_] }, #{ atom() => [_] }}
            | {stop, _}.

handle_info( {tcp_closed, Socket}, NetState ) ->
  #irc_state{ socket = Socket } = gen_pnet:get_usr_info( NetState ),
  {stop, tcp_closed};

handle_info( {tcp, Socket, [$P, $I, $N, $G|X]}, NetState ) ->
  #irc_state{ socket = Socket } = gen_pnet:get_usr_info( NetState ),
  ok = gen_tcp:send( Socket, "PONG"++X ),
  noreply;

handle_info( {tcp, Socket, Data}, NetState ) ->
  #irc_state{ socket = Socket } = gen_pnet:get_usr_info( NetState ),
  [LineAcc] = gen_pnet:get_ls( 'Data', NetState ),
  {noreply, #{ 'Data' => [LineAcc] }, #{ 'Data' => [LineAcc++Data] }};

handle_info( _Request, _NetState ) -> noreply.


-spec init( Args :: _ ) -> {ok, _}.

init( {ConnInfo, Channel, UsrMod, UsrArg} )
when is_list( Channel ),
     is_atom( UsrMod ) ->

  #conn_info{ server    = Server,
              port      = Port,
              nick_name = NickName,
              user_name = UserName,
              real_name = RealName } = ConnInfo,

  % create socket
  {ok, Socket} = gen_tcp:connect( Server, Port, [list, {active, true}] ),

  error_logger:info_report( [{status, create_socket},
                             {server, Server},
                             {port, Port}] ),

  #irc_state{ socket     = Socket,
              nick_name  = NickName,
              user_name  = UserName,
              real_name  = RealName,
              channel    = Channel,
              usr_mod    = UsrMod,
              usr_arg    = UsrArg }.


-spec terminate( Reason :: _, NetState :: _ ) -> ok.

terminate( _Reason, _NetState ) -> ok.


-spec trigger( Place :: atom(), Token :: _, NetState :: _ ) ->
            pass | drop.

trigger( 'Outbox', connect, NetState ) ->

  #irc_state{ socket    = Socket,
              nick_name = NickName,
              user_name = UserName,
              real_name = RealName } = gen_pnet:get_usr_info( NetState ),

  HostName = "*",
  ServerName = "8",

  % send registration info
  ok = gen_tcp:send( Socket,
                     io_lib:format( "NICK ~s\r\nUSER ~s ~s ~s :~s\r\n",
                     [NickName, UserName, HostName, ServerName, RealName] ) ),

  error_logger:info_report( [{status, register_user},
                             {nick_name, NickName},
                             {user_name, UserName},
                             {real_name, RealName}] ),

  drop;

trigger( 'Outbox', join, NetState ) ->

  #irc_state{ socket  = Socket,
              channel = Channel } = gen_pnet:get_usr_info( NetState ),

  ok = gen_tcp:send( Socket, io_lib:format( "JOIN ~s\r\n", [Channel] ) ),

  error_logger:info_report( [{status, join_channel}, {channel, Channel}] ),

  drop;

trigger( 'Outbox', {privmsg, Receiver, Content}, NetState ) ->

  #irc_state{ socket  = Socket } = gen_pnet:get_usr_info( NetState ),

  S = io_lib:format( "PRIVMSG ~s :~s\r\n", [Receiver, Content] ),

  ok = gen_tcp:send( Socket, S ),

  error_logger:info_report( [{status, send_privmsg},
                             {content, Content}] ),

  drop;

trigger( _Place, _Token, _NetState ) -> pass.


%%====================================================================
%% Petri net callback functions
%%====================================================================

-spec place_lst() -> [atom()].

place_lst() -> ['Data', 'Inbox', 'Outbox', 'ConnState', 'UsrState'].


-spec trsn_lst() -> [atom()].

trsn_lst() -> [recv, drop_msg,
               request_connect, ack_connect, request_join, ack_join,
               privmsg, namereply, join, part, error].


-spec init_marking( Place :: atom(), UsrInfo :: _ ) -> [_].

init_marking( 'Data',      _ )        -> [""];
init_marking( 'ConnState', _ )        -> [connect];
init_marking( 'UsrState', #irc_state{ usr_mod = UsrMod, usr_arg = UsrArg } ) -> [UsrMod:init( UsrArg )];
init_marking( _Place,      _UsrInfo ) -> [].


-spec preset( Trsn :: atom() ) -> [atom()].

preset( recv )            -> ['Data'];
preset( drop_msg )        -> ['Inbox'];
preset( request_connect ) -> ['ConnState'];
preset( ack_connect )     -> ['ConnState', 'Inbox'];
preset( request_join )    -> ['ConnState'];
preset( ack_join )        -> ['ConnState', 'Inbox'];
preset( privmsg )         -> ['ConnState', 'Inbox', 'UsrState'];
preset( namereply )       -> ['ConnState', 'Inbox', 'UsrState'];
preset( join )            -> ['ConnState', 'Inbox', 'UsrState'];
preset( part )            -> ['ConnState', 'Inbox', 'UsrState'];
preset( kick )            -> ['ConnState', 'Inbox', 'UsrState'];
preset( error )            -> ['ConnState', 'Inbox', 'UsrState'].


-spec is_enabled( Trsn :: atom(), Mode :: #{ atom() => [_]}, UsrInfo :: _ ) ->
        boolean().

is_enabled( recv, #{ 'Data' := [S] }, _ ) ->
  case string:find( S, "\r\n" ) of
    nomatch -> false;
    _       -> true
  end;

is_enabled( drop_msg, #{ 'Inbox' := [#irc_msg{ command = "001" }] }, _ )    -> true;
is_enabled( drop_msg, #{ 'Inbox' := [#irc_msg{ command = "002" }] }, _ )    -> true;
is_enabled( drop_msg, #{ 'Inbox' := [#irc_msg{ command = "003" }] }, _ )    -> true;
is_enabled( drop_msg, #{ 'Inbox' := [#irc_msg{ command = "004" }] }, _ )    -> true;
is_enabled( drop_msg, #{ 'Inbox' := [#irc_msg{ command = "005" }] }, _ )    -> true;
is_enabled( drop_msg, #{ 'Inbox' := [#irc_msg{ command = "250" }] }, _ )    -> true;
is_enabled( drop_msg, #{ 'Inbox' := [#irc_msg{ command = "251" }] }, _ )    -> true; % RPL_LUSERCLIENT
is_enabled( drop_msg, #{ 'Inbox' := [#irc_msg{ command = "252" }] }, _ )    -> true; % RPL_LUSEROP
is_enabled( drop_msg, #{ 'Inbox' := [#irc_msg{ command = "253" }] }, _ )    -> true; % RPL_LUSERUNKNOWN
is_enabled( drop_msg, #{ 'Inbox' := [#irc_msg{ command = "254" }] }, _ )    -> true; % RPL_LUSERCHANNELS
is_enabled( drop_msg, #{ 'Inbox' := [#irc_msg{ command = "255" }] }, _ )    -> true; % RPL_LUSERME
is_enabled( drop_msg, #{ 'Inbox' := [#irc_msg{ command = "265" }] }, _ )    -> true;
is_enabled( drop_msg, #{ 'Inbox' := [#irc_msg{ command = "266" }] }, _ )    -> true;
is_enabled( drop_msg, #{ 'Inbox' := [#irc_msg{ command = "328" }] }, _ )    -> true;
is_enabled( drop_msg, #{ 'Inbox' := [#irc_msg{ command = "332" }] }, _ )    -> true; % RPL_TOPIC
is_enabled( drop_msg, #{ 'Inbox' := [#irc_msg{ command = "333" }] }, _ )    -> true;
is_enabled( drop_msg, #{ 'Inbox' := [#irc_msg{ command = "366" }] }, _ )    -> true; % RPL_ENDOFNAMES
is_enabled( drop_msg, #{ 'Inbox' := [#irc_msg{ command = "372" }] }, _ )    -> true; % RPL_MOTD
is_enabled( drop_msg, #{ 'Inbox' := [#irc_msg{ command = "375" }] }, _ )    -> true; % RPL_MOTDSTART
is_enabled( drop_msg, #{ 'Inbox' := [#irc_msg{ command = "MODE" }] }, _ )   -> true;
is_enabled( drop_msg, #{ 'Inbox' := [#irc_msg{ command = "NOTICE" }] }, _ ) -> true;

is_enabled( request_connect, #{ 'ConnState' := [connect] }, _ ) ->
  true;

is_enabled( ack_connect, #{ 'ConnState' := [await_connect],
                            'Inbox' := [#irc_msg{ command = "376" }] }, _ ) ->
  true;

is_enabled( ack_connect, #{ 'ConnState' := [await_connect],
                            'Inbox' := [#irc_msg{ command = "422" }] }, _ ) ->
  true;

is_enabled( request_join, #{ 'ConnState' := [join] }, _ ) ->
  true;

is_enabled( ack_join, #{ 'ConnState' := [await_join],
                         'Inbox' := [#irc_msg{ prefix = Prefix, command = "JOIN" }] },
                      #irc_state{ nick_name = NickName } ) ->
  lists:prefix( NickName, get_nick_name( Prefix ) );

is_enabled( privmsg, #{ 'ConnState' := [ready],
                        'Inbox'     := [#irc_msg{ command = "PRIVMSG" }],
                        'UsrState'  := [_] }, _ ) ->
  true;

is_enabled( namereply, #{ 'ConnState' := [ready],
                            'Inbox'     := [#irc_msg{ command = "353" }],
                            'UsrState'  := [_] }, _ ) ->
  true;

is_enabled( join, #{ 'ConnState' := [ready],
                     'Inbox'     := [#irc_msg{ prefix = Prefix, command = "JOIN" }],
                     'UsrState'  := [_] },
                  #irc_state{ nick_name = NickName } ) ->
  not lists:prefix( NickName, get_nick_name( Prefix ) );

is_enabled( part, #{ 'ConnState' := [ready],
                     'Inbox'     := [#irc_msg{ command = "PART" }],
                     'UsrState'  := [_] }, _ ) ->
  true;

is_enabled( part, #{ 'ConnState' := [ready],
                     'Inbox'     := [#irc_msg{ command = "QUIT" }],
                     'UsrState'  := [_] }, _ ) ->
  true;

is_enabled( part, #{ 'ConnState' := [ready],
                      'Inbox'     := [#irc_msg{ command = "KICK",
                                                arg_lst = [_, U |_] }],
                      'UsrState'  := [_] },
                  #irc_state{ nick_name = NickName } ) ->

  case U of
    NickName -> false;
    _        -> true
  end;

is_enabled( error, #{ 'ConnState' := [ready],
                      'Inbox'     := [#irc_msg{ command = "KICK",
                                                arg_lst = [_, U |_] }],
                      'UsrState'  := [_] },
                   #irc_state{ nick_name = NickName } ) ->

  case U of
    NickName -> true;
    _        -> false
  end;

is_enabled( error, #{ 'ConnState' := [_],
                      'Inbox'     := [#irc_msg{ command = [$4, _, _] }],
                      'UsrState'  := [_] }, _ ) ->
  true;

is_enabled( error, #{ 'ConnState' := [_],
                      'Inbox'     := [#irc_msg{ command = [$5, _, _] }],
                      'UsrState'  := [_] }, _ ) ->
  true;

is_enabled( _Trsn, _Mode, _UsrInfo ) -> false.


-spec fire( Trsn :: atom(), Mode :: #{ atom() => [_] }, UsrInfo :: _ ) ->
            abort | {produce, #{ atom() => [_] }}.

fire( recv, #{ 'Data' := [S] }, _ ) ->
  [Prefix, Suffix] = string:split( S, "\r\n" ),
  Msg = parse_msg( Prefix ),
  {produce, #{ 'Data' => [Suffix], 'Inbox' => [Msg] }};

% fire( drop_msg, #{ 'Inbox' := [Msg] }, _ ) ->
%   io:format( "~p~n", [Msg] ),
%   {produce, #{}};

fire( drop_msg, _, _ ) ->
  {produce, #{}};

fire( request_connect, _, _ ) ->
  {produce, #{ 'ConnState' => [await_connect], 'Outbox' => [connect] }};

fire( ack_connect, _, _ ) ->
  error_logger:info_report( [{status, ack_connect}] ),
  {produce, #{ 'ConnState' => [join] }};

fire( request_join, _, _ ) ->
  {produce, #{ 'ConnState' => [await_join], 'Outbox' => [join] }};

fire( ack_join, _, _ ) ->
  error_logger:info_report( [{status, ack_join}] ),
  {produce, #{ 'ConnState' => [ready] }};

fire( privmsg, #{ 'ConnState' := [ready],
                  'Inbox'     := [Msg],
                  'UsrState'  := [UsrState] },
               #irc_state{ channel   = Channel,
                           nick_name = NickName,
                           usr_mod   = UsrMod } ) ->

  #irc_msg{ prefix  = Prefix,
            arg_lst = [Receiver, Content] } = Msg,

  Mode =
    case Receiver of
      NickName -> private;
      Channel  -> public
    end,

  Sender = get_nick_name( Prefix ),

  Target =
    case Mode of
      private -> Sender;
      public  -> Channel
    end,

  case UsrMod:handle_privmsg( Mode, Sender, Content, UsrState ) of

    {noreply, UsrState1} ->

      error_logger:info_report( [{status, recv_privmsg},
                                 {sender, Sender},
                                 {content, Content},
                                 {react, noreply}] ),

      {produce, #{ 'ConnState' => [ready],
                   'UsrState'  => [UsrState1] }};

    {reply, Reply, UsrState1} ->

      error_logger:info_report( [{status, recv_privmsg},
                                 {sender, Sender},
                                 {content, Content},
                                 {react, reply}] ),

      OutboxLst = [{privmsg, Target, S} || S <- string:tokens( Reply, "\n" )],

      {produce, #{ 'ConnState' => [ready],
                   'UsrState'  => [UsrState1],
                   'Outbox'    => OutboxLst }};

    {spawn, F, UsrState1} ->

      Self = self(),

      G =
        fun() ->
          Reply = F(),
          lists:foreach(
            fun( S ) -> gen_pnet:cast( Self, {privmsg, Target, S} ) end,
            string:tokens( Reply, "\n" ) )
        end,

      _Pid = spawn_link( G ),

      {produce, #{ 'ConnState' => [ready],
                   'UsrState'  => [UsrState1] }}

  end;

fire( namereply, #{ 'ConnState' := [ready],
                    'Inbox'     := [#irc_msg{ arg_lst = [_, _, _, S] }],
                    'UsrState'  := [UsrState] },
                 #irc_state{ usr_mod   = UsrMod,
                             nick_name = NickName } ) ->

  F =
    fun
      ( [$@|Z] ) -> Z;
      ( [$+|Z] ) -> Z;
      ( Z )      -> Z
    end,

  P = fun( N ) ->
        N =/= NickName
      end,

  % remove name prefixes
  UsrLst1 = [F( X ) || X <- string:split( S, " ", all )],

  UsrLst2 = lists:filter( P, UsrLst1 ),

  error_logger:info_report( [{status, join},
                             {user_lst, UsrLst2} ] ),

  UsrState1 = lists:foldl( fun( U, State ) -> UsrMod:handle_join( U, State ) end,
                           UsrState, UsrLst2 ),

  {produce, #{ 'ConnState' => [ready],
               'UsrState'  => [UsrState1] }};

fire( join, #{ 'ConnState' := [ready],
               'Inbox'     := [#irc_msg{ prefix = Prefix }],
               'UsrState'  := [UsrState] },
            #irc_state{ usr_mod   = UsrMod } ) ->

  U = get_nick_name( Prefix ),

  error_logger:info_report( [{status, join},
                             {user_lst, [U]} ] ),

  UsrState1 = UsrMod:handle_join( U, UsrState ),

  {produce, #{ 'ConnState' => [ready], 'UsrState' => [UsrState1] }};


fire( part, #{ 'ConnState' := [ready],
               'Inbox'     := [#irc_msg{ command = "KICK",
                                         arg_lst = [_, U |_] }],
               'UsrState'  := [UsrState] },
            #irc_state{ usr_mod   = UsrMod } ) ->

  error_logger:info_report( [{status, part},
                             {user_lst, [U]} ] ),

  UsrState1 = UsrMod:handle_part( U, UsrState ),

  {produce, #{ 'ConnState' => [ready], 'UsrState' => [UsrState1] }};


fire( part, #{ 'ConnState' := [ready],
               'Inbox'     := [Msg],
               'UsrState'  := [UsrState] },
            #irc_state{ usr_mod   = UsrMod } ) ->

  #irc_msg{ prefix = Prefix } = Msg,

  U = get_nick_name( Prefix ),

  error_logger:info_report( [{status, part},
                             {user_lst, [U]} ] ),

  UsrState1 = UsrMod:handle_part( U, UsrState ),

  {produce, #{ 'ConnState' => [ready], 'UsrState' => [UsrState1] }};

fire( error, #{ 'ConnState' := [_],
                'Inbox'     := [Msg],
                'UsrState'  := [_] }, _ ) ->
  
  #irc_msg{ prefix  = Prefix,
            command = Command,
            arg_lst = ArgLst } = Msg,

  error_logger:info_report( [{status, error},
                             {prefix, Prefix},
                             {command, Command},
                             {arg_lst, ArgLst} ] ),

  error( Msg ).




%%====================================================================
%% Internal functions
%%====================================================================

parse_msg( S ) ->
  parse_msg( prefix, S, #irc_msg{} ).

parse_msg( prefix, [$:|S], Msg ) ->
  [Prefix, Rest] = string:split( S, " " ),
  parse_msg( command, Rest, Msg#irc_msg{ prefix = Prefix } );

parse_msg( prefix, S, Msg ) ->
  parse_msg( command, S, Msg );

parse_msg( command, S, Msg ) ->
  [Command, Rest] = string:split( S, " " ),
  parse_msg( arg_lst, Rest, Msg#irc_msg{ command = Command } );

parse_msg( arg_lst, [$:|S], Msg = #irc_msg{ arg_lst = ArgLst } ) ->
  Msg#irc_msg{ arg_lst = ArgLst++[S] };

parse_msg( arg_lst, S, Msg = #irc_msg{ arg_lst = ArgLst } ) ->
  case string:split( S, " " ) of
    [LastArg]   -> Msg#irc_msg{ arg_lst = ArgLst++[LastArg] };
    [Arg, Rest] -> parse_msg( arg_lst, Rest, Msg#irc_msg{ arg_lst = ArgLst++[Arg]} )
  end.

get_nick_name( Prefix ) ->
  [Prefix1|_] = string:split( Prefix, "@" ),
  [Prefix2|_] = string:split( Prefix1, "!" ),
  Prefix2.