# gen_ircclient
###### A scaffold for Erlang IRC bots.

[![hex.pm](https://img.shields.io/hexpm/v/gen_ircclient.svg?style=flat)](https://hex.pm/packages/gen_ircclient) [![Build Status](https://travis-ci.org/joergen7/gen_ircclient.svg?branch=master)](https://travis-ci.org/joergen7/gen_ircclient)

## IRC Client Behaviour

```erlang
-callback init( Arg :: _ ) -> State :: _.
```

```erlang
-callback handle_privmsg( Mode :: private | public, Sender :: string(), Content :: string(), State :: _ ) ->
              {noreply, NewState :: _}
            | {reply, Reply :: string(), NewState :: _}
            | {spawn, F :: fun( () -> string() ), NewState :: _}.
```

```erlang
-callback handle_join( User :: string(), State :: _ ) -> _.
```

```erlang
-callback handle_part( User :: string(), State :: _ ) -> _.
```

## Example Bot

```erlang
-module( example_bot ).

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
```

## Usage

```erlang
Server   = "irc.freenode.net",
Port     = 6667,
NickName = "adam0815",
UserName = "adam",
RealName = "Adam Canopy".
```

```erlang
ConnInfo = {conn_info, Server, Port, NickName, UserName, RealName},
Channel = "#botwar",
UsrMod  = abot.
```

```erlang
gen_ircclient:start_link( ConnInfo, Channel, UsrMod, [] ).
```

## System Requirements

- [Erlang](http://www.erlang.org/) OTP 18.0 or higher
- [Rebar3](https://www.rebar3.org/) 3.0.0 or higher

## Authors

- Jörgen Brandt ([@joergen7](https://github.com/joergen7/)) [joergen.brandt@onlinehome.de](mailto:joergen.brandt@onlinehome.de)

## License

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html)