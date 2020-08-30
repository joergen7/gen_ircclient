%% -*- erlang -*-
%%
%% A scaffold for Erlang IRC bots.
%%
%% Copyright 2018-2020 Jörgen Brandt
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
%% @copyright 2018-2020 Jörgen Brandt
%%
%% @end
%% -------------------------------------------------------------------


%%====================================================================
%% Records
%%====================================================================

-record( irc_msg, {prefix, command, arg_lst = []} ).
-record( conn_info, {server, port, nick_name, user_name, real_name} ).

%%====================================================================
%% Types
%%====================================================================

-type msg_mode() :: private
                  | public.