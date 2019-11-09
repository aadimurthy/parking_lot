%%%-------------------------------------------------------------------
%% @doc parking_lot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(parking_lot_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},

    Children = [#{ id  => parking_lot_ticket_server, 
    start    => {parking_lot_free_slot_server, start_link, []},
    restart  => permanent,
    shutdown => 5000,
    type     => worker,
    modules  => [parking_lot_ticket_server]
    },
    #{ id  => parking_lot_allocation_server, 
    start    => {parking_lot_allocation_server, start_link, []},
    restart  => permanent,
    shutdown => 5000,
    type     => worker,
    modules  => [parking_lot_allocation_server]
    }
    ],

    {ok, {SupFlags, Children}}.

%% internal functions
