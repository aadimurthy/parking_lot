-module(parking_lot_allocation_server).

-behaviour(gen_server).

-export([start_link/0,
         allocate/1,
         remove/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Synchronous call
allocate(SlotNumber)->
    gen_server:call(?MODULE, {allocate,SlotNumber}).
    
%% Synchronous call
remove(SlotNumber)->
    gen_server:call(?MODULE, {make_free, SlotNumber}).


%%% Server functions
init([]) ->
    ets:new(filled_slots, [ordered_set, named_table]),
    {ok, #state{}}.

handle_call({allocate, SlotNumber}, _From, State) ->
    ets:insert(filled_slots,{SlotNumber}),
    {reply, ok, State};

handle_call({make_free, SlotNumber}, _From, State) ->
    ets:delete(filled_slots,{SlotNumber}),
    parking_lot_free_slot_server:add_free_slot(SlotNumber),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions