-module(parking_lot_free_slot_server).

-behaviour(gen_server).

-export([start_link/0,
         get_free_slot/0,
         add_free_slot/1,
         load_free_slots/1]).

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


load_free_slots(Slots)->
    gen_server:call(?MODULE, {load_free_slots, Slots}).  
    

%% Synchronous call
get_free_slot()->
    gen_server:call(?MODULE, get_free_slot).
    
%% Asynchronous call
add_free_slot(SlotNumber)->
    {SlotNumberInt, _ }  = string:to_integer(SlotNumber),
    gen_server:cast(?MODULE, {add_free_slot, SlotNumberInt}).  


%%% Server functions
init([]) ->
    ets:new(free_slots, [ordered_set, named_table]),
    {ok, #state{}}.

handle_call({load_free_slots, Slots}, _From, State) ->
    {SlotsInt, _ } = string:to_integer(Slots),   
    [ets:insert(free_slots,{Slot})||Slot<-lists:seq(1,SlotsInt)],
    {reply, "Created a parking lot with "++Slots++" slots", State};

handle_call(get_free_slot, _From, State) ->
    FreeSlot=ets:first(free_slots),
    ets:delete(free_slots,FreeSlot),
    {reply, integer_to_list(FreeSlot), State}.

handle_cast({add_free_slot, SlotNumber}, State) ->
    ets:insert(free_slots, {SlotNumber}),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions