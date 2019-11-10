-module(parking_lot_allocation_server).
-behaviour(gen_server).

-export([start_link/0,
         allocate/3,
         remove/1,
         status/0]).

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
allocate('$end_of_table', _,  _)->
   "Sorry, parking lot is full";

allocate(SlotNumber, RegNumber, Colour)->
    gen_server:call(?MODULE, {allocate, {SlotNumber, RegNumber, Colour}}).
    
%% Synchronous call
remove(SlotNumber)->
    gen_server:call(?MODULE, {make_free, SlotNumber}).

%% Synchronous call
status()->
    gen_server:call(?MODULE, status).


%%% Server functions
init([]) ->
    create_ets_table(),
    {ok, #state{}}.

handle_call({allocate, {SlotNumber, RegNumber, Colour} = IData}, _From, State) ->
    allocate_slot(IData),
    {reply, reply("Allocated slot number ", SlotNumber), State};

handle_call({make_free, SlotNumber}, _From, State) ->
    free_slot_from_allocation(SlotNumber),
    {reply, reply("Slot number ", SlotNumber , " is free"), State};

handle_call(status, _From, State) ->
    {ok, Status} = get_status(),
    {reply, reply_status(Status), State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
create_ets_table()->
    ets:new(filled_slots, [ordered_set, named_table]).

allocate_slot({SlotNumber, RegNumber, Colour} = IData)->
    ets:insert(filled_slots, IData).

free_slot_from_allocation(SlotNumber)->
    ets:delete(filled_slots, SlotNumber),
    parking_lot_free_slot_server:add_free_slot(SlotNumber).

get_status()->
    {ok, ets:tab2list(filled_slots)}.


reply(Message, Value)->
        Message ++ integer_to_list(Value).
reply(Message1, Value, Message2) ->
        Message1 ++ integer_to_list(Value) ++ Message2.

reply_status(Status)->
    lists:foldl(
        fun({SlotNumber, RegNumber, Colour}=Row, Heading)-> 
           Heading++integer_to_list(SlotNumber)++"\t"++RegNumber++"\t"++Colour++"\n"
        end,
       "Slot No.\tRegistration No\tColour\n",Status).
