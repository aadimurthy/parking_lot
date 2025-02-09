-module(parking_lot_allocation_server).
-behaviour(gen_server).

-export([start_link/0,
         allocate/3,
         remove/1,
         group_reg_number_with/1,
         group_slots_with/1,
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
   "\nSorry, parking lot is full";

allocate(SlotNumber, RegNumber, Colour) when is_integer(SlotNumber)->
    gen_server:call(?MODULE, {allocate, {SlotNumber, RegNumber, Colour}});

allocate(_Value, _,  _)->
  "\nParking Failed".
    
%% Synchronous call
remove(SlotNumber)->
    gen_server:call(?MODULE, {make_free, SlotNumber}).

%% Synchronous call
group_reg_number_with(Colour)->
    gen_server:call(?MODULE, {group_reg_number_with, Colour}).

%% Synchronous call
group_slots_with({colour, Colour})->
    gen_server:call(?MODULE, {group_slots_by, {colour, Colour}});

group_slots_with({reg_number, RegNumber})->
    gen_server:call(?MODULE, {group_slots_by, {reg_number, RegNumber}}).

%% Synchronous call
status()->
    gen_server:call(?MODULE, status).


%%% Server functions
init([]) ->
    create_ets_table(),
    {ok, #state{}}.

handle_call({allocate, {SlotNumber, _RegNumber, _Colour} = IData}, _From, State) ->
    allocate_slot(IData),
    {reply, reply("Allocated slot number ", SlotNumber), State};

handle_call({make_free, SlotNumber}, _From, State) ->
    free_slot_from_allocation(SlotNumber),
    {reply, reply("Slot number ", SlotNumber , " is free"), State};

handle_call({group_reg_number_with, Colour}, _From, State) ->
    {ok, RegNumList} = get_reg_numbers_by(Colour),
    {reply, reply_group(RegNumList), State};   

handle_call({group_slots_by, {colour, Colour}}, _From, State) ->
    {ok, Slots} = get_slots_by({colour, Colour}),
    {reply, reply_group(Slots), State};

handle_call({group_slots_by, {reg_number, RegNumber}}, _From, State) ->
    {ok, Slots} = get_slots_by({reg_number, RegNumber}),
    {reply, reply_group(Slots), State};

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

create_ets_table()-> % created order set which is equivalent of Binary search algorithm ()
    ets:new(filled_slots, [ordered_set, named_table]).

allocate_slot({_SlotNumber, _RegNumber, _Colour} = IData)->
    ets:insert(filled_slots, IData).

free_slot_from_allocation(SlotNumber)->
    ets:delete(filled_slots, SlotNumber),
    parking_lot_free_slot_server:add_free_slot(SlotNumber).

get_reg_numbers_by(Colour)->
    {ok, ets:match(filled_slots,{'_', '$1', Colour})}.

get_slots_by({colour, Colour})->
    {ok, ets:match(filled_slots,{'$1', '_', Colour})};

get_slots_by({reg_number, RegNumber})->
    {ok, ets:match(filled_slots,{'$1', RegNumber, '_'})}.

get_status()->
    {ok, ets:tab2list(filled_slots)}.


reply(Message, Value)->
       ["\n", Message, integer_to_list(Value)].
reply(Message1, Value, Message2) ->
       ["\n", Message1, integer_to_list(Value), Message2].


reply_group([])->
    "\nNot found";

reply_group([[Item]]) when is_integer(Item)->
     ["\n", integer_to_list(Item)];

reply_group([Item])->
     ["\n", Item];

reply_group(Items) ->
    reply_group_s(Items, []).

reply_group_s([],Acc) ->
  ["\n", Acc];

reply_group_s([[Item]],Acc) when is_integer(Item)->
    ["\n", Acc, ", ", integer_to_list(Item)];

reply_group_s([Item],Acc)->
    ["\n", Acc, ", ", Item];

reply_group_s([[Item1], [Item2]|T],Acc) when is_integer(Item1) ->
    reply_group_s(T,[integer_to_list(Item1), ", ", integer_to_list(Item2)|Acc]);

reply_group_s([Item1, Item2|T],Acc) ->
   reply_group_s(T,[Item1, ", ", Item2|Acc]).



reply_status(Status)->
    lists:foldl(
        fun({SlotNumber, RegNumber, Colour}, Heading)-> 
           [Heading, "\n", integer_to_list(SlotNumber), "\t\t", RegNumber,"\t\t",Colour]
        end,
       "\nSlot No.\tRegistration No\t\tColour",Status).
