-module(parking_lot_execute_commands).
-export([execute_commands/0]).
-define(INPUTFILE, os:getenv("INPUTFILE")).

execute_commands()->
    CommandList = 
        case ?INPUTFILE of
           false ->
                parking_lot_read_input_file:into_list("file_inputs.txt");
            Value ->
                parking_lot_read_input_file:into_list(Value)
        end,
        lists:foreach(
            fun(Command)-> io:format("~s",[execute(Command)]) end, 
                 CommandList),
        io:format("~s~n",[""]).                

% Commands API
execute(["create_parking_lot", Slots]) ->
    parking_lot_free_slot_server:load_free_slots(Slots);               

execute(["park", RegNumber, Colour]) ->
   FreeSlot = parking_lot_free_slot_server:get_free_slot(),
   parking_lot_allocation_server:allocate(FreeSlot, RegNumber, Colour);

execute(["leave", SlotNumber])->
    parking_lot_allocation_server:remove(SlotNumber);

execute(["status"])->
    parking_lot_allocation_server:status();

execute(["registration_numbers_for_cars_with_colour", Colour]) ->
    parking_lot_allocation_server:group_reg_number_with(Colour);

execute(["slot_numbers_for_cars_with_colour", Colour]) ->
    parking_lot_allocation_server:group_slots_with({colour, Colour});

execute(["slot_number_for_registration_number", RegNumber]) ->
    parking_lot_allocation_server:group_slots_with({reg_number, RegNumber});

execute(_) ->
    "\nnot_supported".




