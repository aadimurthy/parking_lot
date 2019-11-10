-module(parking_lot_execute_commands).
-export([execute_commands/0]).

execute_commands()->
    CommandList = 
        parking_lot_read_input_file:into_list("/Users/aadi/parking_lot/file_inputs.txt"),
        lists:map(
            fun(Command)-> execute(Command)++"\n" end, 
                 CommandList).


execute(["create_parking_lot", Slots]) ->
    parking_lot_free_slot_server:load_free_slots(Slots);               

execute(["park", RegNumber]) ->
   FreeSlot = parking_lot_free_slot_server:get_free_slot(),
   parking_lot_allocation_server:allocate(FreeSlot);

execute(["leave", SlotNumber])->
    parking_lot_allocation_server:remove(SlotNumber);

execute(["status"])->
    "ok";

execute(["slot_numbers_for_cars_with_colour",Colour]) ->
    "ok";

execute(["slot_number_for_registration_number", RegNumber]) ->
    "ok";

execute(_) ->
    "not_supported".




