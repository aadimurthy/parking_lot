-module(parking_lot_SUITE).
-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         all/0,
         create_parking_lot_test/1,
         test_parking_command/1,
         test_reg_number_grouping/1,
         test_slot_number_grouping/1,
         test_command_not_support/1,
         test_leave_command/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,120}}].

init_per_suite(Config) ->
    application:ensure_all_started(parking_lot),
    Config.

end_per_suite(_Config) ->
    application:stop(parking_lot),
    ok.

init_per_testcase(_, Config) ->
    parking_lot_execute_commands:execute(["create_parking_lot", 5]),
  Config.

all() ->
[create_parking_lot_test, test_parking_command, test_leave_command, test_reg_number_grouping, test_slot_number_grouping,
                          test_command_not_support].


create_parking_lot_test(_)->
    ?assertEqual(1, parking_lot_free_slot_server:get_free_slot()),
    ?assertEqual(2, parking_lot_free_slot_server:get_free_slot()),
    ?assertEqual(3, parking_lot_free_slot_server:get_free_slot()),
    ?assertEqual(4, parking_lot_free_slot_server:get_free_slot()),
    ?assertEqual(5, parking_lot_free_slot_server:get_free_slot()),
    ?assertEqual('$end_of_table', parking_lot_free_slot_server:get_free_slot()),
    ok.

test_parking_command(_)->
    parking_lot_execute_commands:execute(["park", "KA-01-HH-1234", "White"]),
    parking_lot_execute_commands:execute(["park", "KA-01-HH-9999", "White"]),
    parking_lot_execute_commands:execute(["park", "KA-01-BB-0001", "Black"]),
    parking_lot_execute_commands:execute(["park", "KA-01-HH-2701", "Blue"]),
    ?assertEqual(
      [[[
      ["\nSlot No.\tRegistration No\t\tColour",
      "\n", "1", "\t\t",  "KA-01-HH-1234", "\t\t", "White" ],
      "\n", "2", "\t\t",  "KA-01-HH-9999", "\t\t", "White" ],
      "\n", "3", "\t\t",  "KA-01-BB-0001", "\t\t", "Black" ],
      "\n", "4", "\t\t",  "KA-01-HH-2701", "\t\t", "Blue"  ],
                    parking_lot_execute_commands:execute(["status"])),
    parking_lot_execute_commands:execute(["park", "12122323", "Blue"]),
    ?assertEqual("\nSorry, parking lot is full", parking_lot_execute_commands:execute(["park", "234234234342", "Blue"])),
    ok.

test_leave_command(_)->
    parking_lot_execute_commands:execute(["park", "KA-01-HH-1234", "White"]),
    parking_lot_execute_commands:execute(["park", "KA-01-HH-9999", "White"]),
    parking_lot_execute_commands:execute(["park", "KA-01-BB-0001", "Black"]),
    parking_lot_execute_commands:execute(["leave", 2]),
    ?assertEqual(2,parking_lot_free_slot_server:get_free_slot()),
    ok.

test_reg_number_grouping(_)->
    parking_lot_execute_commands:execute(["park", "KA-01-HH-1234", "White"]),
    parking_lot_execute_commands:execute(["park", "KA-01-HH-9999", "White"]),
    parking_lot_execute_commands:execute(["park", "KA-01-BB-0001", "Black"]),
    parking_lot_execute_commands:execute(["park", "KA-01-BB-0002", "White"]),
    ?assertEqual(["\n", ["KA-01-BB-0001"]],
            parking_lot_execute_commands:execute(["registration_numbers_for_cars_with_colour", "Black"])),
    ?assertEqual(["\n", [["KA-01-HH-1234"], ", ", ["KA-01-HH-9999"]], ", ", ["KA-01-BB-0002"]],
            parking_lot_execute_commands:execute(["registration_numbers_for_cars_with_colour", "White"])),
    ?assertEqual("\nNot found", parking_lot_execute_commands:execute(["registration_numbers_for_cars_with_colour", "Red"])),
    ok.

test_slot_number_grouping(_)->
    parking_lot_execute_commands:execute(["park", "KA-01-HH-1234", "White"]),
    parking_lot_execute_commands:execute(["park", "KA-01-HH-9999", "White"]),
    parking_lot_execute_commands:execute(["park", "KA-01-BB-0001", "Black"]),
    parking_lot_execute_commands:execute(["park", "KA-01-BB-0002", "White"]),
    ?assertEqual(["\n", ["1", ", ", "2"], ", ", "4"],
            parking_lot_execute_commands:execute(["slot_numbers_for_cars_with_colour", "White"])),
    ?assertEqual("\nNot found", 
            parking_lot_execute_commands:execute(["slot_number_for_registration_number", "1234"])),
    ?assertEqual(["\n", "4"],
            parking_lot_execute_commands:execute(["slot_number_for_registration_number", "KA-01-BB-0002"])),
    ok.

test_command_not_support(_)->
    parking_lot_execute_commands:execute(["Dummy","Command"]),
    ok.









    


