# !/bin/bash 
  
# A simple bash script to move up to desired directory level directly


export INPUTFILE=$(cd "$(dirname "$1")"; pwd -P)/$(basename "$1")
erl -noshell -pa _build/default/lib/parking_lot/ebin/ -eval "application:start(parking_lot), parking_lot_execute_commands:execute_commands()" -run init stop