# !/bin/bash 
  

if [ $# -lt "1" ]; then
        echo "Usage:  $0 <fileName>   (eg: ./bin/parking_lot.sh file_inputs.txt)"
        exit 1;
fi

export INPUTFILE=$(cd "$(dirname "$1")"; pwd -P)/$(basename "$1")
erl -noshell -pa _build/default/lib/parking_lot/ebin/ -eval "application:start(parking_lot), parking_lot_execute_commands:execute_commands()" -run init stop