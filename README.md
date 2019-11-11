## Parking lot using Erlang Programming (Actor model)

##### Note:  We need a Erlang Installation to build and run this application.

One can install the erlang on his respective OS by downloading the package from https://www.erlang-solutions.com/resources/download.html

For eg: 

#### Erlang Installation on Linux (Ubuntu)

Follow the step1 and step2 from https://tecadmin.net/install-erlang-on-ubuntu/


### To build 

```
 ./bin/setup.sh
```

### To run 
```
./bin/parking_lot.sh  file_inputs.txt
```
or
 ```              
./bin/parking_lot.sh  /Users/aadi/parking_lot/file_inputs.txt
```


### Functional testing and code coverage 

``` 
./bin/run_functional_tests​.sh
``` 

This application uses below Erlang programming OTP behaviours/features 
#### Erlang Supervisor 
Supervisor behavior module provides a supervisor, a process that supervises other processes called child processes like gen_server

A supervisor is responsible for starting, stopping and monitoring its child processes. The basic idea of a supervisor is that it should keep its child processes alive and make our application as fault tolerant one 

#### Erlang GenServer
In an Erlang application, usually we have a number of services communicating to process the incoming requests from the clients. Most of the time these services are implemented by writing gen_server code

Gen_server behavior module provides the client-server relation using synchronous and asynchronous message passing.


 
#### Erlang ETS
ETS is a robust in-memory store for Elixir and Erlang objects that comes included. ETS is capable of storing large amounts of data and offers constant time data access.

Tables in ETS are created and owned by individual processes. When an owner process terminates, its tables are destroyed. You can have as many ETS table as you want, the only limit is the server memory


#### How this application work ?

Whenever vehicle arrives, it gets the next available and nearest free slot from **parking_lot_free_slot_server** process and allocation is done in the **parking_lot_allocation_server** 

**parking_lot_free_slot_server** maintains the all the free slot information and serves the requests synchronously to avoid the race condition(Persuming we can have more clients).

**parking_lot_allocation_server** is the main process and maintains the all the occupied slots and vehical information and also it notifies the free_slot process whenever vehicle leaves. 


#### Future enhancements 

1) Create a very large pool of **parking_lot_allocation_server** processes to get higher performance by using the all the cores of system and very tiny Erlang's light weighted  processes  
2) Switch from ETS to Mnesia for data persistence
3) Off load free slot code to different module and also one could able to write cusom moduel to inject his own algorithm for sot allocation  
4) Dockerize the service. 
   


