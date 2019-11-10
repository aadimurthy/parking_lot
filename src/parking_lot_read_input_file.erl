
-module(parking_lot_read_input_file).
-export( [into_list/1] ).
 
into_list( File ) ->
        {ok, IO} = file:open( File, [read] ),
        %Line = string:strip(io:get_line(IO, ''), right, $\n),
        into_list(io:get_line(IO, ''), IO, [] ).
 
 
into_list( eof, _IO, Acc ) -> lists:reverse( Acc );
into_list( {error, _Error}, _IO, Acc ) -> lists:reverse( Acc );
into_list( Line, IO, Acc ) -> 
        LineTmp = string:strip(Line, right, $\n),
        StringTokens = string:tokens(LineTmp, " "),
        into_list(io:get_line(IO, ''), IO, [StringTokens | Acc] ).
