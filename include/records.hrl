-record(state,
        {addr,
         port,
         type}).

-record(serv_state,
        {state = #state{},
         sock}).
