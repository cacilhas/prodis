% -*- Prolog -*-
% @copyright ©2015, Rodrigo Cacilhας <batalema@cacilhas.info>
:- module(listen, [start_listening/0]).
:- [library(settings), library(readutil), logging, redis_server, operators].

start_listening :-
    tcp_socket(Server),
    tcp_setopt(Server, reuseaddr),
    setting(prodis:host, Host),
    setting(prodis:port, Port),
    tcp_bind(Server, Host:Port),
    tcp_listen(Server, 10),
    log(debug, 'listening ~w:~w', [Host, Port]),
    mainloop(Server).


mainloop(Server) :-
    log(debug, 'accepting connections', []),
    tcp_accept(Server, Slave, Peer),
    log(debug, 'connection from ~w', [Peer]),
    tcp_open_socket(Slave, Stream),
    thread_create(look_to([peer-Peer, stream-Stream]), _,
                  [detached(true),
                   at_exit(close(Stream, [force(true)]))]),
    mainloop(Server).


%%------------------------------------------------------------------------------
look_to([peer-Peer, stream-Stream]) :-
    % TODO: authentication,
    next_command(Stream, Command),
    continue_processing([peer-Peer, stream-Stream], Command).


%% Finish thread and close conection
continue_processing([peer-Peer |_], end_of_file) :- !,
    log(warn, '(~w) disconnected', [Peer]).

continue_processing([peer-Peer, stream-Stream], [Command|Params]) :- !,
    string_upper(Command, RealCommand),
    log(debug, '(~w) received command ~w, params: ~w',
        [Peer, RealCommand, Params]),
    redis_process_command(RealCommand, Params, Response),
    respond([peer-Peer, stream-Stream], Response).

continue_processing(State, Command) :-
    string(Command), !,
    continue_processing(State, [Command]).


%% Close connection
respond([peer-Peer, stream-Stream], `+CLOSED\r\n`) :- !,
    log(debug, '(~w) closing connection', [Peer]),
    write(Stream, "+CLOSE\r\n").

%% Respond and wait for commands
respond([peer-Peer, stream-Stream], Response) :- !,
    string_codes(R1, Response),
    log(debug, '(~w) responding: ~q', [Peer, R1]),
    write(Stream, R1),
    flush_output(Stream),
    look_to([peer-Peer, stream-Stream]).


%%------------------------------------------------------------------------------
next_command(Stream, R) :-
    read_line_to_codes(Stream, Term),
    process_codes(Stream, Term, R).


%% No data
process_codes(_Stream, end_of_file, end_of_file) :- !.


%% Array
process_codes(Stream, [0'*|Param], R) :- !,
    atom_codes(Aux, Param),
    atom_to_term(Aux, Length, []),
    get_redis_array(Stream, Length, R).


%% Integer
process_codes(_Stream, [0':|Param], R) :- !,
    atom_codes(Aux, Param),
    atom_to_term(Aux, R, []).


%% Simple string
process_codes(_Stream, [0'+|Param], R) :- !,
    string_codes(R, Param).


%% Bulk string
process_codes(Stream, [0'$, 0'0], "") :- !,
    read_line_to_codes(Stream, []).

process_codes(Stream, [0'$|Param], R) :- !,
    atom_codes(Aux, Param),
    atom_to_term(Aux, Length, []),
    get_redis_bulk_string(Stream, Length, [], R).


%% Unknown
process_codes(_Stream, Data, R) :-
    atom_codes(R, Data).


get_redis_bulk_string(_Stream, N, _, null) :- N < 0, !.

get_redis_bulk_string(_Stream, 0, Acc, R) :- !,
    string_codes(R, Acc).

get_redis_bulk_string(Stream, N, Acc, R) :- !,
    read_line_to_codes(Stream, Code, [13, 10]),
    append(Acc, Code, Acc1),
    length(Acc1, Length),
    bulk_string_next_step(Stream, N, Length, Acc1, R).

bulk_string_next_step(Stream, N, Length, Acc, R) :-
    Length < N, !,
    get_redis_bulk_string(Stream, N, Acc, R).

bulk_string_next_step(Stream, N, _, Acc, R) :- !,
    slice(Acc, N, Acc1),
    get_redis_bulk_string(Stream, 0, Acc1, R).


get_redis_array(_, N, _) :- N < 0, !, fail.

get_redis_array(_, 0, []) :- !.

get_redis_array(Stream, N, [R1 | Rest]) :- !,
    next_command(Stream, R1),
    N1 is N - 1,
    get_redis_array(Stream, N1, Rest).

% vim:set et:syntax=prolog
