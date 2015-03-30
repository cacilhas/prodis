% -*- Prolog -*-
% @copyright ©2015, Rodrigo Cacilhας <batalema@cacilhas.info>
:- module(log_formatter, []).
:- [library(date)].

:- format_predicate('T', format_time(_Arg, _Time)).


format_log(Level, Format, Args, Log) :-
    get_time(Stamp),
    concat('~T [~a] ', Format, Format2),
    concat(Format2, '~n', Format3),
    upcase_atom(Level, Level2),
    Log = format(Format3, [Stamp, Level2 | Args]).


format_time(_Arg, Stamp) :-
    must_be(number, Stamp),
    format_time(current_output, '%FT%T%z', Stamp).

% vim:set et:syntax=prolog
