% -*- Prolog -*-
% @copyright ©2015, Rodrigo Cacilhας <batalema@cacilhas.info>
:- module(test_load_parameters, []).
:- [library(settings)].

:- begin_tests(load_parameters, [cleanup(prodis:load_parameters)]).

test(settings, [forall(member([X, Y], [[set_a, ['-s', set_a]],
                                         [set_b, ['-sset_b']],
                                         [set_c, ['--settings', set_c]],
                                         [set_d, ['--settings=set_d']]]))]) :-
    prodis:load_parameters(Y),
    setting(prodis:conffile, X).

test(logfile, [forall(member([X, Y], [[log_a, ['-l', log_a]],
                                        [log_b, ['-llog_b']],
                                        [log_c, ['--log-file', log_c]],
                                        [log_d, ['--log-file=log_d']]]))]) :-
    prodis:load_parameters(Y),
    setting(prodis:logfile, X).

test(unknown, [fail]) :-
    telling(Out),
    tell('/dev/null'),
    (prodis:load_parameters(['-h']) -> tell(Out);
                                         tell(Out), fail).

:- end_tests(load_parameters).

% vim:set et:syntax=prolog
