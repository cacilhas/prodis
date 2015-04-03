% -*- Prolog -*-
% @copyright ©2015, Rodrigo Cacilhας <batalema@cacilhas.info>
:- module(test_settings, []).
:- [library(settings)].

:- begin_tests(prodis_settings).

test(settings, forall(member(X-Y, [host-localhost,
                                     port-5000,
                                     cache-'/tmp/cache',
                                     cachedelay-300,
                                     logfile-'/dev/null',
                                     loglevel-error,
                                     password-["testpass"],
                                     conffile-'settings/test.pro']))) :-
     setting(prodis:X, Y).

:- end_tests(prodis_settings).

% vim:set et:syntax=prolog
