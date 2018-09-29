/*
 * PceTest.pl
 *
 */

:- use_module(library(pce)).

% xpce_hello_world/0
% The classical Hello World example

xpce_helllo_world :-
    new(P, picture('Hello World')),
    send(P, display, text('Hello World'), point(5,5)),
    send(P, open).


