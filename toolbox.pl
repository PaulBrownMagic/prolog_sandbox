/* <module> Toolbox with useful general use predicates
 *
 * Language SWI-Prolog
 *
 * @author Thierry JAUNAY
 * @licence MIT
 * @arg creadate 2018-07-28
 * @arg update 2018-09-24
 *
 * Latest saved version ...
 * @version 1809.011
 *
 *
 * ----------
 *
 * Copyright (c)  2018, Wiserman & Partners
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

 :- module(toolbox, [
               cls/0,
%              format_2/2,
               get_char_1/1,
               known/3,
               pause/1,
               pause/0,
               print_matrix/1,
               if_empty_default/3,
               list_to_string/3,
               list_to_string/2
           ]).

% cache buffering

:- dynamic(known/3).

% cls is det
% Clear screen ANSI Escape code = ESC [ 2 J
% source = https://stackoverflow.com/questions/16908764/clearing-screen-in-swipl-prolog-in-windows

cls :-
    write('\e[2J').

%! get_char_1(X:char) is det
% Get one char + echo i-e same as get_char/1 but with no buffer

get_char_1(X) :-
    get_single_char(X1), % get a single char to avoid buffer mistyping
    char_code(X, X1),    % transform its code into char
    write(X).            % write char

% print_matrix(+[A|Rest]:list) is det
% Print the items from a list

print_matrix([]).
print_matrix([H|T]) :-
    write(H), nl, print_matrix(T).

% pause/0-1 is det
% (+X:atom)
% Wait for sth to be typed

pause(X) :-
    write(X), get_single_char(_), nl.

pause :-
    pause('Press any key to continue ...').

%! if_empty_default(+ToCheck:string, +Default:string, -Result:string)
%! is semidet
% Replace Result by Default if ToCheck is empty

if_empty_default(ToCheck, Default, Result) :-
    ToCheck = ""
    -> Result = Default
    ;  Result = ToCheck.

%! list_to_string(+List:list, +Separator:atom, -String:string)
%! is semidet
% String_concat elements of a list into one string
% Adding a Separator in between each element

list_to_string(List, Separator, String) :-
    atomic_list_concat(List, Separator, Atom),
    atom_string(Atom, String).

%! list_to_string(+List:list, -String:string) is semidet
% String_concat elements of a list into one string

list_to_string(List, String) :-
    atomic_list_concat(List, Atom),
    atom_string(Atom, String).

/*
 * Replacing the initial recursive contribution from @PaulBrownMagic

list_to_string(List, String) :-
    list_to_string(List, "", String).
list_to_string([], String, String).
list_to_string([S|T], Acc, String) :-
    string_concat(Acc, S, Acc2),
    list_to_string(T, Acc2, String).
*/

/* ********** END OF FILE ********** */

























