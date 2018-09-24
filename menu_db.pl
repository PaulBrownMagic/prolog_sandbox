/* <module> Menu management menu_db.pl
 *
 * based on a referential mx_choice_item/3
 * (+MX:Integer, +Choice:Integer, +Name:String)
 *
 * to provide a simple selection with menu number + choice
 * in two ways : numerical or hashtag.
 *
 * Language = SWI Prolog
 *
 * @author Thierry JAUNAY
 * @licence MIT
 * @arg creadate 2018-08-05
 * @arg update 2018-09-24
 *
 * Latest saved version ...
 * @version 1809.045
 *
 * Thx to Paul Brown (@PaulBrownMagic) for his initial contribution
 * from my spaghetti coding to his Prolog fluent coding.
 *
 * Thx to Anne OGBORN (@AnnieTheObscure) for her patience and
 * great advices making me write a much better Prolog style code.
 *
 * ----------
 *
 * Use =
 * - create the menu database (in menu_db_x)
 * - create do_it_std for menu choices
 * - create do_it_ext for extra choices (@tbd hashtags)
 * - use go/0 or ask_menu/1 or do_it/2 versions depending on needs
 *
 * Also added some useful "Internal Tools"
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

% MODULES AND DATA
% ----------------
%

% menu_db module

:- module(menu_db,[
    % cache management
        retract_cached_mx/1,
     % internal tools
        mxw/3,
        mxwm/0,
        mxwc/1,
        mxw_/1,
        exist_mx/1,
        cached_exist_mx/1,
    % messages and errors
        mx_write_message/2,
        mx_write_message/1,
        mx_write_error/2,
    % menu_db program
        cached_make_menu/2,
        do_it/2,
        check_menu/2,
        ask_menu/1,
    % test
        mx_test/1 ] ).

% menu_db settings

:- setting(mx_separator, char, '>', "title menu separator").
:- setting(mx_ext_char, char, '#', "extended menu prefix char").
:- setting(mx_exit_char, char, '.', "menu exit char").

:- load_settings('settings.db').

% ----------
% VOCABULARY
% ----------
% to gain time reading the code ...

% MX:integer = menu number
% MenuX:string = menu numbered MX
% MenuLabel:string = name of the menu
% Separator:char = char in between label and menu
% Str:string = temp variables used for strings
% Subparts:list = list of menu subparts
% UserChoice:char = menu choice selected by user
%    C:integer = when specifically an integer choice
%
% Acronyms
% --------
% mxw stands for mx write

% -----------------
% RIGHTS MANAGEMENT
% -----------------

% etc. ... @tbd later

% --------
% MX TOOLS
% --------
% Misc internal tools for menu_db

% mxw/3
% (+MX:integer, +Choice:integer, +Item:string)
% Test tool to quickly display mx_choice_item/3

mxw(MX, Choice, Item) :-
    findall([MX, Choice, Item], mx_choice_item(MX, Choice, Item), MXs),
    mxw_(MXs).

% mxwm/0
% mx write filtered on MX = -1, to only display codes and messages

mxwm :-
    mxw(-1, _, _).

% mxwc/1
% (+MX:integer)
% Write all in cache about menu number MX

mxwc(MX) :-
    findall([X, MX, MenuX], known(X, MX, MenuX), Xs),
    mxw_(Xs).

% mxw_/1
% ([A|Rest]:list)
% Print list or error message if empty

mxw_([H|T]) :-
    print_matrix([H|T]).
mxw_([]) :-
    nl, mx_write_error(2, 110), nl,
    !,
    false.

% retract_cached_mx/1
% (+MX:integer)
% Purge cache from elements about menu number MX

retract_cached_mx(MX) :-
    % retract cached menu string
        retract(known(menux, MX, _)),
    % retract cached list of standard choices
        retract(known(mx_std_choices, MX, _)).

% exist_mx/1 is semidet
% (+MX:integer)
% Check if menu number is valid
% true if MX found once / false otherwise
% @tbd error messages

exist_mx(MX) :-
    mx_choice_item(MX, _, _),
    !.

cached_exist_mx(MX) :-
    known(menux, MX, _),
    !.

% --------
% MESSAGES
% --------
% Messages and errors management based on mx_menu_item/3
%
% Type: integer = error type
% X: integer = message number in mx_choice_item(-1, X, _)

% mx_write_message/2
% (+Attributes:list, +X:integer)
% Extract and write message number X according to display attributes

mx_write_message(Attributes, X) :-
    mx_choice_item(-1, X, Str),
    ansi_format(Attributes, '~w', [Str]).

mx_write_message(X) :-
    mx_write_message(_, X).

% mx_write_error/2
% (+Type:integer, +X:integer)
% Extract message number X, write it

mx_write_error(Type, X) :-
    mx_write_message([fg(red)], Type),
    mx_write_message([fg(red)], X).

% ------
% LABELS
% ------

% make_menu_label/3
% (+MX:integer, -MenuLabel:string, +Separator:string)

make_menu_label(MX, MenuLabel, Separator) :-
% Make MenuLabel including suffix separator
    % Replace menu label by default one if empty
        mx_label(MX, MenuLabel1),
        mx_label(-1, Default),
        if_empty_default(MenuLabel1, Default, MenuLabel2),
    % Replace separator by DefaultSeparator if empty
        setting(mx_separator, DefaultSeparator),
        if_empty_default(Separator, DefaultSeparator, Separator1),
    % Make MenuLabel string including its suffix separator
        format(atom(A), "~w~w ", [MenuLabel2, Separator1]),
        atom_string(A, MenuLabel).

% make_menu_label/2
% (+MX:integer, -MenuLabel:string)

make_menu_label(MX, MenuLabel) :-
% Make MenuLabel with default suffix separator
    make_menu_label(MX, MenuLabel, _).

% ------------
% MAKING MENUS
% ------------
% PS: I could add horizontal / vertical display options
% but the idea is more to get a help style menu fitting on one line

% make_menu_list/2
% (+MX:integer, -Xs:list)
% Extract from mx_choice_item/3 the list XS of [X|Y] menu items
% where X is the choice and Y the menu item name
% Error if Xs is empty (no menu item)

make_menu_list(MX, Xs) :-
    findall([X, Y], mx_choice_item(MX, X, Y), Xs),
    make_menu_list_(Xs).

make_menu_list_([]) :-
    nl, mx_write_error(2, 110), nl,
    !,
    false.

make_menu_list_(_).

% format_subparts/2
% (+XS:list, -Subparts:list)
% Put menu list into subparts ["0=Back ", "1=Option "] ready for
% joining ++ Thx to @PaulBrownMagic

format_subparts([], []).
% base case with empty lists

format_subparts([X|Xs], [SubpartHead|SubpartTrail]) :-
     % concatenates menu choice and name from X to A
         format(atom(A), "~w=~w ", X),
     % atom A to string SubpartHead
         atom_string(A, SubpartHead),
     % recursion with the trail
         format_subparts(Xs, SubpartTrail).

% make_menu/2
% (+MX:integer, -MenuX:string)
% Check = false if no number for the menu
% Make MenuX as a string = Label + Separator + Subparts
% Check = false if no number for the menu

make_menu(MX, _) :-
    \+ exist_mx(MX),
    !,
    false.

make_menu(MX, MenuX) :-
    % extract subparts needed to build the menu string + check OK
        make_menu_list(MX, Xs),
    % extract menu label (replace by defaut if none)
        make_menu_label(MX, MenuLabel),
    % build the list of items ready for joining
        format_subparts(Xs, Subparts),
    % make the menu line MenuX
        list_to_string([MenuLabel|Subparts], MenuX).

% cached_make_menu/2
% (+MX:integer, -MenuX:string)
% Caching optimization on MenuX string
% Check = false if no number for the menu

cached_make_menu(MX, MenuX) :-
    known(menux, MX, MenuX),
    !.

cached_make_menu(MX, MenuX) :-
    make_menu(MX, MenuX),
    asserta(known(menux, MX, MenuX)).

% -----
% DO_IT
% -----
% Program execution based on menu selection by user
% = to adapt depending on program needs

do_it(MX, _) :-
    \+ exist_mx(MX),
    !,
    false.

do_it(MX, UserChoice) :-
    nl, writeln('TDB - replace by real do_it/2'),
    format('(Menu: ~w / Choice: ~w)~n~n', [MX, UserChoice]).

% ---------------------
% MANAGING MENU CHOICES
% ---------------------
% Check choices / errors before launching do_it

% make_std_choices/2
% (MX:integer, Choices:list)
% Make the list of valid Choices for menu number MX

make_std_choices(MX, _) :-
    \+ exist_mx(MX),
    !,
    false.

make_std_choices(MX, Choices) :-
    findall(Choice, mx_choice_item(MX, Choice, _), Choices).

% cached_std_choices/2
% (MX:integer, Choices:list)
% Caching optimization on Choices list
% Known or added to be known

cached_std_choices(MX, Choices) :-
    known(mx_std_choices, MX, Choices), !.

cached_std_choices(MX, Choices) :-
    make_std_choices(MX, Choices),
    asserta(known(mx_std_choices, MX, Choices)).

% is_std_choice/2
% (MX:integer, C:integer)
% Check if C is a valid menu standard choice

is_std_choice(MX, C) :-
    % true if C appears once
    cached_std_choices(MX, Choices),
    member(C, Choices).

% mx_choice_error/1
% (UserChoice:Char) can be either num or alpha
% Display the error message on num or alpha menu choice error

mx_choice_error(UserChoice) :-
% error message on bad num menu choice
    number(UserChoice),
    !,
    nl, ansi_format([fg(red)],'"~d" ', [UserChoice]),
    mx_write_error(1, 550), nl,
    mx_write_message([fg(blue)], 511).

mx_choice_error(UserChoice) :-
% error message on bad alpha menu choice
    nl, ansi_format([fg(red)],'"~w" ', [UserChoice]),
    mx_write_error(1, 551), nl,
    mx_write_message([fg(blue)], 511).

% check_menu/2
% (MX:integer, UserChoice:char)
% Check choices exit / standard / extended / others
% Manage errors

check_menu(_, UserChoice) :-
% if mx_exit_char then exit
    setting(mx_exit_char, UserChoice),
    !.

check_menu(MX, UserChoice) :-
% if extended menu choice then stop searching and do it
    setting(mx_ext_char, UserChoice),
    !,
    do_it(MX, UserChoice).

check_menu(MX, UserChoice) :-
% if num choice and valid then stop searching and do it
    atom_number(UserChoice, C),
    is_std_choice(MX, C),
    !,
    do_it(MX, C).

check_menu(MX, UserChoice) :-
% if num choice and not valid choice
% then error message, stop searching and false to repeat
    atom_number(UserChoice, C),
    \+ is_std_choice(MX, C),
    mx_choice_error(C),
    !,
    false.

check_menu(_, UserChoice) :-
% Latest check ending by a bad choice
% as neither exit or std choice or extended
% then error message, stop searching and false to repeat
    mx_choice_error(UserChoice),
    !,
    false.

% ---------
% GO / MENU
% ---------
% Launch program with go/0 or ask_menu/1

% ask_menu/1
% (MX:integer)
% Display menu number MX, ask / control and launch choices

ask_menu(MX) :-
    \+ exist_mx(MX),
    !,
    false.

ask_menu(MX) :-
    % make and display menu
        cached_make_menu(MX, MenuX),
        write(MenuX),
    % ask choice and repeat until valid choice
        nl, mx_write_message([fg(blue)],510),
        repeat,
        (   get_char_1(UserChoice),
            check_menu(MX, UserChoice),
            ! )
        ; !.

go :-
    cls,
    ask_menu(1).

% ---------------
% TEST CHECK-LIST
% ---------------

%% Typical choices to test with mx_test/2 and ask_menu/2,
% once menu_db_0 loaded :
%
% MX= 1 / num choice 1 (all is fine)
% MX = 1 / num choice 5 (not existing choice)
% MX = 1 / ext choice = # (alpha extended choice)
% MX = 1 / ext choice = a (non existing alpha extended choice)
% MX = 3 (non existing menu = with label but no items)
%

mx_test(MX) :-
     ask_menu(MX).

/* ********** END OF FILE ********** */
































