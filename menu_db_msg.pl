/* <module> menu_db_msg.pl
 *
 * @version 1809.003
 * @licence MIT
 * @copyright Wiserman & Partners
 * @author Thierry JAUNAY
 * @arg creadate 2018/08/30
 * @arg update 2018/09/30
 * @arg comment Messages for menu_db.pl
 * @arg language SWI-Prolog
 *
 * ----------
 *
 * Copyright (c) 2018, Wiserman & Partners
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

% --------
% MESSAGES
% --------

:- multifile prolog:message//1.

prolog:message(menu_not_found(MX) ) -->
    [ '(~w) Menu not found: ~w' - [100, MX] ].

prolog:message(no_item ) -->
    [ '(~w) No menu item found' - [110] ].

prolog:message(no_item(MX) ) -->
    [ '(~w) No menu item found in menu: ~w' - [111, MX] ].

prolog:message(num_choice ) -->
    [ '(~w) Standard numerical choice' - [500] ].

prolog:message(ext_choice ) -->
    [ '(~w) Extended menu choice' - [501] ].

prolog:message(what_choice ) -->
    [ '(~w) What is your choice ... ? ' - [510] ].

prolog:message(chose_again ) -->
    [ '(~w) Please retype your menu choice ...  ' - [511] ].

prolog:message(bad_num_choice(UserChoice) ) -->
    [ '(~w) Bad numerical code choice: \'~w\'' - [550, UserChoice], nl ].

prolog:message(bad_ext_choice(UserChoice) ) -->
    [ '(~w) Bad extended code choice: \'~w\'' - [551, UserChoice], nl ].
