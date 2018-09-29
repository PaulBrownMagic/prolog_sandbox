/* xpce_examples.pl
 *
 * Examples while discovering XPCE
 *
 * to provide a simple selection with menu number + choice
 * in two ways : numerical or hashtag.
 *
 * @version 1809.004
 * @licence MIT
 * @copyright Wiserman & Partners
 * @author Thierry JAUNAY
 * @arg creadate 2018/09/28
 * @arg update 2018/09/29
 * @arg comment xpce_examples.pl - examples while discovering XPCE
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

:- use_module(library(pce)).

% xpce_hello_world/0
% The classical Hello World example

xpce_helllo_world :-
    new(P, picture('Hello World')),
    send(P, display, text('Hello World'), point(5,5)),
    send(P, open).


