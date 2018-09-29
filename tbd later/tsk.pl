/*
 * tsk.pl
 *
 * Task management toolbox to do it just with text files,
 * hashtags and line menus.
 *
 * Status = under development
 *
 * Language = SWI Prolog
 *
 * @author Thierry JAUNAY
 * @licence GPL
 * @arg creadate 2018-08-25
 * @arg update 2018-08-25
 *
 * Latest saved version ...
 * @version 1809.XXX
 *
 * ----------
 *
 * Use =
 * -
 * -
 * -
 * - use go_tsk/0
 *
 * Tools = check in the Tools part
 *
 */

%
% MODULES & DATA
% --------------

% test database ...
%    use_module(tsk_db_0).

% ... to customize to dev needs and for example be renamed
%    use_module(tsk_db_1).

% menus management
    use_module(menu_db).

% toolbox for herein used predicates
    use_module(toolbox, [
               get_char_1/1,
               if_empty_default/3,
               list_to_string/2,
               print_matrix/1 ] ).

% cache buffering
    :- dynamic(known/3).

%
% OPEN FILES
% ----------
%

%
% CHECK HASHTAGS
% --------------
%










