/*
 * load.pl for tsk.pl
 *
 * Language SWI Prolog
 *
 * Created 2018-08-25
 * Last update 2018-08-25
 *
 * Loads the needed modules for tsk.pl
 *
 */

use_module(toolbox, [
                get_char_1/1,
                if_empty_default/3,
                list_to_string/2,
                print_matrix/1 ] ).

use_module(menu_db).
use_module(menu_db0).



