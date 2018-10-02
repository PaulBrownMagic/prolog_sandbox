/*
 load_menu_db.pl

 @author Paul Brown
 @license MIT

*/

% Get access to the interpreter
:- use_module('data/data_access',
    [ get_data/2
    , load_data/1
    ]).

% Load the data source
:- load_data('data/menu_db_for_test').
