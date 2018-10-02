:- module(data_access,
    [ get_data/2
    , load_data/1
    ]
).

/*

Interface to data and data representation. Prevents changes in how
the data is stored from effecting the rest of the application.

@author Paul Brown
@license MIT
*/

% Using frames, change this to
% accomodate other data representations
:- use_module(frames,
    [ get_frame/2 as get_data
    , load_data/1
    ]
).
