:- module(frames,
    [ get_frame/2
    ]
).
/** <module> Frames

This is a query engine to a frame-based knowledge base.
It allows the knowledge based to be queried.

@author Paul Brown
@licence MIT
*/

%! get_frame(+Name, ?Slots) is semidet.
%  Query the data to find the slots
%
%  @arg Name A unique identifier of a frame
%  @arg Slots A list of the thing's attributes or single Attr=Value
get_frame(Name, Slots) :-
    frame(Name, Slots). % Request is for all slots
get_frame(Name, Required) :-
    \+ frame(Name, Required), % Request is for some slots
    frame(Name, Slots),
    get_from_slots(Required, Slots).

%! get_from_slots(?Required, +Slots:list) is nondet.
%  extract the required attr=value pairs from the provided slots
get_from_slots([], _).  % Nothing required
get_from_slots(Attr=Value, Slots) :-  % Singular required in Slots
    member(Attr=Value, Slots),
    \+ is_list(Value).
get_from_slots(Attr=Value, Slots) :-  % List of values, yield with member
    member(Attr=List, Slots),
    is_list(List),
    member(Value, List).
get_from_slots(Attr=Value, Slots) :-  % check ako
    member(ako=Ako, Slots),
    get_frame(Ako, Attr=Value).
get_from_slots(Attr=Value, Slots) :-  % check isa
    member(isa=Ako, Slots),
    get_frame(Ako, Attr=Value).
get_from_slots([Required|Tail], Slots) :-  % List of Req, recurse.
    get_from_slots(Required, Slots),
    get_from_slots(Tail, Slots).

load_data(Filename) :-
    consult(Filename).
