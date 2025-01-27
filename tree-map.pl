%constructor
ctor(Key, Value, Height, Left, Right, node(Key, Value, Height, Left, Right)).
ctor(Key, Value, node(Key, Value, 0, nill, nill)).

%getters
get_val(nill, 0) :- !.
get_height(nill, 0) :- !.
get_children(node(_, _, _, Right, Left), Right, Left).
get_key(node(Key, _, _, _, _), Key).
get_val(node(_, Value, _, _, _), Value).
get_height(node(_, _, Height, _, _), Height).
get_left(node(_, _, _, Left, _), Left).
get_right(node(_, _, _, _, Right), Right).
get_pair(node(Key, Value, _, _, _), Key, Value).
move(node(Key, Value, Height, Left, Right), Key, Value, Height, Left, Right).


%getters+
get_right_height(Tree, Height) :- get_right(Tree, Right), get_height(Right, Height).
get_left_height(Tree, Height) :- get_left(Tree, Left), get_height(Left, Height).

get_balance(nill, 0) :- !.
get_balance(Tree, Balance) :- get_children(Tree, nill, nill), !, Balance is 0.
get_balance(Tree, Balance) :- get_left(Tree, nill), !, get_right_height(Tree, Height), Balance is - Height.
get_balance(Tree, Balance) :- get_right(Tree, nill), !, get_left_height(Tree, Height), Balance is Height.
get_balance(Tree, Balance) :- get_left_height(Tree, Left), get_right_height(Tree, Right), Balance is Left - Right.


%setters
correct_val(Tree, NewTree, Value) :-
    move(Tree, Key, _, Height, Left, Right),
    ctor(Key, Value, Height, Left, Right, NewTree).
correct_height(Tree, NewTree) :-
    move(Tree, Key, Value, _, Left, Right),
    Height is max(get_height(Left), get_height(Right)) + 1,
    ctor(Key, Value, Height, Left, Right, NewTree).
correct_left(Tree, NewTree, Left) :-
    move(Tree, Key, Value, Height, _, Right),
    ctor(Key, Value, Height, Left, Right, NewTree).
correct_right(Tree, NewTree, Right) :-
    move(Tree, Key, Value, Height, Left, _),
    ctor(Key, Value, Height, Left, Right, NewTree).


%rotations
small_left_rotation(Tree, NewTree) :-
    get_right(Tree, Right),
    get_left(Right, Left),
    correct_right(Tree, NewRight, Left),
    correct_height(NewRight, NewTree_tmp),
    correct_left(Right, NewLeft, NewTree_tmp),
    correct_height(NewLeft, NewTree).

small_right_rotation(Tree, NewTree) :-
    get_left(Tree, Left),
    get_right(Left, Right),
    correct_left(Tree, NewLeft, Right),
    correct_height(NewLeft, NewTree_tmp),
    correct_right(Left, NewRight, NewTree_tmp),
    correct_height(NewRight, NewTree).

big_left_rotation(Tree, NewTree) :-
    get_right(Tree, Right),
    small_right_rotation(Right, NewRight),
    correct_right(Tree, NewRight, NewTree_tmp),
    small_left_rotation(Tree, NewTree).

big_right_rotation(Tree, NewTree) :-
    get_left(Tree, Left),
    small_left_rotation(Left, NewLeft),
    correct_left(Tree, NewLeft, NewTree_tmp),
    small_right_rotation(Tree, NewTree).


%balance
balance_switch(Tree, _, 1, -2, NewTree) :- !, big_left_rotation(Tree, NewTree).
balance_switch(Tree, _, Right, 2, NewTree) :- small_right_rotation(Tree, NewTree).
balance_switch(Tree, 1, _, -2, NewTree) :- !, big_right_rotation(Tree, NewTree).
balance_switch(Tree, Left, _, -2, NewTree) :- small_right_rotation(Tree, NewTree).
balance_switch(Nothing, _, _, _, Nothing).

balance(Tree, NewTree) :-
    get_balance(Tree, This),
    get_balance(Tree, Left),
    get_balance(Tree, Right),
    balance_switch(Tree, Left, Right, This, NewTree).


%map
map_put(nill, Key, Value, Tree) :- ctor(Key, Value, Tree), !.
map_put(Tree, Key, Value, NewTree) :- get_key(Tree, Key), !, correct_val(Tree, NewTree, Value).
map_put(Tree, Key, Value, NewTree) :- get_key(Tree, ThisKey), Key > ThisKey, !,
                                          get_right(Tree, Right), map_put(Right, Key, Value, NewRight),
                                          correct_right(Tree, NewTree_tmp, NewRight),
                                          balance(NewTree_tmp, NewTree).
map_put(Tree, Key, Value, NewTree) :- get_key(Tree, ThisKey), Key < ThisKey, !,
                                          get_left(Tree, Left), map_put(Left, Key, Value, NewLeft),
                                          correct_left(Tree, NewTree_tmp, NewLeft),
                                          balance(NewTree_tmp, NewTree).

map_build([], nill).
map_build([(Key, Value) | Other], Tree) :- map_build(Other, NewTree), map_put(NewTree, Key, Value, Tree).

map_get(Tree, Key, Value) :- get_pair(Tree, Key, Value), !.
map_get(Tree, Key, Value) :- get_pair(Tree, ThisKey, ThisVal),
                                Key < ThisKey, !, get_left(Tree, Left), map_get(Left, Key, Value).
map_get(Tree, Key, Value) :- get_pair(Tree, ThisKey, ThisVal),
                                Key > ThisKey, get_right(Tree, Right), map_get(Right, Key, Value).


delete_left_recur(Tree, Tree) :- get_left(Tree, nill), !.
delete_left_recur(Tree, Next) :- get_left(Tree, LeftTree), delete_left_recur(LeftTree, Next).

switch_remover(_, nill, nill, nill) :- !.
switch_remover(_, nill, Right, Right) :- !.
switch_remover(_, Left, nill, Left) :- !.
switch_remover(Tree, Left, Right, NewTree) :- delete_left_recur(Right, Next), get_pair(Next, Key, Value),
																							move(Tree, _, _, Height, Left, Right), ctor(Key, Value, Height, Left, Right, NewTree_tmp),
																							map_remove(Right, Key, NewTree_R), correct_right(NewTree_tmp, NewTree, NewTree_R).

map_remove(nill, _, nill) :- !.
map_remove(Tree, Key, NewTree) :- get_key(Tree, Key), !, get_children(Tree, Left, Right),
																	switch_remover(Tree, Left, Right, NewTree_tmp),
																	balance(NewTree_tmp, NewTree).
map_remove(Tree, Key, NewTree) :- get_key(Tree, ThisKey), Key > ThisKey, !,
                                  get_right(Tree, Right), map_remove(Right, Key, NewRight),
                                          correct_right(Tree, NewTree_tmp, NewRight),
                                          balance(NewTree_tmp, NewTree).
map_remove(Tree, Key, NewTree) :- get_key(Tree, ThisKey), Key < ThisKey, !,
                                  get_left(Tree, Left), map_remove(Left, Key, NewLeft),
                                          correct_left(Tree, NewTree_tmp, NewLeft),
                                          balance(NewTree_tmp, NewTree).
                                          
