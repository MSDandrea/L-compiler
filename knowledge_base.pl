% -- grammar
atom_is_lower(N) :-
    atom_chars(N, [L]),
    char_type(L, lower).



expression --> literal.
expression --> variable.
expression --> expression , expression_tail.
expression_tail --> [].
expression_tail --> [+], expression.
expression_tail --> [-], expression.
expression_tail --> [*], expression.
add_op --> [+].
expression_op --> [-].
expression_op --> [*].

literal --> [I], {integer(I)}.
variable --> [V], {atom_is_lower(V)}.



% what_is
what_is(I,O):-string_chars(I,A), literal(A,[]), O = "literal".
what_is(I,O):-string_chars(I,A), variable(A,[]), O = "variable".
what_is(I,O):-string_chars(I,A), expression(A,[]), O = "expression".

% stack ops
pop([X|List],X,List).
push(X,List,[X|List]).

% memory ops_dic
update_memory(K, V, Dict_in, Dict_out):- put_dict([K=V], Dict_in, Dict_out).
read_memory(K,Dict,Expected):- get_dict(K,Dict,Expected). 

%SMC transitions tran(initial_S, initial_M, initial_C, final_S, final_M, final_C).
tran(S, M, [L1,OP,L2| C], [L1, L2 | S], M, [OP | C]):- expression([L1,OP,L2],[]). %expression_tran
tran(S, M, [K| C], [K|S], M, C):- literal([K],[]). %literal_tran
tran(S,M,[K|C],[V|S],M,C):- read_memory(K,M,V). %var_tran

%eval of SMC states
eval([L1,L2 | _], _, [OP | _], Result):- add_op([OP],[]), Result is L1 + L2.

