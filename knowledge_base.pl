% -- grammar
atom_is_lower(N) :-
    atom_chars(N, [L]),
    char_type(L, lower).



expression --> literal.
expression --> variable.
expression --> expression , expression_tail.
expression_tail --> [].
expression_tail --> [+], expression,{write("Reconhecido como soma"),nl}.
expression_tail --> [-], expression,{write("Reconhecido como subtração"),nl}.
expression_tail --> [*], expression,{write("Reconhecido como multiplicação"),nl}.
add_op --> [+].
expression_op --> [-].
expression_op --> [*].

literal --> [I], {integer(I)}.
literal --> [C], {char_type(C,digit)}.
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
write_smc_state(S,M,C):-
    write("----ESTADO-ATUAL[SMC]----"),nl,
    write(S),
    nl,
    write(M),
    nl,
    write(C),nl.
tran(S, M, [L1,OP,L2| C], [L1, L2 | S], M, [OP | C]):- expression([L1,OP,L2],[]), write_smc_state(S, M, [L1,OP,L2| C]), write_smc_state([L1, L2 | S], M, [OP | C]). %expression_tran
tran(S, M, [K| C], [K|S], M, C):- literal([K],[]). %literal_tran
tran(S,M,[K|C],[V|S],M,C):- read_memory(K,M,V). %var_tran

%eval of SMC states
eval([L1,L2 | _], _, [OP | _], Result):- add_op([OP],[]), Result is L1 + L2.

%"Compile"
compile_list(List,Result):- expression(List,_), tran([],_,List,S,M,C), eval(S,M,C,Result).

