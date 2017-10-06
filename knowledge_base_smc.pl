:- op(0, xfx, =>).
:- op(800, xfx, =>).

:- nb_setval(mpt,0).

% -- grammar
atom_is_lower(N) :-
    atom_chars(N, [L]),
    char_type(L, lower);fail.



expression --> literal.
expression --> variable.
expression --> expression , expression_tail.
expression_tail --> [].
expression_tail --> [+], expression,{write("Reconhecido como soma"),nl}.
expression_tail --> [-], expression,{write("Reconhecido como subtração"),nl}.
expression_tail --> [*], expression,{write("Reconhecido como multiplicação"),nl}.

literal --> [I], {integer(I)}.
variable --> [V], {atom_is_lower(V)}.
truth_value--> [T],{member(T,[true,false])}.


boolean_expression --> truth_value.
boolean_expression --> [~], boolean_expression.
boolean_expression --> expression, [=], expression.
boolean_expression --> boolean_expression, [or], boolean_expression.

command --> [if], boolean_expression, [then], command, [else], command.
command --> [while], boolean_expression, [do], command.
command --> [nil].
command --> variable, [:=], expression.
command --> command, [';'], command.


% what_is
what_is(I,O):-string_chars(I,A), literal(A,[]), O = "literal".
what_is(I,O):-string_chars(I,A), variable(A,[]), O = "variable".
what_is(I,O):-string_chars(I,A), expression(A,[]), O = "expression".

next_space(S):-
    nb_getval(mpt,V),
    Vn is V+1,
    nb_setval(mpt,Vn),
    S is Vn.

% memory ops_dic
update_memory(K, V, Dict_in, Dict_out):-put_dict([K=V], Dict_in, Dict_out).
read_memory(K,Dict,Expected):- get_dict(K,Dict,Expected). 

% environment_ops
% bind constant to environment
bind(Ei,K,V,Ef):- update_memory(K,V,Ei,Ef). 

% put value in memory and reference in environment
mloc(Ei,Mi,K,V,Ef,Mf):- 
    read_memory(K,Ei,loc(P)) -> % checks if variable is already in memory, updates value if it is
        update_memory(P,V,Mi,Mf), Ef = Ei; 
    next_space(Id),update_memory(K,loc(Id),Ei,Ef),update_memory(Id,V,Mi,Mf),!. % find new space in memory and create var

%read value of key either in environment or memory
get_value(K,E,M,V):- 
    read_memory(K,E,loc(T)) ->
        read_memory(T,M,V);
    read_memory(K,E,V).

%SMC transitions tran(initial_S, initial_M, initial_C, final_S, final_M, final_C).
write_smc_state(S,M,C):-
    write("----ESTADO-ATUAL[SMC]----"),nl,
    write(S),
    nl,
    write(M),
    nl,
    write(C),nl.

%%%%%% Declarations %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(E,S,M,[const(V,T,Exp) | C]) => (E,[V|S],M,[Exp,cnt|C]).
(E,[N,V|S],M,[cnt | C]) => (E,[bnd(V,N)| S],M,C).
(E,[bnd(V,N)| S],M,C) => (E,S,Mf,[]):-
    bind(E,V,N,Em),
    eval((Em,S,M,C),(Em,[],Mf,[])).

(E,S,M,[var(V,T,Exp)| C]) => (E,[V|S],M,[Exp,vr|C]).
(E,[N,V],M,[vr | C]) => (E,S,Mf,[]):-
    mloc(E,M,V,N,Em,Mm),
    eval((Em,S,Mm,C),(Em,[],Mf,[])).

%%%%%%% COMMANDS %%%%%%%%%%%%%%%%%%%%%%
(E,S,M,[nil| C]) => (E,S,M,C).

(E,S,M,[Exp | C]) => (E,S,M,[E1,E2|C]):-
    compound(Exp),
    Exp =.. [OP,E1,E2],
    member(OP,[;]).

(E, S, M, [if(B,P1,P2)|C]) => (E, [P1,P2|S], M, [B, if|C]).                 
(E, S, M, [while(B,P1)|C]) => (E, [B, P1|S], M, [B, while|C]).      

(E, [true, P1, _|S], M, [if|C]) => (E, S, M, [P1|C]).                      
(E, [false, _, P2|S], M, [if|C]) => (E, S, M, [P2|C]).                     
(E, [true, B, P1|S], M, [while|C]) => (E, S, M, [P1,while(B, P1)|C]).      
(E, [false, _, _|S], M, [while|C]) => (E, S, M, C).  

%%%%%%% BOOLEAN EXPRESSION %%%%%%%%%%%%
(E, S,M,[Bool|C]) => (E, [Bool | S], M , C):-
    truth_value([Bool],_).      

(E,[L1,L2 |S],M,[OP|C]) => (E,[Value | S],M,C):-
    literal([L1],_),literal([L2],_),
    member(OP,[=]),
    (call(L1=:=L2) ->   Value=true; Value=false).
                     
(E,S,M,[not(B) | C]) => (E,S,M,[B, not| C]).
(E, [true | S],M,[not | C]) => (E, [false | S],M,C).
(E, [false | S],M,[not | C]) => (E, [true | S],M,C).

(E,S,M,[or(B1,B2)| C]) => (E,S,M,[B1,B2,or| C]).
(E,[true,_ |S],M,[or|C]) => (E,[true | S],M,C).
(E,[_,true |S],M,[or|C]) => (E,[true | S],M,C).
(E,[false,false | S],M,[or|C]) => (E,[false|S],M,C).

%%%%%%% EXPRESSIONS %%%%%%%%%%%%%%%%%%%
(E,[L1,L2 | S], M, [OP|C]) => (E,[Result | S],M,C):-
    literal([L1],_),literal([L2],_),
    member(OP,[+,-,*]),
    Expression =.. [OP,L2,L1],
    Result is Expression.


(E,S,M,[Exp | C]) => (E,S,M,[E1,E2,OP|C]):-
    compound(Exp),
    Exp =.. [OP,E1,E2],
    member(OP,[+,-,*,=]).
    

(E,S,M,[Exp | C]) => (E,[E1|S],M,[E2,OP|C]):-
    compound(Exp),
    Exp =.. [OP,E1,E2],
    variable([E1],_),
    member(OP,[:=]).

(E,[Lit,Var|S],Minit,[:= | C]) => (E,S,Mfin,C):-
    literal([Lit],_),
    variable([Var],_),
    update_memory(Var,Lit,Minit,Mfin).

(E,S,M,[Literal | C]) => (E,[Literal | S],M,C):-
    literal([Literal],_).

(E,S, M, [Var | C]) => (E,[Val | S], M, C):-
    variable([Var],_),
    read_memory(Var,M,Val).



eval(Input, Output) :-
    Input => Mid,
    %   nl,nl,write("("),write(Input),write(")"),write(" => "),write("("),write(Mid),write(")"),
    !,
    eval(Mid, Output).
eval(Output, Output).

run(Program,Memory):-
    eval((e{},[],m{},[Program]), (e{},[],Memory,[])).

