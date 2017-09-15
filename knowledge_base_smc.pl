:- op(0, xfx, =>).
:- op(800, xfx, =>).

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


% memory ops_dic
update_memory(K, V, Dict_in, Dict_out):-put_dict([K=V], Dict_in, Dict_out).
read_memory(K,Dict,Expected):- get_dict(K,Dict,Expected). 

%SMC transitions tran(initial_S, initial_M, initial_C, final_S, final_M, final_C).
write_smc_state(S,M,C):-
    write("----ESTADO-ATUAL[SMC]----"),nl,
    write(S),
    nl,
    write(M),
    nl,
    write(C),nl.

%%%%%%% COMMANDS %%%%%%%%%%%%%%%%%%%%%%
(S,M,[nil| C]) => (S,M,C).

(S,M,[Exp | C]) => (S,M,[E1,E2|C]):-
    compound(Exp),
    Exp =.. [OP,E1,E2],
    member(OP,[;]).

(S, M, [if(B,P1,P2)|C]) => ([P1,P2|S], M, [B, if|C]).                 
(S, M, [while(B,P1)|C]) => ([B, P1|S], M, [B, while|C]).              

([true, P1, _|S], M, [if|C]) => (S, M, [P1|C]).                      
([false, _, P2|S], M, [if|C]) => (S, M, [P2|C]).                     
([true, B, P1|S], M, [while|C]) => (S, M, [P1,while(B, P1)|C]).      
([false, _, _|S], M, [while|C]) => (S, M, C).  

%%%%%%% BOOLEAN EXPRESSION %%%%%%%%%%%%
(S,M,[Bool|C]) => ([Bool | S], M , C):-
    truth_value([Bool],_).   	

([L1,L2 |S],M,[OP|C]) => ([Value | S],M,C):-
    literal([L1],_),literal([L2],_),
    member(OP,[=]),
    (call(L1=:=L2) ->   Value=true; Value=false).
                     
(S,M,[not(B) | C]) => (S,M,[B, not| C]).
([true | S],M,[not | C]) => ([false | S],M,C).
([false | S],M,[not | C]) => ([true | S],M,C).

(S,M,[or(B1,B2)| C]) => (S,M,[B1,B2,or| C]).
([true,_ |S],M,[or|C]) => ([true | S],M,C).
([_,true |S],M,[or|C]) => ([true | S],M,C).
([false,false | S],M,[or|C]) => ([false|S],M,C).

%%%%%%% EXPRESSIONS %%%%%%%%%%%%%%%%%%%
([L1,L2 | S], M, [OP|C]) => ([Result | S],M,C):-
	literal([L1],_),literal([L2],_),
    member(OP,[+,-,*]),
	Expression =.. [OP,L1,L2],
	Result is Expression.


(S,M,[Exp | C]) => (S,M,[E1,E2,OP|C]):-
    compound(Exp),
    Exp =.. [OP,E1,E2],
    member(OP,[+,-,*,=]).
    

(S,M,[Exp | C]) => ([E1|S],M,[E2,OP|C]):-
    compound(Exp),
    Exp =.. [OP,E1,E2],
    variable([E1],_),
    member(OP,[:=]).

([Lit,Var|S],Minit,[:= | C]) => (S,Mfin,C):-
    literal([Lit],_),
    variable([Var],_),
    update_memory(Var,Lit,Minit,Mfin).

(S,M,[Literal | C]) => ([Literal | S],M,C):-
    literal([Literal],_).

(S, M, [Var | C]) => ([Val | S], M, C):-
    variable([Var],_),
    read_memory(Var,M,Val).

eval(Input, Output) :-
	Input => Mid,
	!,
	eval(Mid, Output).
eval(Output, Output).

run(Program,Memory):-
    eval(([],m{},[Program]), ([],Memory,[])).

