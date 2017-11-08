:- [parser].
:- op(0, xfx, =>).
:- op(800, xfx, =>).

% -- identifiers
atom_is_lower(N) :-
    atom_chars(N, [L]),
    char_type(L, lower);fail.

integer --> [I], {integer(I)}.
variable --> [V], {atom_is_lower(V)}.
truth_value--> [T],{member(T,[true,false])}.


%max of list
max_l([],0).
max_l([X],X).
max_l([X|Xs], M):- max_l(Xs, M), M >= X.
max_l([X|Xs], X):- max_l(Xs, M), X >  M.

next_space(S,M):-
    dict_pairs(M,_,ListPairs),
    pairs_keys_values(ListPairs,Keys,_),
    max_l(Keys,Current),
    S is Current+1,!.



% memory ops_dic
update_dictionary(K, V, Dict_in, Dict_out):-put_dict([K=V], Dict_in, Dict_out).
read_dictionary(K,Dict,Expected):- get_dict(K,Dict,Expected). 

% environment_ops
% bind constant to environment
bind(Ei,K,V,Ef):- update_dictionary(K,V,Ei,Ef). 

mloc(Mi,V,Id,Mf):-
 next_space(Id,Mi),
 update_dictionary(Id,V,Mi,Mf),!.

get_value(K,E,M,V):- 
    read_dictionary(K,E,loc(T)), read_dictionary(T,M,V).
get_value(K,E,_,V):-  read_dictionary(K,E,V).

set_value(K,E,Mi,V,Mf):-
    read_dictionary(K,E,loc(T)), update_dictionary(T,V,Mi,Mf).

%SMC transitions (initial_E, initial_S, initial_M, initial_C) =>(final_S, final_M, final_C).
%%%%%% Declarations %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(E,[bnd(V,N)| S],M,C) => (E,S,Mf,[]):-
    bind(E,V,N,Em),
    eval((Em,S,M,C),(Em,[],Mf,[])).
    
(E,S,M,[const(V,_,Exp) | C]) => (E,[V|S],M,[Exp,cnt|C]).
(E,[N,V|S],M,[cnt | C]) => (E,[bnd(V,N)| S],M,C).

(E,S,M,[var(V,_,Exp)| C]) => (E,[V|S],M,[Exp,vr|C]).

(E,[N,V | S],M,[vr | C]) => (E,[bnd(V,loc(Id)) |S],Mf,C):-
    mloc(M,N,Id,Mf).



%%%%%%% COMMANDS %%%%%%%%%%%%%%%%%%%%%%
(E,S,M,[nil| C]) => (E,S,M,C).

(E,S,M,[sequence(E1,E2) | C]) => (E,S,M,[E1,E2|C]).

(E, S, M, [if(B,P1,P2)|C]) => (E, [P1,P2|S], M, [B, if|C]).                 
(E, S, M, [while(B,P1)|C]) => (E, [B, P1|S], M, [B, while|C]).      

(E, [true, P1, _|S], M, [if|C]) => (E, S, M, [P1|C]).                      
(E, [false, _, P2|S], M, [if|C]) => (E, S, M, [P2|C]).                     
(E, [true, B, P1|S], M, [while|C]) => (E, S, M, [P1,while(B, P1)|C]).      
(E, [false, _, _|S], M, [while|C]) => (E, S, M, C).  

%%%%%%% BOOLEAN EXPRESSION %%%%%%%%%%%%
(E, S,M,[Bool|C]) => (E, [Bool | S], M , C):-
    truth_value([Bool],_).      

(E,S,M,[equals(E1,E2) | C]) => (E,S,M,[E1,E2, =| C]).

(E,[L1,L2 |S],M,[OP|C]) => (E,[Value | S],M,C):-
    integer([L1],_),integer([L2],_),
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
(E,S,M,[if_exp(Bool,Exp1,Exp2)|C]) => (E,S,M,[Exp1, Exp2, Bool, if_exp|C]).
(E,[ true,_,Sol1 |S],M,[if_exp| C]) => (E,[Sol1 | S],M,C).
(E,[ false,Sol2,_ |S],M,[if_exp| C]) => (E,[Sol2 | S],M,C).


(E,S,M,[Literal | C]) => (E,[Literal | S],M,C):-
    integer([Literal],_).

(E,[L1,L2 | S], M, [OP|C]) => (E,[Result | S],M,C):-
    integer([L1],_),integer([L2],_),
    member(OP,[+,-,*]),
    Expression =.. [OP,L2,L1],
    Result is Expression.


(E,S,M,[algebra(E1,OP,E2) | C]) => (E,S,M,[E1,E2,OP|C]).
    

(E,S,M,[assign(E1,E2) | C]) => (E,[E1|S],M,[E2,:=|C]):-
    variable([E1],_).

(E,[Lit,Var|S],Minit,[:= | C]) => (E,S,Mfin,C):-
    integer([Lit],_),
    variable([Var],_),
    set_value(Var,E,Minit,Lit,Mfin).


(E,S, M, [Var | C]) => (E,[Val | S], M, C):-
    variable([Var],_),
    get_value(Var,E,M,Val).

eval(Input, Output) :-
    Input => Mid,
    !,
    eval(Mid, Output).
eval(Output, Output).

run(Program,Memory):-
    eval((e{},[],m{},[Program]), (e{},[],Memory,[])).

compile_run(String,M):-
    ast(String,Tree), !, write(Tree), nl,nl, run(Tree,M).