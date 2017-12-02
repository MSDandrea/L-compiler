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
delete_dictionary(Mi,K,Mf):- del_dict(K,Mi,_,Mf).

% environment_ops
% bind constant to environment
bind(Ei,K,V,Ef):- update_dictionary(K,V,Ei,Ef). 

mloc(Mi,V,Id,Mf):-
 next_space(Id,Mi),
 update_dictionary(Id,V,Mi,Mf),!.

free(Mi,loc(K),Mf):-
 delete_dictionary(Mi,K,Mf).

free(Mi,_,Mi).

get_value(K,E,M,V):- 
    read_dictionary(K,E,loc(T)), read_dictionary(T,M,V).
get_value(K,E,_,V):-  read_dictionary(K,E,V).

set_value(K,E,Mi,V,Mf):-
    read_dictionary(K,E,loc(T)), update_dictionary(T,V,Mi,Mf).


build_param(Mem_in,Env_in,P,[],Env_in,Mem_in,P).
build_param(Mem_in,Env_in,[HeadP | TailP],[HeadF | TailF],Env_out,Mem_out,S):-
    mloc(Mem_in,HeadP,Id,Mem_med),
    bind(Env_in,HeadF,loc(Id),Env_med),
    build_param(Mem_med,Env_med,TailP,TailF,Env_out,Mem_out,S).

clean_param(Env,Mem_in,[],Mem_in).
clean_param(Env,Mem_in,[ParamHead|ParamTail],Mem_out):-
    read_dictionary(ParamHead,Env,Value),
    free(Mem_in,Value,Mem_mid),
    clean_param(Env,Mem_mid,ParamTail,Mem_out).



append([],C,C).
append([Head|Tail],C,Cf):-
    Cm = [Head | C],
    append(Tail,Cm,Cf).

%SMC transitions (initial_E, initial_S, initial_M, initial_C) =>(final_S, final_M, final_C).

%%%%%% Declarations %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(O,E,[bnd(V,N)| S],M,C) => (Of,E,S,Mf,[]):-
    bind(E,V,N,Em),
    eval((O,Em,S,M,C),(Of,Em,[],Mm,[])),
    free(Mm,N,Mf).
    
(O,E,S,M,[const(V,_,Exp) | C]) => (O,E,[V|S],M,[Exp,cnt|C]).
(O,E,[N,V|S],M,[cnt | C]) => (O,E,[bnd(V,N)| S],M,C).

(O,E,S,M,[var(V,_,Exp)| C]) => (O,E,[V|S],M,[Exp,vr|C]).

(O,E,[N,V | S],M,[vr | C]) => (O,E,[bnd(V,loc(Id)) |S],Mf,C):-
    mloc(M,N,Id,Mf).

(O,E,S,M,[proc(I,F,sequence(A,B)) | C]) => (O,E,[bnd(I,abs(F,sequence(A,B))) |S ],M,C).


%%%%%%% COMMANDS %%%%%%%%%%%%%%%%%%%%%%
(O,E,S,M,[nil| C]) => (O,E,S,M,C).

(O,E,S,M,[sequence(E1,E2) | C]) => (O,E,S,M,[E1,E2|C]).

(O,E, S, M, [if(B,P1,P2)|C]) => (O,E, [P1,P2|S], M, [B, if|C]).                 
(O,E, S, M, [while(B,P1)|C]) => (O,E, [B, P1|S], M, [B, while|C]).      

(O,E, [true, P1, _|S], M, [if|C]) => (O,E, S, M, [P1|C]).                      
(O,E, [false, _, P2|S], M, [if|C]) => (O,E, S, M, [P2|C]).                     
(O,E, [true, B, P1|S], M, [while|C]) => (O,E, S, M, [P1,while(B, P1)|C]).      
(O,E, [false, _, _|S], M, [while|C]) => (O,E, S, M, C).

(O,E,S,M,[call(I,P)|C]) => (O,E,S,M,[params(P), call(I) | C]).
(O,E,S,M,[params(P)|C]) => (O,E,S,M,Cf):-
    append(P,C,Cf).

(O,E,S,M,[call(I)|C]) => (Of,E,Sm,Mc,C):-
    get_value(I,E,M,abs(F,Seq)),
    reverse(F,F_stack),
    build_param(M,E,S,F_stack,Em,Mm,Sm),
    eval((O,Em,[],Mm,[Seq]),(Of,Em,[],Mf,[])),
    clean_param(Em,Mf,F_stack,Mc).


(O,E,[Val | S],M,[print | C]) => ([Val | O],E,S,M,C).
(O,E,S,M,[print(Exp)|C]) => (O,E,S,M,[Exp, print | C]).

(O,E,S,M,[exit(Exp)|C]) => (O,E,S,M,[Exp, exit | C]).
(O,E,[Val | S],M,[exit | C]) => ([Val | O],E,[],M,[]).

%%%%%%% BOOLEAN EXPRESSION %%%%%%%%%%%%
(O,E, S,M,[Bool|C]) => (O,E, [Bool | S], M , C):-
    truth_value([Bool],_).      

(O,E,S,M,[equals(E1,E2) | C]) => (O,E,S,M,[E1,E2, =| C]).

(O,E,[L1,L2 |S],M,[OP|C]) => (O,E,[Value | S],M,C):-
    integer([L1],_),integer([L2],_),
    member(OP,[=]),
    (call(L1=:=L2) ->   Value=true; Value=false).
                     
(O,E,S,M,[not(B) | C]) => (O,E,S,M,[B, not| C]).
(O,E, [true | S],M,[not | C]) => (O,E, [false | S],M,C).
(O,E, [false | S],M,[not | C]) => (O,E, [true | S],M,C).

(O,E,S,M,[or(B1,B2)| C]) => (O,E,S,M,[B1,B2,or| C]).
(O,E,[true,_ |S],M,[or|C]) => (O,E,[true | S],M,C).
(O,E,[_,true |S],M,[or|C]) => (O,E,[true | S],M,C).
(O,E,[false,false | S],M,[or|C]) => (O,E,[false|S],M,C).

%%%%%%% EXPRESSIONS %%%%%%%%%%%%%%%%%%%
(O,E,S,M,[if_exp(Bool,Exp1,Exp2)|C]) => (O,E,S,M,[Exp1, Exp2, Bool, if_exp|C]).
(O,E,[ true,_,Sol1 |S],M,[if_exp| C]) => (O,E,[Sol1 | S],M,C).
(O,E,[ false,Sol2,_ |S],M,[if_exp| C]) => (O,E,[Sol2 | S],M,C).


(O,E,S,M,[Literal | C]) => (O,E,[Literal | S],M,C):-
    integer([Literal],_).

(O,E,[L1,L2 | S], M, [OP|C]) => (O,E,[Result | S],M,C):-
    integer([L1],_),integer([L2],_),
    member(OP,[+,-,*]),
    Expression =.. [OP,L2,L1],
    Result is Expression.


(O,E,S,M,[algebra(E1,OP,E2) | C]) => (O,E,S,M,[E1,E2,OP|C]).
    

(O,E,S,M,[assign(E1,E2) | C]) => (O,E,[E1|S],M,[E2,:=|C]):-
    variable([E1],_).

(O,E,[Lit,Var|S],Minit,[:= | C]) => (O,E,S,Mfin,C):-
    integer([Lit],_),
    variable([Var],_),
    set_value(Var,E,Minit,Lit,Mfin).


(O,E,S, M, [Var | C]) => (O,E,[Val | S], M, C):-
    variable([Var],_),
    get_value(Var,E,M,Val).

eval((O,E,S,M,C), Output) :-
    (O,E,S,M,C) => Mid,
    !,
    eval(Mid, Output).
eval(Output, Output).

run(Program,Memory,Output):-
    eval(([],e{},[],m{},[Program]), (Rev_Output,e{},[],Memory,[])),reverse(Rev_Output,Output).

compile_run(String,M,O):-
    ast(String,Tree), !, write(Tree), nl,nl, run(Tree,M,O).