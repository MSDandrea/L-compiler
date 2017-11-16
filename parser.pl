atoms_strings([A],Inter,S):-
    number_string(String,A),
    append([String],Inter,S).
atoms_strings([A|B],Inter,S):-
    number_string(String,A),
    append([String],Inter,Uni),
    atoms_strings(B,Uni,S).

atoms_strings([A],Inter,S):-
    atom_string(String,A),
    append([String],Inter,S).
atoms_strings([A|B],Inter,S):-
    atom_string(String,A),
    append([String],Inter,Uni),
    atoms_strings(B,Uni,S).

ast(String,Tree):-
    split_string(String," ", " ", String_List),
    atoms_strings(String_List,[],Rev),
    reverse(Rev,True_List),
    phrase(parser(Tree),True_List).

not_reserved(A):- \+ member(A,[if,then,else,while,do]).

literal(X) --> [X], { atom(X), not_reserved(X) }.
type(T) --> [T], {member(T,[int])}.
num(N) --> [N], { number(N) }.

parser(Tree) --> program(Tree).

program(sequence(A,B)) --> declaration(A), [';'], program(B).
program(sequence(A,B)) --> command(A), [';'], program(B).
program(B) --> command(B).


declaration(const(Lit,Type,Exp)) --> [const], literal(Lit), type(Type), ['='], expression(Exp).
declaration(var(Lit,Type,Exp)) --> [var], literal(Lit), type(Type), ['='], expression(Exp).

expression(A) --> num(A).
expression(A) --> literal(A).
expression(if_exp(B,Exp1,Exp2)) --> [if], bool_expression(B), [then], expression(Exp1), [else], expression(Exp2).
expression(algebra(A,OP,B)) --> expression(A), [OP], expression(B), {member(OP,['+','-','*'])}.

 
bool_expression(true) --> [true]. 
bool_expression(false) --> [false].
bool_expression(equals(A,B)) -->  num(A), [=], num(B).
bool_expression(equals(A,B)) -->  literal(A), [=], num(B).
bool_expression(not(B)) --> [~], bool_expression(B).
bool_expression(or(B1,B2)) --> bool_expression(B1), [or], bool_expression(B2).


command(nil) --> [].
command(assign(A,B)) --> literal(A), [':='], expression(B).
command(if(B,Cmd1,Cmd2)) --> [if], bool_expression(B), [then], program(Cmd1), [else], program(Cmd2).
command(while(B,Cmd)) --> [while], bool_expression(B), [do], program(Cmd).
command(print(Exp)) --> [print], expression(Exp).
command(exit(Exp)) --> [exit], expression(Exp).