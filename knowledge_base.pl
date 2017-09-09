atom_is_lower(N) :-
    atom_chars(N, [L]),
    char_type(L, lower).


literal --> [I], {integer(I)}.
variable --> [V], {atom_is_lower(V)}.

expression --> literal.
expression --> variable.

expression --> expression , expression_tail.
expression_tail --> [].
expression_tail --> [+], expression.
expression_tail --> [-], expression.
expression_tail --> [*], expression.

