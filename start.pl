:- [smc_machine].
main:-
    	write('Please type L filename:'),
		nl,
		read(user_input,X),
		with_output_to(atom(A), write(X)),
		open(A,read,File),
		read_string(File, "", "\r\t ", End, String), nl,
		write(String), nl, nl,
		split_string(String, "\n", "", String2),
		atomic_list_concat(String2, '', Atom), atom_string(Atom, Programa),
		compile_run(Programa,M),
		write(M),nl.