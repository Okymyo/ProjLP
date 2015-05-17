start :- carregaprojeto, desafios.
carregaprojeto :- ['puzzle8.pl'], ['testes-alunos.pl'].
desafios :- correTodosDesafios(1, 7).

correTodosDesafios(Limite, Limite).

correTodosDesafios(Min, Max) :- 
    Min < Max,
    writeln('\n'), write('teste '), write(Min), writeln(':'),
    desafio(Min), 
    Min1 is Min + 1,
    correTodosDesafios(Min1, Max).
