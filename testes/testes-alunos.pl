desafio(1) :- resolve_manual([1, 2, 3, 4, 5, 6, 7, 8, 0], [1, 0, 2, 4, 5, 3, 7, 8, 6]).
% input b d e b d

desafio(2) :- resolve_manual([0, 1, 3, 4, 2, 5, 7, 8, 6], [1, 2, 3, 4, 5, 6, 7, 8, 0]).
% input e c e c

desafio(3) :- resolve_cego([1, 2, 3, 4, 5, 6, 7, 8, 0], [1, 0, 2, 4, 5, 3, 7, 8, 6]).

desafio(4) :- resolve_cego([0, 1, 3, 4, 2, 5, 7, 8, 6], [1, 2, 3, 4, 5, 6, 7, 8, 0]).

desafio(5) :- resolve_info_m([1, 2, 3, 4, 5, 6, 7, 8, 0], [1, 0, 2, 4, 5, 3, 7, 8, 6]).

desafio(6) :- resolve_info_m([0, 1, 3, 4, 2, 5, 7, 8, 6], [1, 2, 3, 4, 5, 6, 7, 8, 0]).
