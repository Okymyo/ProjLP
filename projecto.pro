% Bons casos de teste:
% (3x3 - 05 passos) resolve_info_m([1, 2, 3, 4, 5, 6, 7, 8, 0], [1, 2, 3, 7, 4, 5, 8, 0, 6]).
% (3x3 - 31 passos) resolve_info_m([8, 6, 7, 2, 5, 4, 3, 0, 1], [1, 2, 3, 4, 5, 6, 7, 8, 0]).
% (4x4 - 50 passos) resolve_info_m([12, 15, 6, 10, 4, 9, 5, 8, 14, 13, 0, 2, 1, 7, 11, 3], [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0]).
% (3x3 -      TRUE) transformacao_possivel([0, 1, 3, 4, 2, 5, 7, 8, 6], [1, 2, 3, 4, 5, 6, 7, 8, 0]).
% (3x3 -     FALSE) transformacao_possivel([1, 2, 3, 4, 6, 7, 8, 5, 0], [1, 2, 3, 4, 5, 6, 7, 8, 0]).
% (4x4 -      TRUE) transformacao_possivel([1, 2, 3, 4, 5, 6, 0, 8, 9, 10, 7, 11, 13, 14, 15, 12], [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0]).
% (4x4 -     FALSE) transformacao_possivel([1, 2, 3, 4, 5, 6, 0, 8, 9, 10, 7, 11, 13, 14, 15, 12], [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 14, 0]).

%
% Declaracoes Gerais
%

% Tamanho do nosso tabuleiro.
% E de notar que a local stack e o tempo de computacao para o algoritmo A* cresce consideravelmente (exponencial?)
% com o tamanho, bem como com o numero de passos necessarios. Testado ate 4x4 com 50 passos necessarios.
tamanho(3).

% Ordem da procura cega.
ordem([b, d, c, e]).

% Jogadas e os seus offsets.
jogada(e, -1).
jogada(d, 1).
jogada(c, N):- tamanho(T), N is -T.
jogada(b, N):- tamanho(N).

% Jogadas e os seus nomes.
nome_movimento(c, 'cima').
nome_movimento(b, 'baixo').
nome_movimento(e, 'a esquerda').
nome_movimento(d, 'a direita').

%
% Predicados Gerais
%

% Verifica se a Peca esta numa dada Posicao num dado Tabuleiro.
% Podem-se fazer perguntas do genero peca([1, 2, 3], P, 2). a qual a resposta e P = 1.
peca(Tabuleiro, Posicao, Peca):- peca(Tabuleiro, Posicao, Peca, 0).
peca([Peca|_], Posicao, Peca, Acc):- Posicao is Acc.
peca([_|Cauda], Posicao, Peca, Acc):- 
	Acc1 is Acc+1,
	peca(Cauda, Posicao, Peca, Acc1).

% Verifica se uma lista com todas as Peca1 substituidas por Peca2 e igual a outra.
substitui([Peca1|Cauda], Peca1, Peca2, [Peca2|Cauda]).
substitui([Cabeca|Cauda], Peca1, Peca2, CFinal):-
	substitui(Cauda, Peca1, Peca2, CFinal1),
	CFinal = [Cabeca|CFinal1].

% Verifica se uma configuracao CInical é igual à CFinal com Peca1 trocada por Peca2.
troca(CInicial, Peca1, Peca2, CFinal):-
	substitui(CInicial, Peca1, tmp, CTemporaria),
	substitui(CTemporaria, Peca2, Peca1, CTemporaria1),
	substitui(CTemporaria1, tmp, Peca2, CFinal).

% Verifica se uma Posicao esta numa dada Linha.
% As Linhas comecam a contar do 0.
% Podes fazer perguntas do genero linha(2, L). L=0.
linha(Posicao, Linha):-
	tamanho(T),
	Linha is Posicao // T.

% Verifica se uma Posicao esta numa dada Coluna.
% As Colunas comecam a contar do 0.
% Pode-se fazer perguntas do genero de coluna(4, C). C=1
coluna(Posicao, Coluna):-
	tamanho(T),
	Coluna is Posicao mod T.

% Verifica se Posicao1 esta na mesma linha que a Posicao2.
% Este predicado apenas verifica! Nao instancia nada! Nao podes fazer perguntas mesma_linha(1, P).
mesma_linha(Posicao1, Posicao2):-
	linha(Posicao1, Linha),
	linha(Posicao2, Linha).

% Verifica se um movimento J aplicado na peca Peca transforma CInicial em CFinal.
mov_legal(CInicial, Jogada, Peca, CFinal):-
	peca(CInicial, Posicao, Peca),
	jogada(Jogada, Offset),
	Posicao1 is Posicao+Offset,
	peca(CInicial, Posicao1, 0),
	valida(Posicao, Posicao1, Offset),
	troca(CInicial, Peca, 0, CFinal).

% Valida os movimentos horizontais.
valida(PInicial, PFinal, Offset):-
	(Offset == 1; Offset == -1),
	mesma_linha(PInicial, PFinal).

% Valida os movimentos verticais.
valida(_, _, Offset):-
	(tamanho(Offset); (OffsetInv is -Offset, tamanho(OffsetInv))).

% Resolucao manual:
% 1. Pedir input ao utilizador
% 2. Verificar se input e um movimento legal
%	2.1. Caso nao seja, pedir input novamente
% 3. Aplicar movimento
%	3.1. Caso tenha terminado, anunciar
% 4. Correr com a nova configuracao
resolve_manual(CInicial, CFinal):-
	imprime_transf(CInicial, CFinal),
	!,
	pede_input(M),
	resolve_manual(CInicial, CFinal, M).

resolve_manual(CInicial, CFinal, M):-
	transformacao_possivel(CInicial, CFinal),
	mov_legal(CInicial, M, _, Resultado),
	Resultado \= CFinal,
	imprime_config(Resultado),
	pede_input(M2),
	resolve_manual(Resultado, CFinal, M2).

resolve_manual(CInicial, CFinal, M):-
	mov_legal(CInicial, M, _, Resultado),
	Resultado = CFinal,
	imprime_config(Resultado),
	write('Parabens!').

resolve_manual(CInicial, CFinal, M):-
	not(mov_legal(CInicial, M, _, _)),
	writeln('Movimento ilegal'),
	pede_input(M2),
	resolve_manual(CInicial, CFinal, M2).

% Pede input ao utilizador
pede_input(M):-
	nl,
	writeln('Qual o seu movimento?'),
	read(M).

% Resolucao cega:
% 1. Obter movimento da lista de movimentos
% 2. Verificar se o movimento e um movimento legal
%	2.1. Caso nao seja, retirar movimento da lista de movimentos, e correr novamente
% 3. Verificar se o resultado desse movimento nao foi previamente obtido
%	3.1. Caso nao seja, retirar movimento da lista de movimentos, e correr novamente
% 4. Aplicar movimento e guardar qual o movimento realizado e a peca movida
%	4.1. Caso tenha terminado, listar passos necessarios
% 5. Restaurar lista de movimentos ao estado inicial, e correr com a nova configuracao
resolve_cego(CInicial, CFinal):-
	transformacao_possivel(CInicial, CFinal),
	imprime_transf(CInicial, CFinal),
	!,
	ordem(Ordem),
	resolve_cego(CInicial, CFinal, Ordem, [], [], Solucao),
	imprime_passos(Solucao).

resolve_cego(CInicial, CFinal, _, _, Movimentos, Solucao):-
	CInicial == CFinal,
	Solucao = Movimentos.

resolve_cego(CInicial, CFinal, [M|_], Anteriores, Movimentos, Solucao):-
	mov_legal(CInicial, M, Peca, Resultado),
	not(na_lista(Anteriores, Resultado)),
	append([Resultado], Anteriores, Anteriores2),
	ordem(Ordem),
	append(Movimentos, [[Peca, M]], Temp),
	resolve_cego(Resultado, CFinal, Ordem, Anteriores2, Temp, Solucao).

resolve_cego(CInicial, CFinal, [_|Restantes], Anteriores, Movimentos, Solucao):-
	resolve_cego(CInicial, CFinal, Restantes, Anteriores, Movimentos, Solucao).
	
% Verifica se um determinado Item se encontra numa dada lista.
na_lista([Cabeca|Cauda], Item):-
	Cabeca == Item;
	na_lista(Cauda, Item).

% Procura informada utilizando A* e distancia de Manhattan!
% A Lista Abertos contem todos os Nos que ainda nao foram expandidos.
% A Lista Fechados contem todos os Nos previamente expandidos.
resolve_info_m(CInicial, CFinal):-
	imprime_transf(CInicial, CFinal),
	!,
	M = [],
	dist_manhattan(CInicial, CFinal, H),
	numero_elementos(M, G),
	F is G + H,
	no(CInicial, F, G, H, M, NoInicial),
	resolve_info_m(CFinal, [NoInicial], [], Solucao),
	imprime_passos(Solucao).

% Escolhe o no da Lista de nos abertos com o menor F, e expande-o.
resolve_info_m(CFinal, Abertos, Fechados, Solucao):-
	menor_F(Abertos, Indice),
	elemento_N(Abertos, Indice, No),
	remove_N(Abertos, Indice, Abertos1),
	!,
	expande_no(No, Abertos1, Fechados, CFinal, Solucao).

% Expande um dado No, adicionando todos os seus sucessores nao-previamente descobertos a Lista de nos abertos, e o no dado a Lista de nos fechados.
expande_no([C, _, _, _, M], _, _, C, M).
expande_no(No, Abertos, Fechados, CFinal, Solucao):-
	sucessores(No, CFinal, Sucessores),
	filtra_sucessores(Sucessores, Abertos, Fechados, Abertos1),
	append([No], Fechados, Fechados1),
	!,
	resolve_info_m(CFinal, Abertos1, Fechados1, Solucao).

filtra_sucessores([], Abertos, _, Abertos).
filtra_sucessores([No|Restantes], Abertos, Fechados, Resultado):-
	[Configuracao|_] = No,
	not(configuracao_calculada(Abertos, Configuracao)),
	not(configuracao_calculada(Fechados, Configuracao)),
	!,
	filtra_sucessores(Restantes, Abertos, Fechados, Temp),
	append([No], Temp, Resultado).

filtra_sucessores([_|Restantes], Abertos, Fechados, Resultado):-
	filtra_sucessores(Restantes, Abertos, Fechados, Resultado).

% Verifica se um No se encontra numa Lista de nos.
configuracao_calculada([[Cabeca|_]|Lista], Configuracao):-
	(Cabeca == Configuracao;
	configuracao_calculada(Lista, Configuracao)).

% Dado um No, e a Configuracao Final, gera todos os Nos Sucessores.
sucessores([C, _, G, _, M], CFinal, Sucessores):-
	ordem(Ordem),
	sucessores([C, _, G, _, M], CFinal, Sucessores, Ordem).

sucessores(_, _, [], []).
sucessores([C, _, G, _, M], CFinal, Sucessores, [Mov|Restantes]):-
	mov_legal(C, Mov, Peca, Resultado),
	!,
	G1 is G + 1,
	dist_manhattan(Resultado, CFinal, H),
	F is G1 + H,
	append(M, [[Peca, Mov]], M1),
	no(Resultado, F, G1, H, M1, No),
	sucessores([C, '', G, '', M], CFinal, Temp, Restantes),
	Sucessores = [No|Temp].

sucessores([C, _, G, _, M], CFinal, Sucessores, [_|Restantes]):-
	sucessores([C, '', G, '', M], CFinal, Sucessores, Restantes).

% Dada uma Lista de nos, devolve o indice do no de menor F.
menor_F(Abertos, Indice):- menor_F(Abertos, 0, _, Indice).
menor_F([Cabeca|[]], Actual, F, Actual):- no(_, F, _, _, _, Cabeca).
menor_F([Cabeca|Cauda], Actual, F, Indice):-
	Prox is Actual + 1,
	menor_F(Cauda, Prox, F_Temp, I_Temp),
	no(_, F_Actual, _, _, _, Cabeca),
	(F_Actual =< F_Temp -> (F = F_Actual, Indice = Actual); (F = F_Temp, Indice = I_Temp)).

% Verifica se uma dada configuracao, com f(C) = F, g(C) = G, h(C) = H, e movimentos ate C, gera um determinado no.
no(C, F, G, H, M, [C, F, G, H, M]).

% Calcula a distancia de Manhattan entre duas configuracoes.
dist_manhattan(CInicial, CFinal, Distancia):- dist_manhattan(CInicial, CFinal, CInicial, 0, Distancia).
dist_manhattan(_, _, [], Soma, Soma).
dist_manhattan(CInicial, CFinal, [0|Pecas], Soma, Distancia):- dist_manhattan(CInicial, CFinal, Pecas, Soma, Distancia).
dist_manhattan(CInicial, CFinal, [Peca|Pecas], Soma, Distancia):-
	peca(CInicial, PInicial, Peca),
	peca(CFinal, PFinal, Peca),
	coluna(PInicial, ColunaI),
	linha(PInicial, LinhaI),
	coluna(PFinal, ColunaF),
	linha(PFinal, LinhaF),
	Temp is Soma + (abs(ColunaI - ColunaF) + abs(LinhaI - LinhaF)),
	dist_manhattan(CInicial, CFinal, Pecas, Temp, Distancia).
	
% Verificacao de solvabilidade
% Dada uma Configuracao Inicial e uma Configuracao Final, indica se e possivel transformar uma na outra.
%
% Para tamanhos impares:
% Calcula-se o numero de inversoes necessarias: o numero de pecas que se encontram a frente da Peca A
% na configuracao inicial mas atras da mesma na configuracao final. Isto e calculado para cada peca.
% Este valor tem necessariamente de ser par.
%	
% Para tamanhos pares:
% E feito calculando novamente o numero de inversoes necessarias, mas desta vez soma-se a esse resultado
% a linha em que se encontra a peca 0 (o espaco vazio), e mod2 deste valor tem de ser igual a mod2
% da linha em que se encontra o 0 na configuracao final.
transformacao_possivel(CInicial, CFinal):-
	inversoes_c(CInicial, CFinal, Inversoes),
	tamanho(T),
	!,
	(T =< 0 -> fail; true),
	(T mod 2 =:= 1 ->
	Inversoes mod 2 =:= 0;							% Tamanho impar (3x3, 5x5, ...)
	(peca(CInicial, PosicaoI, 0),					% Tamanho par (2x2, 4x4, ...)
	peca(CFinal, PosicaoF, 0),
	linha(PosicaoI, LinhaI),
	linha(PosicaoF, LinhaF),
	(Inversoes + LinhaI) mod 2 =:= LinhaF mod 2)),
	!.

% Calcula o numero de inversoes entre uma dada Configuracao Inicial e uma Configuracao Final.
inversoes_c(CInicial, CFinal, Total):-
	tamanho(T),
	NumPecas is T*T - 1,
	inversoes_c(CInicial, CFinal, NumPecas, Total).

inversoes_c(_, _, 0, 0).
inversoes_c(CInicial, CFinal, Peca, Total):-
	ProxPeca is Peca - 1,
	inversoes_c(CInicial, CFinal, ProxPeca, Temp),
	inversoes_p(CInicial, CFinal, Peca, Inversoes),
	Total is Temp + Inversoes.

% Calcula o numero de inversoes associadas a uma determinada Peca.
inversoes_p([Cabeca], _, Cabeca, 0).
inversoes_p([Cabeca|_], _, Cabeca, 0).
inversoes_p([0|Restantes], CFinal, Peca, Inversoes):- inversoes_p(Restantes, CFinal, Peca, Inversoes).
inversoes_p([Cabeca|Restantes], CFinal, Peca, Inversoes):-
	inversoes_p(Restantes, CFinal, Peca, Temp),
	peca(CFinal, PosicaoCabeca, Cabeca),
	peca(CFinal, PosicaoPeca, Peca),
	(PosicaoCabeca > PosicaoPeca ->
	Inversoes is Temp + 1;
	Inversoes = Temp).
	
%
% Predicados Secundarios
%

% Imprime os passos realizados, por ordem.
imprime_passos([]):- write('.').
imprime_passos([[Peca,M]|Restantes]):-
	nome_movimento(M, Movimento),
	nl,
	write('mova a peca '),
	write(Peca),
	write(' para '),
	write(Movimento),
	imprime_passos(Restantes).

% Dada uma transformacao CInicial -> CFinal, escreve os elementos de ambas as configuracoes separados por '->'	
imprime_transf(CInicial, CFinal):-
	nl,
	tamanho(T),
	writeln('Transformacao desejada:'),
	imprime_transf(CInicial, CFinal, 0, T).

imprime_transf(_, _, T, T).
imprime_transf(CInicial, CFinal, 1, T):- imprime_transf(CInicial, CFinal, 1, T, ' ->').
imprime_transf(CInicial, CFinal, Linha, T):- imprime_transf(CInicial, CFinal, Linha, T, '   ').
imprime_transf(CInicial, CFinal, Linha, T, Separador):-
	Lin is Linha+1,
	separa_N(CInicial, T, CInicial1, CInicial2),
	separa_N(CFinal, T, CFinal1, CFinal2),
	imprime_lista(CInicial1, 0, T),
	write(Separador),
	imprime_lista(CFinal1, 0, T),
	nl,
	imprime_transf(CInicial2, CFinal2, Lin, T).

% Dada uma configuracao, escreve todos os elementos dessa mesma configuracao	
imprime_config(Config):-
	nl,
	tamanho(T),
	imprime_config(Config, 0, T).

imprime_config(_, T, T).
imprime_config(Config, Linha, T):-
	Lin is Linha+1,
	separa_N(Config, T, Config1, Config2),
	imprime_lista(Config1, 0, T),
	nl,
	imprime_config(Config2, Lin, T).

% Dada uma lista, escreve os primeiros T elementos separados por um espaco
imprime_lista(_, T, T).
imprime_lista([Cabeca|Cauda], Contador, T):-
	Contador =< T,
	Contador1 is Contador + 1,
	write(' '),
	escreve_digito(Cabeca),
	imprime_lista(Cauda, Contador1, T).

% Escreve o digito dado, substituindo 0 por um espaco
escreve_digito(N):- N \== 0 -> write(N); write(' ').

% Devolve em L1 os primeiros N elementos da lista, em L2 os restantes.
separa_N([Cabeca|Cauda], 0, [], [Cabeca|Cauda]).
separa_N([Cabeca|Cauda], 1, [Cabeca], Cauda).
separa_N([Cabeca|Cauda], N, L1, L2):-
	N > 1,
	N1 is N - 1,
	separa_N(Cauda, N1, Temp, L2),
	append([Cabeca], Temp, L1).

% Devolve em Resultado a Lista sem o elemento de indice N.
remove_N(Lista, N, Resultado):-
	separa_N(Lista, N, L1, [_|L2]),
	append(L1, L2, Resultado).	

% Devolve em Elemento o item na posicao N da Lista.
elemento_N([Cabeca|_], 0, Cabeca).
elemento_N([_|Cauda], N, Elemento):-
	N1 is N - 1,
	elemento_N(Cauda, N1, Elemento).

% Dada uma lista, calcula o numero de elementos
numero_elementos([], 0).
numero_elementos([_|Cauda], Total):-
	numero_elementos(Cauda, Temp),
	Total is Temp + 1.