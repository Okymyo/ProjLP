% Tamanho do nosso tabuleiro.
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
	resolve_info_m(CFinal, Abertos1, Fechados1, Solucao).
	
filtra_sucessores([], Abertos, _, Abertos).
filtra_sucessores([No|Restantes], Abertos, Fechados, Resultado):-
	[Configuracao|_] = No,
	not(configuracao_calculada(Abertos, Configuracao)),
	not(configuracao_calculada(Fechados, Configuracao)),
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
	
/*resolve_info_m(CInicial, CFinal):-
	imprime_transf(CInicial, CFinal),
	!,
	resolve_info_m(CInicial, CFinal, [], [], Solucao),
	open('output2.txt', write, Stream),
	imprime_passos(Solucao, Stream),
	close(Stream).
	
resolve_info_m(CInicial, CFinal, _, Movimentos, Solucao):-
	CInicial == CFinal,
	Solucao = Movimentos.
	
resolve_info_m(CInicial, CFinal, Anteriores, Movimentos, Solucao):-
	melhor_movimento(CInicial, CFinal, Anteriores, Peca, Movimento),
	mov_legal(CInicial, Movimento, Peca, Resultado),
	append(Movimentos, [[Peca, Movimento]], Temp),
	append([Resultado], Anteriores, Anteriores2),
	resolve_info_m(Resultado, CFinal, Anteriores2, Temp, Solucao).*/

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

/*% Calcula o melhor movimento a realizar
melhor_movimento(CInicial, CFinal, Anteriores, Peca, Movimento):-
	ordem(Ordem),
	dist_manhattan(CInicial, CFinal, Distancia),
	melhor_movimento(CInicial, CFinal, Anteriores, Ordem, 'sem_peca', 'sem_movimento', Distancia, Peca, Movimento).
	
melhor_movimento(_, _, _, [], MelhorPecaTemp, MelhorMovTemp, _, MelhorPecaTemp, MelhorMovTemp):- MelhorPecaTemp \= 'sem_peca'.

melhor_movimento(CInicial, CFinal, Anteriores, [M|Restantes], _, _, DistTemp, Peca, Movimento):-
	mov_legal(CInicial, M, P, Temp),
	not(na_lista(Anteriores, Temp)),
	dist_manhattan(Temp, CFinal, Distancia),
	Distancia < DistTemp,
	melhor_movimento(CInicial, CFinal, Anteriores, Restantes, P, M, Distancia, Peca, Movimento).
	
melhor_movimento(CInicial, CFinal, Anteriores, [_|Restantes], MelhorPecaTemp, MelhorMovTemp, DistTemp, Peca, Movimento):-
	melhor_movimento(CInicial, CFinal, Anteriores, Restantes, MelhorPecaTemp, MelhorMovTemp, DistTemp, Peca, Movimento).*/
	
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
	
% Verifica se um determinado Item se encontra numa dada lista.
na_lista([Cabeca|Cauda], Item):-
	Cabeca == Item;
	na_lista(Cauda, Item).
	
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
escreve_digito(N):- N \== 0 -> write( N) ; write(' ').

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