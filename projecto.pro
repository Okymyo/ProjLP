vazio([]).

% Tamanho do nosso tabuleiro.
tamanho(3).

% Ordem da procura cega.
ordem_cega([b, d, c, e]).
 
% Jogadas e os seus offsets.
jogada(e, -1).
jogada(d, 1).
jogada(c, N):-
	tamanho(T),
	N is -T.
jogada(b, N):-
	tamanho(N).
	
% Jogadas e os seus nomes.
nome_movimento(c, 'cima').
nome_movimento(b, 'baixo').
nome_movimento(e, 'a esquerda').
nome_movimento(d, 'a direita').
 
% Verifica se a Peca esta numa dada Posicao num dado Tabuleiro.
% Podes fazer perguntas do genero peca([1, 2, 3], P, 2). e ele responde P = 1.
peca(Tabuleiro, Posicao, Peca):-
	peca(Tabuleiro, Posicao, Peca, 0).
peca([Peca|_], Posicao, Peca, Acc):-
	Posicao is Acc.
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

% Valida os movimentos horizontais
valida(PInicial, PFinal, Offset):-
	(Offset == 1; Offset == -1),
	mesma_linha(PInicial, PFinal).

% Valida os movimentos verticais
valida(_, _, Offset):-
	(tamanho(Offset); (OffsetInv is -Offset, tamanho(OffsetInv))).
	

resolve_manual(CInicial, CFinal):-
	imprime_transf(CInicial, CFinal),
	pede_input(M),
	resolve_manual(CInicial, CFinal, M).
	
resolve_manual(CInicial, CFinal, M):-
	mov_legal(CInicial, M, _, Resultado),
	Resultado \= CFinal,
	imprime_transf(CInicial, Resultado),
	pede_input(M2),
	resolve_manual(Resultado, CFinal, M2).
	
resolve_manual(CInicial, CFinal, M):-
	mov_legal(CInicial, M, _, Resultado),
	Resultado = CFinal,
	imprime_transf(CInicial, Resultado),
	write('DING DING DING').
	
resolve_manual(CInicial, CFinal, M):-
	not(mov_legal(CInicial, M, _, _)),
	imprime_transf(CInicial, CInicial),
	pede_input(M2),
	resolve_manual(CInicial, CFinal, M2).
	
pede_input(M):-
	writeln('Qual o seu movimento?'),
	read(M).
	
% Procura cega!
resolve_cego(CInicial, CFinal):-
	imprime_transf(CInicial, CFinal),
	ordem_cega(OC),
	resolve_cego(CInicial, CFinal, OC, [], [], Solucao),
	imprime_passos(Solucao).
	
resolve_cego(CInicial, CFinal, [M|_], Anteriores, Movimentos, Solucao):-
	mov_legal(CInicial, M, Peca, Resultado),
	not(Resultado == CFinal),
	not(na_lista(Anteriores, Resultado)),
	append([Resultado], Anteriores, Anteriores2),
	ordem_cega(OC),
	append(Movimentos, [[Peca, M]], Temp),
	resolve_cego(Resultado, CFinal, OC, Anteriores2, Temp, Solucao).
	
resolve_cego(CInicial, CFinal, [M|_], _, Movimentos, Solucao):-
	mov_legal(CInicial, M, Peca, Resultado),
	Resultado == CFinal,
	append(Movimentos, [[Peca, M]], Solucao).
	
resolve_cego(CInicial, CFinal, [_|Restantes], Anteriores, Movimentos, Solucao):-
	%((not(mov_legal(CInicial, M, _, Resultado))); (mov_legal(CInicial, M, _, Resultado), na_lista(Anteriores, Resultado))),
	resolve_cego(CInicial, CFinal, Restantes, Anteriores, Movimentos, Solucao).
	
imprime_passos([]):-
	write('.').
imprime_passos([[Peca,M]|Restantes]):-
	nome_movimento(M, Movimento),
	nl,
	write('mova a peca '),
	write(Peca),
	write(' para '),
	write(Movimento),
	imprime_passos(Restantes).
	
na_lista([Cabeca|Cauda], Item):-
	Cabeca == Item;
	na_lista(Cauda, Item).
	
imprime_transf(CInicial, CFinal):-
	tamanho(T),
	imprime_transf(CInicial, CFinal, 0, T),
	nl.
	
imprime_transf(CInicial, CFinal, Linha, T):-
	Linha < T,
	Linha == 1,
	Lin is Linha+1,
	primeiros_N(CInicial, T, CInicial1, CInicial2),
	primeiros_N(CFinal, T, CFinal1, CFinal2),
	imprime_linha(CInicial1, 1, T),
	write(' -> '),
	imprime_linha(CFinal1, 1, T),
	nl,
	imprime_transf(CInicial2, CFinal2, Lin, T).
	
imprime_transf(CInicial, CFinal, Linha, T):-
	Linha < T,
	(Linha > 1; Linha < 1),
	Lin is Linha+1,
	primeiros_N(CInicial, T, CInicial1, CInicial2),
	primeiros_N(CFinal, T, CFinal1, CFinal2),
	imprime_linha(CInicial1, 1, T),
	write('    '),
	imprime_linha(CFinal1, 1, T),
	nl,
	imprime_transf(CInicial2, CFinal2, Lin, T).
	
imprime_transf(_, _, Linha, T):-
	Linha == T.
	
imprime_linha([Cabeca|Cauda], Contador, T):-
	Contador < T,
	Contador1 is Contador + 1,
	write(Cabeca),
	write(' '),
	imprime_linha(Cauda, Contador1, T).
	
imprime_linha([Cabeca|_], Contador, T):-
	Contador == T,
	write(Cabeca).
	
primeiros_N([Cabeca|Cauda], N, L1, L2):-
	N > 1,
	N1 is N - 1,
	primeiros_N(Cauda, N1, Temp, L2),
	append([Cabeca], Temp, L1).

primeiros_N([Cabeca|Cauda], N, L1, L2):-
	N == 1,
	L1 = [Cabeca],
	L2 = Cauda.