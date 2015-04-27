% Tamanho do nosso tabuleiro.
tamanho(3).
 
% Jogadas e os seus offsets.
jogada(e, -1).
jogada(d, 1).
jogada(c, N):-
    tamanho(T),
    N is -T.
jogada(b, N):-
    tamanho(N).
 
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

% Verifica se um movimento, que seja direita ou esquerda, nao muda de linha
%valida(PInicial, PFinal, Offset):-
%   (Offset == -1; Offset == 1),
%   tamanho(T),
%   A is (PInicial mod T) + Offset,
%    B is (PFinal mod T),
%    A == B.

valida(PInicial, PFinal, Offset):-
    (Offset == 1; Offset == -1),
    mesma_linha(PInicial, PFinal).

valida(_, _, Offset):-
    (tamanho(Offset); (OffsetInv is -Offset, tamanho(OffsetInv))).
    
resolve_manual(CInicial, CFinal):-
    imprime_transf(CInicial, CFinal).
    
    
    
    
    
    
    
    
    
imprime_transf(CInicial, CFinal):-
    tamanho(T),
    imprime_transf(CInicial, CFinal, 0, T).
    
imprime_transf(CInicial, CFinal, Linha, T):-
    Linha < T,
    Linha == 1,
    Lin is Linha+1,
    primeiros_N(CInicial, T, CInicial1, CInicial2),
    primeiros_N(CFinal, T, CFinal1, CFinal2),
    imprime_linha(CInicial1, 1, T),
    write('\t->\t'),
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
    write('\t\t'),
    imprime_linha(CFinal1, 1, T),
    nl,
    imprime_transf(CInicial2, CFinal2, Lin, T).
    
imprime_transf(_, _, Linha, T):-
    Linha == T.
    
imprime_linha([Cabeca|Cauda], Contador, T):-
    Contador < T,
    Contador1 is Contador + 1,
    write(Cabeca),
    write('\t'),
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