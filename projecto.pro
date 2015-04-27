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
    linha(Posicao, Linha, 0).
linha(Posicao, Linha, Acc):-
        tamanho(N),
        Posicao >= Acc*N+0,
    Posicao < Acc*N+N,
    Linha is Acc.  
linha(Posicao, Linha, Acc):-
    tamanho(N),
    Acc < N,
    Acc1 is Acc+1,
    linha(Posicao, Linha, Acc1).
 
% Verifica se Posicao1 esta na mesma linha que a Posicao2.
% Este predicado apenas verifica! Nao instancia nada! Nao podes fazer perguntas mesma_linha(1, P).
mesma_linha(Posicao1, Posicao2):-
    linha(Posicao1, Linha1),
    linha(Posicao2, Linha2),
    Linha1 == Linha2.
 
% Verificar se o movimento for direita ou esquerda se Posicao e Posicao+offset estao na mesma linha!
mov_legal(CInicial, Jogada, Peca, CFinal):-
    peca(CInicial, Posicao, Peca),
    jogada(Jogada, Offset),
    Posicao1 is Posicao+Offset,
    peca(CInicial, Posicao1, Peca1),
    Peca1 = 0,
    troca(CInicial, Peca, 0, CFinal).