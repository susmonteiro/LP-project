% Susana Monteiro, 92560

:- consult(codigo_comum).

%-----------------------------------------------------------------------------
% 3.1.1 predicado aplica_R1_triplo(Triplo, N_Triplo): aplica R1 ao triplo
%-----------------------------------------------------------------------------

aplica_R1_triplo(T,NT) :- 
    n_variaveis(T,NumVar),
    (NumVar == 0, not(iguais(T)), NT = T, !
                    ;
     NumVar == 1, not(iguais(T)), NT = T, ! 
     % o caso de cima verifica-se quando nao e possivel que o triplo
     % seja composto por tres numeros iguais, por exemplo, [1,_,0]  
                    ;
     NumVar == 1, maplist(substitui_variavel(0),T,NT), not(iguais(NT)), !
                    ;
     NumVar == 1, maplist(substitui_variavel(1),T,NT), not(iguais(NT)), !
                    ;
     NumVar >= 2, NT = T).

% iguais(Triplo): verifica se todos os elementos de um triplo sao iguais
iguais([_|[]]).
iguais([El,El|R]) :- iguais([El|R]), !.

% n_variaveis(Triplo, NumVar): devolve o numero de variaveis num triplo
n_variaveis(T,NumVar) :- include(var,T,VarsT), length(VarsT,NumVar).

% substitui_variavel(Valor, Elemento, NovoElemento): 
% se Elemento for uma variavel, substitui por Valor
substitui_variavel(Val,El,Val) :- var(El), !.
substitui_variavel(_,El,El).


%-----------------------------------------------------------------------------
% 3.1.2 predicado aplica_R1_fila_aux(Fila, N_Fila): 
% aplica uma vez a regra R1 a Fila
%-----------------------------------------------------------------------------

aplica_R1_fila_aux([E1,E2,E3],R) :- aplica_R1_triplo([E1,E2,E3],R), !.
% condicao de paragem: a fila e constituida apenas por um triplo
aplica_R1_fila_aux([E1,E2,E3|L],R) :- 
    aplica_R1_triplo([E1,E2,E3],[R1,R2,R3]),
    aplica_R1_fila_aux([R2,R3|L],RT),
    R = [R1|RT].


%-----------------------------------------------------------------------------
% 3.1.3 predicado aplica_R1_fila(Fila, N_Fila): aplicar a regra R1 
% (aplica_R1_fila_aux) ate que nao haja mais alteracoes a serem feitas
%-----------------------------------------------------------------------------

aplica_R1_fila(L,R) :- aplica_R1_fila(L,R,_).   
% terceiro argumento: guarda a fila, sem alteracoes, 
% para verificar se houve ou nao alguma alteracao
aplica_R1_fila(L,L,L1) :- L == L1, !.
aplica_R1_fila(L,Res,_) :- aplica_R1_fila_aux(L,R), aplica_R1_fila(R,Res,L).


%-----------------------------------------------------------------------------
% 3.1.4 predicado aplica_R2_fila(Fila, N_Fila): aplica a regra R2 a Fila
%-----------------------------------------------------------------------------

aplica_R2_fila(F,NF) :-
    length(F,L), conta_numero(F,0,N0), conta_numero(F,1,N1), MaxN is L/2,
    (N0 < MaxN, N1 < MaxN, NF = F, !
                ; 
     N0 == MaxN, maplist(substitui_variavel(1),F,NF), !
                ;   % a funcao substitui_variavel esta definida em 3.1.1
     N1 == MaxN, maplist(substitui_variavel(0),F,NF)).

% conta_numero(Fila, Valor, NumeroOcorrencias): 
% devolve quantas vezes Valor aparece na lista Fila
conta_numero(F,V,N) :- include(==(V),F,F_com_V), length(F_com_V,N).


%-----------------------------------------------------------------------------
% 3.1.5 predicado aplica_R2_fila(Fila, N_Fila): aplica R1 e R2 a Fila
%-----------------------------------------------------------------------------

aplica_R1_R2_fila(F,NF) :-
    aplica_R1_fila(F,Temp),
    aplica_R2_fila(Temp,NF).    


%-----------------------------------------------------------------------------
% 3.1.6 predicado aplica_R1_R2_puzzle(Puz, N_Puz): aplica R1 e R2 ao Puz
%-----------------------------------------------------------------------------

aplica_R1_R2_puzzle(P,NP) :-
    mat_dimensoes(P,D,D),
    mat_transposta(P,T),
    aplica_R1_R2_colunas(T,D,T1),   
    % para aplicar R1 e R2 as linhas do puzzle, transpoe-se a matriz
    % e depois aplica-se R1 e R2 as colunas dessa matriz transposta
    mat_transposta(T1,P1),
    aplica_R1_R2_colunas(P1,D,NP).

% aplica_R1_R2_colunas(Puzzle, Dimensao, Novo_Puzzle):
% aplica, iterativamente, as regras R1 e R2 a todas as colunas do puzzle
aplica_R1_R2_colunas(P,D,NP) :- aplica_R1_R2_colunas(P,1,D,NP).
aplica_R1_R2_colunas(P,N,D,P) :- N > D, !.
aplica_R1_R2_colunas(P,N,D,NP) :-
    N1 is N + 1,
    mat_elementos_coluna(P,N,F),
    aplica_R1_R2_fila(F,NF),
    mat_muda_coluna(P,N,NF,Temp),
    aplica_R1_R2_colunas(Temp,N1,D,NP).


%-----------------------------------------------------------------------------
% 3.1.7 predicado inicializa(Puz, N_Puz):
% aplica R1 e R2 ao Puz ate nao serem preenchidas novas posicoes
%-----------------------------------------------------------------------------

inicializa(P,NP) :- inicializa(P,NP,_).
inicializa(P,P,P1) :- P =@= P1, !.
inicializa(P,Res,_) :- aplica_R1_R2_puzzle(P,NP), inicializa(NP,Res,P).


%-----------------------------------------------------------------------------
% 3.2 predicado verifica_R3(Puz): aplica a regra R3 ao Puz, isto e,
% verifica se todas as linhas (respetivamente colunas) sao diferentes entre si
%-----------------------------------------------------------------------------

verifica_R3(P) :- 
    verifica_R3_linhas(P),
    % aplicar R3 as linhas do puzzle e o mesmo que 
    % aplicar R3 as colunas do puzzle transposto
    mat_transposta(P,T),
    verifica_R3_linhas(T).

% verifica_R3_linhas(Puzzle): verifica se todas as linhas do Puzzle sao diferentes
verifica_R3_linhas([]).
verifica_R3_linhas([L|P]) :-
    maplist(\==(L),P),
    verifica_R3_linhas(P).


%-----------------------------------------------------------------------------
% 3.3 predicado propaga_posicoes(Posicoes, Puz, N_Puz):
% N_Puz e o resultado de propagar, recursivamente, as mudancas de Posicoes
%-----------------------------------------------------------------------------

propaga_posicoes([],P,P).
propaga_posicoes([(L,C)|R],P,NP) :- 
    mat_transposta(P,T),
    % propagar mudancas as linhas de um puzzle e equivalente a  
    % propagar mudancas do puzzle transposto
    propaga_fila(T,L,T1,LPos),
    maplist(posicao_linha(L),LPos,NPosL),
    append(NPosL,R,R1),
    mat_transposta(T1,P1),
    propaga_fila(P1,C,P2,CPos),
    maplist(posicao_coluna(C),CPos,NPosC),   
    append(NPosC,R1,R2),
    propaga_posicoes(R2,P2,NP).


% propaga_fila(Puz,Indice_Coluna,N_Puz,Posicoes): 
% aplica R1 e R2 aos elementos da coluna C e devolve o N_Puzzle e o 
% vetor de posicoes, que contem as novas posicoes a serem propagadas
propaga_fila(P,C,NP,Pos) :-
    mat_elementos_coluna(P,C,F),
    aplica_R1_R2_fila(F,NF),
    mat_muda_coluna(P,C,NF,NP),
    diferencas(F,NF,Pos).
    
% diferencas(Fila,N_Fila,Posicoes): devolve o vetor com o conjunto de 
% linhas/colunas de uma determinada coluna/linha que vao ser propagadas
diferencas(F,NF,Pos) :- %Pos guarda as posicoes a propagar pela ordem inversa
    length(F,L),
    diferencas(F,NF,[],Pos,1,L).
diferencas([],[],Pos,Pos,_,_).
diferencas([E1|F],[E2|NF],Pos,NPos,N,L) :-
    N1 is N + 1,
    (E1 == E2
        ->
    diferencas(F,NF,Pos,NPos,N1,L)
        ;
    diferencas(F,NF,[N|Pos],NPos,N1,L)).

% posicao_linha(Valor,Elemento,Coordenada): transforma o indice da linha
% numa nova coordenada (Linha,Coluna)
posicao_linha(Val,El,(Val,El)).

% posicao_coluna(Valor,Elemento,Coordenada): transforma o indice da coluna
% numa nova coordenada (Linha,Coluna)
posicao_coluna(Val,El,(El,Val)).


%-----------------------------------------------------------------------------
% 3.4 predicado resolve(Puz,Sol): Sol e uma solucao do Puz
%-----------------------------------------------------------------------------

resolve(Puz, Sol) :- 
    inicializa(Puz,Puz_Ini),
    verifica_R3(Puz_Ini),
    resolve_aux(Puz_Ini, Sol),
    verifica_R3(Sol), !.

% resolve_aux(Puz, Sol): enquanto existirem variaveis em Puz, 
% tenta unifica-las com 0 ou com 1 e propagar a alteracao 
resolve_aux(Puz,Puz) :- completo(Puz), !.
resolve_aux(Puz, Sol) :- 
    primeira_variavel(Puz, Pos),
    mat_muda_posicao(Puz,Pos,0,P1), 
    propaga_posicoes([Pos],P1,P2), 
    resolve_aux(P2,Sol), !.
resolve_aux(Puz, Sol) :- 
    primeira_variavel(Puz, Pos),
    mat_muda_posicao(Puz,Pos,1,P1),
    propaga_posicoes([Pos],P1,P2),
    resolve_aux(P2,Sol).

% completo(Puz): verifica se nao ha mais variaveis em Puz
completo([]).
completo([E|R]) :- 
    (is_list(E)
        -> 
    completo(E), completo(R)
        ;
    not(var(E)), completo(R)).

% primeira_variavel(Puz,Pos): procura a primeira variavel no Puz 
% e devolve a sua coordenada (Linha,Coluna)
primeira_variavel(P,Pos) :- 
    mat_dimensoes(P,D,D),
    primeira_variavel(P,1,1,D,Pos).

primeira_variavel(_,_,_,_,Pos) :- not(var(Pos)), !.
primeira_variavel([E|_],L,C,_,(L,C)) :- var(E), !.
primeira_variavel(_,_,C,D,_) :- C > D.
primeira_variavel([E|R],L,C,D,Pos) :- 
    is_list(E), 
    primeira_variavel(E,L,C,D,Pos), 
    L1 is L + 1, 
    primeira_variavel(R,L1,C,D,Pos), !.
primeira_variavel([_|R],L,C,D,Pos) :- C1 is C + 1, primeira_variavel(R,L,C1,D,Pos).
