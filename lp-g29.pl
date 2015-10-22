%grupo 29
%Miguel Reis 81630
%Nelson Nunes 81159
%_______________________________________________________________________________
%funcoes_aux_listas.

%pos( elemento, Lista, indice do elemento na lista).
%Fornecido um elemento e uma lista, devolve o indice do mesmo.
%Fornecido um indice e uma lista, devolve o elemento.
pos(EL,List,Ind):- pos(EL,List,Ind,Ind).
pos(El,[El|_],_, 1).
pos(El, [_|R],Ind,Ind):- pos(El, R ,Ind, Ind1),Ind is Ind1+1.


%Insere_el(Lista, El, Ind, Lista_nova).
%Recebe uma lista e devolve uma nova, na qual no indice Ind insere o valor El
insere_el([_|R], El , 1 , [El|R]).
insere_el([P|R], El, Ind, [P|R1]):- Ind>1,
                                    Ind1 is Ind -1,
                                    insere_el(R, El, Ind1, R1).

%insere_lista_dupla(Lista,Peca, Mov, Lista_final).
%Recebe uma peca e um movimento, cria uma lista com estas e insere numa lista dupla Inicial.
insere_lista_dupla([],P1,M1,[[P1,M1]]).
insere_lista_dupla([P|R],N1,M1,[P|L1]):-insere_lista_dupla(R,N1,M1,L1).



%troca_el(Lista, indice1, Indice2, Lista com elementos trocados).
%Recebe uma lista e troca os valores.
troca_el(List, N1, N2,List3):- copia_lista(List, L1),
                               copia_lista(List,L2),
                               pos(El1,L1, N1),
                               pos(El2,L2, N2),
                               insere_el(List, El1, N2,List2),
                               insere_el(List2, El2, N1,List3).
%mov_in_conf(Lista,El).
%verifica se um elemento se encontra na lista.
mov_in_conf([El|_],El).
mov_in_conf([_|R],El):-mov_in_conf(R,El).

%funcoes basicas de add e copy.
insere_na_lista(L1,E1,[E1|L1]).
copia_lista(L1,L1).

%___________________________________________________________________
%Output_funcions.
escreve_inicial([A, B, C, D, E, F, G, H, I],
              [J, K, L, M, N, O, P, Q, R]) :-
      write('Transformacao desejada:'), nl,
      escreve(A), escreve(B), escreve(C),
      write('    '),
       escreve(J),  escreve(K),  escreve(L),nl,
       escreve(D),  escreve(E),  escreve(F),
      write(' -> '),
       escreve(M),  escreve(N),  escreve(O), nl,
       escreve(G),  escreve(H),  escreve(I),
      write('    '),
      escreve(P), escreve(Q), escreve(R), nl.

escreve([A, B, C, D, E, F, G, H, I]) :- escreve(A), escreve(B), escreve(C),nl,
                                        escreve(D), escreve(E), escreve(F),nl,
                                        escreve(G), escreve(H), escreve(I),nl.
escreve(S) :- S = 0, write('   ').
escreve(S) :- S < 10, write(' '), write(S), write(' ').
                                                          
escrita_movs([[P,M] | []]) :- write('mova a peca '),
                                  write(P),
                                  traduz(M, Mp),
                                  write(Mp),
                                  write('.'),
                                  nl.
escrita_movs([[P,M] | R]) :- write('mova a peca '),
                                 write(P),
                                 traduz(M, Mp),
                                 write(Mp),
                                 nl,
                                 escrita_movs(R).
traduz(c, ' para cima').
traduz(b, ' para baixo').
traduz(e, ' para a esquerda').
traduz(d, ' para a direita').
%_______________________________________________________________________________

%mov_legal(L,M,_,_)-> verifica se o Mov é legal.
%mov_legal(L,M,P,c)-> devolve a Peca e Configuracao que foi mudada.
%mov_legal testa os movimentos pela ordem [c,b,e,d].

mov_legal(C1,M,P,C2):- mov_cima(C1,P,C2), M=c;
                       mov_baixo(C1,P,C2), M=b;
                       mov_esquerda(C1,P,C2), M=e;
                       mov_direita(C1,P,C2), M=d.
                       
mov_cima(C1,P,C2):- pos(0,C1,P1),P1<7, P2 is P1+3, troca_el(C1,P2,P1,C2),pos(P,C1,P2).                           % Descobre o Indice do 0, verifica que pode subir, desce o indice(+3), troca a peca e calcula a peca mudada.
mov_baixo(C1,P,C2):- pos(0,C1,P1),P1>3, P2 is P1-3, troca_el(C1,P2,P1,C2), pos(P,C1,P2).                         % Descobre o Indice do 0, verifica o que pode descer, desce o indice(-3), troca a peca e calcula a peca mudada.
mov_direita(C1,P,C2):- pos(0,C1,P1),(P1=\=1,P1=\=4,P1=\=7), P2 is P1-1, troca_el(C1,P2,P1,C2), pos(P,C1,P2).     % Descobre o Indice do 0, verifica que nao esta na 1 coluna, desce o indice(-1), troca a peca e calcula a peca mudada.
mov_esquerda(C1,P,C2):- pos(0,C1,P1), (P1=\=3,P1=\=6,P1=\=9), P2 is P1+1, troca_el(C1,P2,P1,C2), pos(P,C1,P2).   % Descobre o Indice do 0, verifica que nao esta na 3 coluna, desce o indice(+1), troca a peca e calcula a peca mudada.
%_______________________________________________________________________________

resolve_manual(Conf_1,Conf_2):- \+ transformacao_possivel(Conf_1,Conf_2), writeln('Nao tem solucao').          %Verifica se existe solucao.
resolve_manual(Conf_1,Conf_2):-escreve_inicial(Conf_1,Conf_2), resolve_manual_loop(Conf_1,Conf_2).             %Escreve ecra inicial e inicia o loop de jogo.
resolve_manual_loop(Conf_1,Conf_1) :- writeln('Parabens!').                                                    %Caso de paragem, configuracao alvo atingida, escreve Parabens.
resolve_manual_loop(Conf_1,Conf_2):- writeln('Qual o seu movimento?'),read(Mov),                               %Lê o movimento de input pretendido.
                                     mov_legal(Conf_1,Mov,_,_), mov_legal(Conf_1,Mov,_,List),                  %Verifica se este movimento gera um configuracao possivel e executa o movimento se sim.
                                     nl,escreve(List),nl,resolve_manual_loop(List,Conf_2).                     %Imprime a lista e inicia a proxima interacao.
resolve_manual_loop(Conf_1,Conf_2):- writeln('Movimento ilegal'), resolve_manual_loop(Conf_1,Conf_2).          %Se um movimento nao gerar um configuracao possivel, este imprime movimento ilegal e inicia a mesma interacao.
%_______________________________________________________________________________

resolve_cego(Conf_1,Conf_2):- \+ transformacao_possivel(Conf_1,Conf_2), writeln('Nao tem solucao').          % Verifica se existe solucao.
resolve_cego(Conf_1,Conf_2):- escreve_inicial(Conf_1,Conf_2), resolve_cego_loop(Conf_1,Conf_2,[],[Conf_1]).  % Escreve ecra inicial e inica o loop de procura.
resolve_cego_loop(Conf_2,Conf_2,Movs,_):- escrita_movs(Movs).                                                        % Caso de paragem, configuracao alvo atingida, escreve solucao pretendida.
resolve_cego_loop(Conf_1,Conf_2,Movs,Confs):- mov_legal(Conf_1, Mov, P, Conf_actual),                                    % Gera um movimento legal.
                                              \+ mov_in_conf(Confs,Conf_actual),                                     % Verifica se gera um configuracao nova, nunca anteriormente atinjida.
                                              insere_lista_dupla(Movs,P,Mov,Movs_new),                               % Insere os movimentos na lista de Movs feitos.
                                              insere_na_lista(Confs,Conf_actual,Conf_new),                           % Insere a configuracao na lista de Confs ja atinjidas.
                                              resolve_cego_loop(Conf_actual,Conf_2,Movs_new,Conf_new),!.                 % Actualiza configuracao actual, lista de Movs e de configuracoes e inicia a proxima iteracao, ponto de corte necessario.

%______________________________________________________________________________________
%distancia_hamming_h(Conf,Configuracao_final,Heuteristica).
%devolve o valor de H de Conf em relacao a Configuracao_final

distancia_hamming_h([],[],0).
distancia_hamming_h([0|R],[_|R1],Dist):-distancia_hamming_h(R,R1,Dist).%o 0 nao conta como peca fora do lugar
distancia_hamming_h([P|R],[P1|R1],Dist):- \+copia_lista(P,P1),!,          % se P != P1, aumenta contador.
                                          distancia_hamming_h(R,R1,Dist1),
                                          Dist is Dist1 +1.
distancia_hamming_h([P|R],[P|R1],Dist):-P=\=0,distancia_hamming_h(R,R1,Dist). %P!=0, e iguais->nao esta fora do lugar

%insere_list_no(C,F,G,H,M,[],[(C,F,G,H,M)])
%insere caracteristicas de um no, como no no inicio da lista
insere_list_no(C,F,G,H,M,[],[(C,F,G,H,M)]).
insere_list_no(C,F,G,H,M,[P|R],[(C,F,G,H,M),P|R]).

%tira_da_lista(List,EL,Res). retira da lista List, o elemento EL, e retorna a lista sem o elemento.
tira_da_lista([P|R],P,R).
tira_da_lista([P|R],El,[P|R1]):- tira_da_lista(R,El,R1).

%funcoes para retornar, uma dada caracterstica do no. No=(C,F,G,H,M)
no_c((C,_,_,_,_),C).
no_f((_,F,_,_,_),F).
no_g((_,_,G,_,_),G).
no_h((_,_,_,H,_),H).
no_m((_,_,_,_,M),M).

%menor_valor_f(List_nos,No), retora uma copia do no na lista List, com menor valor de F
menor_valor_f([P|R],Final):-menor_valor_f(R,P,Final).
menor_valor_f([],Final,Final):-!.
menor_valor_f([P|R],No,Final):-no_f(P,Val1), no_f(No,Val2),Val1<Val2, menor_valor_f(R,P,Final). %se o valor for menor, atualiza o no
menor_valor_f([_|R],No,Final):-menor_valor_f(R,No,Final).                                       %se nao for menor, percorre o resto d alista

%no_na_lista(No,List). se o No, fizer parte da lista, devolve True. Se nao fizer parte, Devolve False.
no_na_lista(No,[No|_]):-!.
no_na_lista(No,[_|R]):- no_na_lista(No,R).
%__________________________________________________________________________________________________________________________________________________________
%resolve_info_h(Conf1,Conf1), recebe dois tabuleiros, e utiliza um algoritmo, com a distancia de hamming, para calcular os movimentos necessarios para chegar a solucao
resolve_info_h(C1,C2):- \+ transformacao_possivel(C1,C2), writeln('Nao tem solucao').  %verifica se as configuracoes que recebe teem solucao.
resolve_info_h(C1,C2):-escreve_inicial(C1,C2),distancia_hamming_h(C1,C2,H),            % escreve os tabuleiros iniciais, e calcula o valor de H do primeiro no
                       resolve_info_h(C2,[(C1,H,0,H,[])],[],(C1,H,0,H,[])).            % e insere o no na lista de abertos. chama de novo a funcao com os valores iniciais ja definidos.
resolve_info_h(C2,Abert,Fech,No):- (\+no_h(No,0)),!,                                   %se o valor do H do no nao for zero, continua.
                                   no_c(No,C),no_g(No,G), no_m(No,M), G1 is G+1,       %Calcula a configuracao nova(C), o numero de pecas alteradas(G) e os movimentos(M) dessa conf ate
                                                                                       %ate ao momento. adiciona um ao valor de G, pois vamos gerar mais um movimento a configuracao atual.
                                   nos_gerados(C,G1,M,L_nos,C2),                       %devolve lista dos nos gerados a partir da config. C, do G1, e do M + novo movimento.
                                   insere_nos_abert(L_nos,Abert,Fech,Abert2),          %se os Nos nao estiverem nos abertos e fechas, insere nos gerados na nova lista de abertos

                                   menor_valor_f(Abert2,Next_No),                      %o algorito calcula o proximo No (com menor valor de F), a expandir.
                                   tira_da_lista(Abert2,Next_No,Abert_final),insere_na_lista(Fech,Next_No,Fech2), %tira dos abertos, o no que vai expadir, e insere nos fechados
                                   resolve_info_h(C2,Abert_final,Fech2,Next_No).           %continua a chamar a funcao

resolve_info_h(_,_,_,No):-no_m(No,Movs),escrita_movs(Movs). %quando H==0, escreve os movs desde a config inicial, ate este no, pois e a solucao.

%insere_nos_abert(List_nos,Abert,Fech, Abertfinal). insere os nos da lista "List_nos" nos abertos, se os abertos e os fechados nao os conterem. devolve a lista aberta nova.
insere_nos_abert([],Abertfinal,_,Abertfinal).
insere_nos_abert([P|R],Abert1,Fech1,Abertfinal):-(\+no_na_lista(P,Abert1),\+no_na_lista(P,Fech1)),!, %se nao estiver nos abertos nem nos fechados, insere nos abertos.
                                                   insere_na_lista(Abert1,P,Abert2),
                                                   insere_nos_abert(R,Abert2,Fech1,Abertfinal).
insere_nos_abert([_|R],Abert1,Fech1,Abertfinal):-insere_nos_abert(R,Abert1,Fech1,Abertfinal).    %se nao tiver, continua a percorrer a lista dos nos


%conf_geradas(Conf, C1, M1, P1). a partir da configuracao Conf, retorna 3 listas contendo as confs geradas, movimentos, e pecas respetivamente,
conf_geradas(C,C1,M1,P1):-findall(C2,mov_legal(C,_,_,C2),C1), findall(M2,mov_legal(C,M2,_,_),M1), findall(P2,mov_legal(C,_,P2,_),P1).%mov_legal(Conf,Mov,Peca,Conffinal)

nos_gerados(C,G1,M,L_nos,C_final):- conf_geradas(C,C1,M1,P1),loop(C1,G1,M,M1,P1,L_nos,C_final).

%loop(L1,G,M,L2,L3,o predicado loop, une os indices das 3 listas, que correspondem aos movimentos de pecas, e configuracoes que se interligam.
loop([P1|R1],G1,M,[P2|R2],[P3|R3],L,C_Final):-loop([P1|R1],G1,M,[P2|R2],[P3|R3],[],L,C_Final).
loop([],_,_,_,_,L,L,_).    %quando L1 fica vazio, tambem fica L2,L3. pois sao listas interligadas de tamanho igual,
loop([P1|R1],G1,M,[P2|R2],[P3|R3],L,L1,C_Final):-insere_lista_dupla(M,P3,P2,M_TOTAL),             %actualiza o M do novo no, inserindo o movimento feito
                                                 distancia_hamming_h(P1,C_Final,H1),F1 is G1+H1,  %calcula o F do no
                                                 insere_list_no(P1,F1,G1,H1,M_TOTAL,L,L_nos),     %cria o no e adiciona-o
                                                 loop(R1,G1,M,R2,R3,L_nos,L1,C_Final).            %continua ate nao haver nos nas listas

%______________________________________________________________________________________________________________________________________
%Optamos pelo seguinte algoritmo.(http://goo.gl/2MKWVB).

%contador total(Configuracao,Caracteristica).
%Calcula e soma as diferentes caracteristicas de cada peca, calculando assim a caracteristica total do tabuleiro(CONT.
contador_total(Conf,Car_total):-contador_total(Conf,Conf,Car_total).
contador_total([A,B,C,D,E,F,G,H,I],Conf,Car_total):- contador(Conf,1,A,Car_1),
                                                contador(Conf,2,B,Car_2),
                                                contador(Conf,3,C,Car_3),
                                                contador(Conf,4,D,Car_4),
                                                contador(Conf,5,E,Car_5),
                                                contador(Conf,6,F,Car_6),
                                                contador(Conf,7,G,Car_7),
                                                contador(Conf,8,H,Car_8),
                                                contador(Conf,9,I,Car_9),
                                                Car_total is (Car_1+Car_2+Car_3+Car_4+Car_5+Car_6+Car_7+Car_8+Car_9).
%cria_lista(Numero,Lista)
%cria_lista de numeros, de 1 ate N. Ex: [1,2,3,...,N].
cria_lista(Num,List):-cria_lista_2(Num,List,[]).
cria_lista_2(0,List,List).
cria_lista_2(Num,List,Aux):- Num1 is Num-1, cria_lista_2(Num1,List,[Num|Aux]).

%compara_listas(Lista1,Lista2, Numero).
%compara_listas recebe duas listas e devolve o numero de elementos iguais.
compara_listas(List1,List2,Num):-compara_listas_loop(List1,List2,Num,0).
compara_listas_loop([],_,Num,Num).
compara_listas_loop([P1|R1],List2,Num,Aux):- member(P1,List2) -> Num1 is Aux+1, compara_listas_loop(R1,List2,Num,Num1); compara_listas_loop(R1,List2,Num,Aux),!.

%conf_lista(Configuracao, Indice, Lista)
%conf_lista devolve numeros ate ao indice, corte a configuracao com o indice fornecido.
conf_lista(Conf,Num,List):- conf_loop(Conf,Num,List,[]).
conf_loop(_,0,List,List).
conf_loop([P1|R1],Num,List,List_1):- Num_1 is Num-1, conf_loop(R1,Num_1,List,[P1|List_1]),!.

%contador(Configuracao,Indice,Elemento,Caracteristica).
%Para um dado Elemento, que ocupa um dado Indice da configuracao, calculasse a caracteristica dessa peca.
%Ao valor do indice, ele subtrai 1 e por cada numero inferior a este repetido subtrai 1.
contador(_,_,0,0).
contador(C,I,EL,CONTADOR):- EL1 is EL-1, I1 is I-1, cria_lista(EL1,L1), conf_lista(C,I1,L2), compara_listas(L1,L2,N), CONTADOR is EL1-N,!.

%Se o resto da divisao de um numero por 2 for 0, entao este e par.
par(NUM):- NUM mod 2 =:= 0.


%transformacao_possivel(configuracao1,configuracao2).
%transformacao_possivel recebe 2 configuracoes, calcula a caracteristica de ambas e se a soma de ambas for par
%significa que e possivel pois significa que as caracteristicas de cada tabuleiro sao ambas par ou ambas impar.
transformacao_possivel(Conf_1,Conf_2):- contador_total(Conf_1,Num_1),
                                contador_total(Conf_2,Num_2),
                                N_total is Num_1+Num_2,!,
                                par(N_total).