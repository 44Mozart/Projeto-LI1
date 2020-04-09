-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2018li1g086 where

import LI11819
import Tarefa0_2018li1g086
import Tarefa1_2018li1g086
import Tarefa2_2018li1g086
import Tarefa3_2018li1g086


-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um 'Estado'.
testesT4 :: [Estado]
testesT4 = [(Estado (mapaInicial (13,13)) [Jogador (2,3) D 5 5 4, Jogador (2,5) C 2 5 4, Jogador (6,3) D 5 5 5, Jogador (6,5) E 2 5 5] [DisparoCanhao 0 (2,4) D, DisparoCanhao 2 (6,4) D, DisparoCanhao 3 (6,4) E]),
            (Estado (mapaInicial (13,13)) [Jogador (2,3) D 5 5 4, Jogador (2,7) C 2 5 4] [DisparoLaser 0 (2,4) D]),
            (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                     [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
              [Jogador (3,5) E 5 5 5, Jogador (6,2) D 4 3 2, Jogador (9,7) D 5 5 5, Jogador (9,9) E 5 5 5]
              [DisparoCanhao 0 (3,4) E, DisparoCanhao 2 (9,8) D, DisparoCanhao 3 (9,8) E]),
              (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
              [Jogador (3,1) D 5 5 5, Jogador (9,8) E 5 5 5, Jogador (6,9) C 5 5 5, Jogador (6,4) B 5 5 5]
              [DisparoCanhao 0 (3,2) D, DisparoLaser 1 (9,7) E, DisparoLaser 2 (5,9) C, DisparoLaser 3 (7,4) B]),
              (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
              [Jogador (5,3) C 5 5 5] [DisparoCanhao 0 (4,3) C]),
              (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
                       [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
              [Jogador (1,3) B 5 5 5] [DisparoCanhao 0 (2,3) B])]

-- * Funções principais da Tarefa 4.

-- | Avança o 'Estado' do jogo um 'Tick' de tempo.
--
-- __NB:__ Apenas os 'Disparo's afetam o 'Estado' do jogo com o passar do tempo.
--
-- __NB:__ Deve chamar as funções 'tickChoques', 'tickCanhoes' e 'tickLasers' pela ordem definida.
tick :: Estado -- ^ O 'Estado' anterior.
     -> Estado -- ^ O 'Estado' após um 'Tick'.
tick = tickChoques . tickCanhoes . tickLasers

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos tiros de 'Laser' disparados.
tickLasers :: Estado -> Estado
tickLasers (Estado m lj ld) = Estado (alteraMapaLaser m ld) (encontraJogadorAfetado (encontraBloco ld m 0) ld lj) (eliminaDisparoLaser ld)

-- | Altera o mapa conforme o disparo Laser

alteraMapaLaser :: Mapa -> [Disparo] -> Mapa
alteraMapaLaser m [] = m
alteraMapaLaser m ((DisparoLaser n (x,y) D):t) | encontraPosicaoMatriz (x+1,y+1) m == Bloco Indestrutivel && encontraPosicaoMatriz (x,y+1) m == Bloco Indestrutivel = alteraMapaLaser m ((DisparoLaser n (x,y) C):t)
                                               | encontraPosicaoMatriz (x+1,y+1) m == Bloco Indestrutivel && encontraPosicaoMatriz (x,y+1) m == Bloco Destrutivel = alteraMapaLaser (atualizaPosicaoMatriz (x,y+1) Vazia m) ((DisparoLaser n (x,y) C):t)
                                               | encontraPosicaoMatriz (x+1,y+1) m == Bloco Indestrutivel && encontraPosicaoMatriz (x,y+1) m == Vazia = alteraMapaLaser m ((DisparoLaser n (x,y) C):t)
                                               | encontraPosicaoMatriz (x+1,y+1) m == Bloco Destrutivel && encontraPosicaoMatriz (x,y+1) m == Bloco Indestrutivel = alteraMapaLaser (atualizaPosicaoMatriz (x,y) Vazia m) ((DisparoLaser n (x,y) C):t)
                                               | encontraPosicaoMatriz (x+1,y+1) m == Vazia && encontraPosicaoMatriz (x,y+1) m == Bloco Indestrutivel = alteraMapaLaser m ((DisparoLaser n (x,y) C):t)
                                               | encontraPosicaoMatriz (x+1,y+1) m == Bloco Destrutivel && encontraPosicaoMatriz (x,y+1) m == Bloco Destrutivel = alteraMapaLaser (atualizaPosicaoMatriz (x,y) Vazia (atualizaPosicaoMatriz (x,y+1) Vazia m)) ((DisparoLaser n (x-1,y) C):t)
                                               | encontraPosicaoMatriz (x+1,y+1) m == Bloco Destrutivel && encontraPosicaoMatriz (x,y+1) m == Vazia = alteraMapaLaser (atualizaPosicaoMatriz (x,y) Vazia (atualizaPosicaoMatriz (x,y+1) Vazia m )) ((DisparoLaser n (x-1,y) C):t)
                                               | encontraPosicaoMatriz (x+1,y+1) m == Vazia && encontraPosicaoMatriz (x,y+1) m == Bloco Destrutivel = alteraMapaLaser (atualizaPosicaoMatriz (x,y) Vazia (atualizaPosicaoMatriz (x,y+1) Vazia m )) ((DisparoLaser n (x-1,y) C):t)
                                               | otherwise = alteraMapaLaser m t
alteraMapaLaser m ((DisparoLaser n (x,y) C):t) | encontraPosicaoMatriz (x,y) m == Bloco Indestrutivel && encontraPosicaoMatriz (x,y+1) m == Bloco Indestrutivel = alteraMapaLaser m ((DisparoLaser n (x,y) C):t)
                                               | encontraPosicaoMatriz (x,y) m == Bloco Indestrutivel && encontraPosicaoMatriz (x,y+1) m == Bloco Destrutivel = alteraMapaLaser (atualizaPosicaoMatriz (x,y+1) Vazia m) ((DisparoLaser n (x,y) C):t)
                                               | encontraPosicaoMatriz (x,y) m == Bloco Indestrutivel && encontraPosicaoMatriz (x,y+1) m == Vazia = alteraMapaLaser m ((DisparoLaser n (x,y) C):t)
                                               | encontraPosicaoMatriz (x,y) m == Bloco Destrutivel && encontraPosicaoMatriz (x,y+1) m == Bloco Indestrutivel = alteraMapaLaser (atualizaPosicaoMatriz (x,y) Vazia m) ((DisparoLaser n (x,y) C):t)
                                               | encontraPosicaoMatriz (x,y) m == Vazia && encontraPosicaoMatriz (x,y+1) m == Bloco Indestrutivel = alteraMapaLaser m ((DisparoLaser n (x,y) C):t)
                                               | encontraPosicaoMatriz (x,y) m == Bloco Destrutivel && encontraPosicaoMatriz (x,y+1) m == Bloco Destrutivel = alteraMapaLaser (atualizaPosicaoMatriz (x,y) Vazia (atualizaPosicaoMatriz (x,y+1) Vazia m)) ((DisparoLaser n (x-1,y) C):t)
                                               | encontraPosicaoMatriz (x,y) m == Bloco Destrutivel && encontraPosicaoMatriz (x,y+1) m == Vazia = alteraMapaLaser (atualizaPosicaoMatriz (x,y) Vazia (atualizaPosicaoMatriz (x,y+1) Vazia m )) ((DisparoLaser n (x-1,y) C):t)
                                               | encontraPosicaoMatriz (x,y) m == Vazia && encontraPosicaoMatriz (x,y+1) m == Bloco Destrutivel = alteraMapaLaser (atualizaPosicaoMatriz (x,y) Vazia (atualizaPosicaoMatriz (x,y+1) Vazia m )) ((DisparoLaser n (x-1,y) C):t)
                                               | otherwise = alteraMapaLaser m t
alteraMapaLaser m ((DisparoLaser n (x,y) B):t) | encontraPosicaoMatriz (x+1,y+1) m == Bloco Indestrutivel && encontraPosicaoMatriz (x+1,y) m == Bloco Indestrutivel = alteraMapaLaser m ((DisparoLaser n (x,y) C):t)
                                               | encontraPosicaoMatriz (x+1,y+1) m == Bloco Indestrutivel && encontraPosicaoMatriz (x+1,y) m == Bloco Destrutivel = alteraMapaLaser (atualizaPosicaoMatriz (x,y+1) Vazia m) ((DisparoLaser n (x,y) C):t)
                                               | encontraPosicaoMatriz (x+1,y+1) m == Bloco Indestrutivel && encontraPosicaoMatriz (x+1,y) m == Vazia = alteraMapaLaser m ((DisparoLaser n (x,y) C):t)
                                               | encontraPosicaoMatriz (x+1,y+1) m == Bloco Destrutivel && encontraPosicaoMatriz (x+1,y) m == Bloco Indestrutivel = alteraMapaLaser (atualizaPosicaoMatriz (x,y) Vazia m) ((DisparoLaser n (x,y) C):t)
                                               | encontraPosicaoMatriz (x+1,y+1) m == Vazia && encontraPosicaoMatriz (x+1,y) m == Bloco Indestrutivel = alteraMapaLaser m ((DisparoLaser n (x,y) C):t)
                                               | encontraPosicaoMatriz (x+1,y+1) m == Bloco Destrutivel && encontraPosicaoMatriz (x+1,y) m == Bloco Destrutivel = alteraMapaLaser (atualizaPosicaoMatriz (x,y) Vazia (atualizaPosicaoMatriz (x,y+1) Vazia m)) ((DisparoLaser n (x-1,y) C):t)
                                               | encontraPosicaoMatriz (x+1,y+1) m == Bloco Destrutivel && encontraPosicaoMatriz (x+1,y) m == Vazia = alteraMapaLaser (atualizaPosicaoMatriz (x,y) Vazia (atualizaPosicaoMatriz (x,y+1) Vazia m )) ((DisparoLaser n (x-1,y) C):t)
                                               | encontraPosicaoMatriz (x+1,y+1) m == Vazia && encontraPosicaoMatriz (x,y+1) m == Bloco Destrutivel = alteraMapaLaser (atualizaPosicaoMatriz (x,y) Vazia (atualizaPosicaoMatriz (x,y+1) Vazia m )) ((DisparoLaser n (x-1,y) C):t)
                                               | otherwise = alteraMapaLaser m t
alteraMapaLaser m ((DisparoLaser n (x,y) E):t) | encontraPosicaoMatriz (x+1,y) m == Bloco Indestrutivel && encontraPosicaoMatriz (x,y) m == Bloco Indestrutivel = alteraMapaLaser m ((DisparoLaser n (x,y) C):t)
                                               | encontraPosicaoMatriz (x+1,y) m == Bloco Indestrutivel && encontraPosicaoMatriz (x,y) m == Bloco Destrutivel = alteraMapaLaser (atualizaPosicaoMatriz (x,y+1) Vazia m) ((DisparoLaser n (x,y) C):t)
                                               | encontraPosicaoMatriz (x+1,y) m == Bloco Indestrutivel && encontraPosicaoMatriz (x,y) m == Vazia = alteraMapaLaser m ((DisparoLaser n (x,y) C):t)
                                               | encontraPosicaoMatriz (x+1,y) m == Bloco Destrutivel && encontraPosicaoMatriz (x,y) m == Bloco Indestrutivel = alteraMapaLaser (atualizaPosicaoMatriz (x,y) Vazia m) ((DisparoLaser n (x,y) C):t)
                                               | encontraPosicaoMatriz (x+1,y) m == Vazia && encontraPosicaoMatriz (x,y) m == Bloco Indestrutivel = alteraMapaLaser m ((DisparoLaser n (x,y) C):t)
                                               | encontraPosicaoMatriz (x+1,y) m == Bloco Destrutivel && encontraPosicaoMatriz (x,y) m == Bloco Destrutivel = alteraMapaLaser (atualizaPosicaoMatriz (x,y) Vazia (atualizaPosicaoMatriz (x,y+1) Vazia m)) ((DisparoLaser n (x-1,y) C):t)
                                               | encontraPosicaoMatriz (x+1,y) m == Bloco Destrutivel && encontraPosicaoMatriz (x,y) m == Vazia = alteraMapaLaser (atualizaPosicaoMatriz (x,y) Vazia (atualizaPosicaoMatriz (x,y+1) Vazia m )) ((DisparoLaser n (x-1,y) C):t)
                                               | encontraPosicaoMatriz (x+1,y) m == Vazia && encontraPosicaoMatriz (x,y) m == Bloco Destrutivel = alteraMapaLaser (atualizaPosicaoMatriz (x,y) Vazia (atualizaPosicaoMatriz (x,y+1) Vazia m )) ((DisparoLaser n (x-1,y) C):t)
                                               | otherwise = alteraMapaLaser m t
alteraMapaLaser m (_:t) = alteraMapaLaser m t
-- | Elimina disparo Laser

eliminaDisparoLaser :: [Disparo] -> [Disparo]
eliminaDisparoLaser [] = []
eliminaDisparoLaser ((DisparoLaser n p d):t) = eliminaDisparoLaser t
eliminaDisparoLaser (h:t) = h : eliminaDisparoLaser t


-- | Encontra blocos indestrutíveis

encontraBloco :: [Disparo] -> Mapa  -> Int -> [(Int,Posicao)]
encontraBloco [] m a = []
encontraBloco ((DisparoLaser n (x,y) D):t) m a | (encontraPosicaoMatriz (x-1,y) m) == Bloco Indestrutivel && (encontraPosicaoMatriz (x,y) m) == Bloco Indestrutivel = (a,(x,y)) : encontraBloco t m (a+1)
                                               | otherwise = encontraBloco ((DisparoLaser n (x,y+1) D):t) m a
encontraBloco ((DisparoLaser n (x,y) C):t) m a | (encontraPosicaoMatriz (x-1,y-1) m) == Bloco Indestrutivel && (encontraPosicaoMatriz (x-1,y) m) == Bloco Indestrutivel = (a,(x,y)) : encontraBloco t m (a+1)
                                               | otherwise = encontraBloco ((DisparoLaser n (x-1,y) C):t) m a
encontraBloco ((DisparoLaser n (x,y) B):t) m a | (encontraPosicaoMatriz (x,y-1) m) == Bloco Indestrutivel && (encontraPosicaoMatriz (x,y) m) == Bloco Indestrutivel = (a,(x,y)) : encontraBloco t m (a+1)
                                               | otherwise = encontraBloco ((DisparoLaser n (x+1,y) B):t) m a
encontraBloco ((DisparoLaser n (x,y) E):t) m a | (encontraPosicaoMatriz (x-1,y-1) m) == Bloco Indestrutivel && (encontraPosicaoMatriz (x,y-1) m) == Bloco Indestrutivel = (a,(x,y)) : encontraBloco t m (a+1)
                                               | otherwise = encontraBloco ((DisparoLaser n (x,y-1) E):t) m a
encontraBloco (_:t) m a = encontraBloco t m (a+1)

-- | Verifica se existe jogador à frente do disparo

encontraJogadorAfetado :: [(Int,Posicao)] -> [Disparo]-> [Jogador] -> [Jogador]
encontraJogadorAfetado [] _ l = l
encontraJogadorAfetado (h:t) ld lj = encontraJogadorAfetado t ld (auxJogador h ld lj)


-- | Altera o jogador se ele for afetado pelo disparo

auxJogador :: (Int,Posicao) -> [Disparo] -> [Jogador] -> [Jogador]
auxJogador par ld [] = []
auxJogador (a,(x,y)) ld ((Jogador (x1,y1) d v l c):t1) | (direcaoDisparo (encontraIndiceLista a ld) == D) && x == x1 && y2 < y1 && y1 < y = (Jogador (x1,y1) d (v-1) l c) : auxJogador (a,(x,y)) ld t1
                                                       | (direcaoDisparo (encontraIndiceLista a ld) == C) && x < x1 && x1 < x2 && y == y1 = (Jogador (x1,y1) d (v-1) l c) : auxJogador (a,(x,y)) ld t1
                                                       | (direcaoDisparo (encontraIndiceLista a ld) == B) && x1 < x && x2 < x1 && y == y1 = (Jogador (x1,y1) d (v-1) l c) : auxJogador (a,(x,y)) ld t1
                                                       | (direcaoDisparo (encontraIndiceLista a ld) == E) && x == x1 && y < y1 && y1 < y2 = (Jogador (x1,y1) d (v-1) l c) : auxJogador (a,(x,y)) ld t1
                                                       | otherwise = (Jogador (x1,y1) d v l c) : auxJogador (a,(x,y)) ld t1

                                                       where (x2,y2) = posicaoDisparo (encontraIndiceLista a ld)



-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos das balas de 'Canhao' disparadas.

tickCanhoes :: Estado -> Estado
tickCanhoes (Estado m lj ld) = Estado (alteraMapaCanhao m ld) (alteraJogadorCanhao ld lj) (disparoCanhao (disparoNoMesmoSitio ld ) m)


-- | Verifica se há dois disparos no mesmo sitio

disparoNoMesmoSitio :: [Disparo] -> [Disparo]
disparoNoMesmoSitio [] = []
disparoNoMesmoSitio (h:t) = disparoNoMesmoSitio (disparoJogador h t)

-- | Auxiliar à função que verifica se há dois disparos no mesmo sitio

disparoJogador :: Disparo -> [Disparo] -> [Disparo]
disparoJogador d [] = []
disparoJogador (DisparoCanhao n (x,y) d) ((DisparoCanhao n1 (x1,y1) d1):t) | x == x1 && y == y1 = t
                                                                           | otherwise = (DisparoCanhao n1 (x1,y1) d1) : disparoJogador (DisparoCanhao n (x,y) d) t
disparoJogador (DisparoCanhao n (x,y) d) ((DisparoLaser n1 (x1,y1) d1):t) | x == x1 && y == y1 = ((DisparoLaser n1 (x1,y1) d1):t)
                                                                          | otherwise = (DisparoLaser n1 (x1,y1) d1) : disparoJogador (DisparoCanhao n (x,y) d) t
disparoJogador (DisparoLaser n (x,y) d) ((DisparoCanhao n1 (x1,y1) d1):t) | x == x1 && y == y1 = t
                                                                          | otherwise = (DisparoCanhao n1 (x1,y1) d1) : disparoJogador (DisparoLaser n (x,y) d) t                                                                          



-- | Altera Jogador afetado pelo disparo Canhao

alteraJogadorCanhao :: [Disparo] -> [Jogador] -> [Jogador]
alteraJogadorCanhao [] l = l
alteraJogadorCanhao _ [] = []
alteraJogadorCanhao (h:t) lj = alteraJogadorCanhao t (auxJogadorCanhao h lj)

-- | Verifica se um disparo afeta o Jogador

auxJogadorCanhao :: Disparo -> [Jogador] -> [Jogador]
auxJogadorCanhao d [] = []
auxJogadorCanhao (DisparoCanhao n (x,y) d) ((Jogador (x1,y1) dj v l c):t1) | v > 0 && d == D && x == x1 && (y+1) == y1 = (Jogador (x1,y1) dj (v-1) l c) : t1
                                                                           | v > 0 && d == C && (x-1) == x1 && y == y1 = (Jogador (x1,y1) dj (v-1) l c) : t1
                                                                           | v > 0 && d == B && (x+1) == x1 && y == y1 = (Jogador (x1,y1) dj (v-1) l c) : t1
                                                                           | v > 0 && d == E && x == x1 && (y-1) == y1 = (Jogador (x1,y1) dj (v-1) l c) : t1
                                                                           | otherwise = (Jogador (x1,y1) dj v l c) : auxJogadorCanhao (DisparoCanhao n (x,y) d) t1
auxJogadorCanhao d (h:t) = (h:t)

-- | Verifica os disparos de canhão

disparoCanhao :: [Disparo] -> Mapa -> [Disparo]
disparoCanhao [] m = []
disparoCanhao ((DisparoCanhao n (x,y) D):t) m | (encontraPosicaoMatriz (x-1,y) m == Vazia && encontraPosicaoMatriz (x,y) m == Vazia) = (DisparoCanhao n (x,y) D) : disparoCanhao t m
                                              | otherwise = disparoCanhao t m
disparoCanhao ((DisparoCanhao n (x,y) C):t) m | (encontraPosicaoMatriz (x-1,y-1) m == Vazia && encontraPosicaoMatriz (x-1,y) m == Vazia) = (DisparoCanhao n (x,y) C) : disparoCanhao t m
                                              | otherwise = disparoCanhao t m
disparoCanhao ((DisparoCanhao n (x,y) B):t) m | (encontraPosicaoMatriz (x,y-1) m == Vazia && encontraPosicaoMatriz (x,y) m == Vazia) = (DisparoCanhao n (x,y) B) : disparoCanhao t m
                                              | otherwise = disparoCanhao t m
disparoCanhao ((DisparoCanhao n (x,y) E):t) m | (encontraPosicaoMatriz (x-1,y-1) m == Vazia && encontraPosicaoMatriz (x,y-1) m == Vazia) = (DisparoCanhao n (x,y) E) : disparoCanhao t m
                                              | otherwise = disparoCanhao t m
disparoCanhao (h:t) m = h : disparoCanhao t m

-- | Altera o Mapa coforme a direção do disparo Canhao

alteraMapaCanhao :: Mapa -> [Disparo] -> Mapa
alteraMapaCanhao [] _ = []
alteraMapaCanhao m [] = m

alteraMapaCanhao m ((DisparoCanhao n (x,y) D):t) | encontraPosicaoMatriz (x+1,y+1) m == Bloco Destrutivel = alteraMapaCanhao (atualizaPosicaoMatriz (x+1,y+1) Vazia m) ((DisparoCanhao n (x,y) D):t)
                                                 | encontraPosicaoMatriz (x,y+1) m == Bloco Destrutivel = alteraMapaCanhao (atualizaPosicaoMatriz (x,y+1) Vazia m) ((DisparoCanhao n (x,y) D):t)
                                                 | otherwise = alteraMapaCanhao m t
alteraMapaCanhao m ((DisparoCanhao n (x,y) C):t) | encontraPosicaoMatriz (x,y) m == Bloco Destrutivel = alteraMapaCanhao (atualizaPosicaoMatriz (x,y) Vazia m) ((DisparoCanhao n (x,y) C):t)
                                                 | encontraPosicaoMatriz (x,y+1) m == Bloco Destrutivel = alteraMapaCanhao (atualizaPosicaoMatriz (x,y+1) Vazia m) ((DisparoCanhao n (x,y) C):t)
                                                 | otherwise = alteraMapaCanhao m t
alteraMapaCanhao m ((DisparoCanhao n (x,y) B):t) | encontraPosicaoMatriz (x+1,y+1) m == Bloco Destrutivel = alteraMapaCanhao (atualizaPosicaoMatriz (x+1,y+1) Vazia m) ((DisparoCanhao n (x,y) B):t)
                                                 | encontraPosicaoMatriz (x+1,y) m == Bloco Destrutivel = alteraMapaCanhao (atualizaPosicaoMatriz (x+1,y) Vazia m) ((DisparoCanhao n (x,y) B):t)
                                                 | otherwise = alteraMapaCanhao m t
alteraMapaCanhao m ((DisparoCanhao n (x,y) E):t) | encontraPosicaoMatriz (x,y) m == Bloco Destrutivel = alteraMapaCanhao (atualizaPosicaoMatriz (x,y) Vazia m) ((DisparoCanhao n (x,y) E):t)
                                                 | encontraPosicaoMatriz (x+1,y) m == Bloco Destrutivel = alteraMapaCanhao (atualizaPosicaoMatriz (x+1,y) Vazia m) ((DisparoCanhao n (x,y) E):t)
                                                 | otherwise = alteraMapaCanhao m t
alteraMapaCanhao m (_:t) = alteraMapaCanhao m t

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos campos de 'Choque' disparados.
tickChoques :: Estado -> Estado
tickChoques (Estado m lj ld) = Estado m lj (tickListaChoques ld)


-- | Retira um tick à lista de Choques
tickListaChoques :: [Disparo] -> [Disparo]
tickListaChoques [] = []
tickListaChoques ((DisparoChoque n t):s) | t >= 1 = tickListaChoques (DisparoChoque n (t-1):s)
                                         | otherwise = tickListaChoques s
tickListaChoques (h:s) = h : tickListaChoques s
