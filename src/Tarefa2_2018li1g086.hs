-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2018li1g086 where

import LI11819
import Tarefa1_2018li1g086
import Tarefa0_2018li1g086

-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(0 , Movimenta C , (Estado (mapaInicial (20,20)) listaJogadores [DisparoLaser 3 (8,1) E])), 
            (2 , Movimenta D , (Estado (mapaInicial (20,20)) listaJogadores [DisparoCanhao 2 (3,7) D])),
            (0 , Movimenta B , (Estado (mapaInicial (20,20)) [Jogador (3,7) D 5 1 1] [])),
            (0 , Movimenta E , (Estado (mapaInicial (20,20)) [Jogador (6,6) C 3 2 4] [])),
            (3 , Movimenta C , (Estado (mapaInicial (20,20)) listaJogadores [])),
            (2 , Dispara Canhao , (Estado (mapaInicial (20,20)) listaJogadores [DisparoCanhao 2 (3,8) D])),
            (1 , Dispara Laser , (Estado (mapaInicial (20,20)) listaJogadores [DisparoLaser 1 (5,7) B , DisparoCanhao 1 (5,6) B])),
            (0 , Dispara Canhao , (Estado (mapaInicial (20,20)) listaJogadores [DisparoCanhao 0 (6,6) D])),
            (1 , Dispara Canhao , (Estado (mapaInicial (20,20)) listaJogadores [DisparoCanhao 0 (6,6) D])),
            (3 , Dispara Choque , (Estado (mapaInicial (20,20)) listaJogadores [DisparoChoque 3 5 , DisparoCanhao 0 (5,6) C])),
            (0 , Movimenta D , (Estado (mapaInicial (20,20)) [Jogador (6,6) D 2 4 5 , Jogador (6,8) C 2 6 7] [])),
            (0 , Movimenta C , (Estado (mapaInicial (20,20)) listaJogadores [DisparoChoque 3 5 , DisparoCanhao 0 (5,6) C])),
            (0 , Movimenta D ,(Estado (mapaInicial(20,20)) [Jogador (6,8) D 2 3 4 , Jogador (7,10) C 2 3 4] []))] 



-- | auxiliar a testes
listaJogadores :: [Jogador]
listaJogadores = [(Jogador (6,6) C 3 2 4),(Jogador (5,6) B 0 4 3),(Jogador (3,7) D 5 0 1),(Jogador (8,1) E 2 2 8)]

-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior..
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.


jogada x (Movimenta C) (Estado m lj ld) | ld == [] && ((podeMexer (encontraJogador x lj 0) m C) == True) && ((elem True (jogadorCaminho (encontraJogador x lj 0) lj)) == False) = Estado m (adicionaJogador x (jogadaJogadorM (encontraJogador x lj 0) C) 0 lj) ld 
                                        | ((choqueNosDisparos ld) == False ) && ((podeMexer (encontraJogador x lj 0) m C) == True) && ((elem True (jogadorCaminho (encontraJogador x lj 0) lj)) == False) = Estado m (adicionaJogador x (jogadaJogadorM (encontraJogador x lj 0) C) 0 lj) ld 
                                        | (((podeMexer (encontraJogador x lj 0) m C) == True) && ((elem True (jogadorCaminho (encontraJogador x lj 0) lj)) == False) && ((mapaComChoque (posicaoJogador (encontraJogador x lj 0)) (jogadorChoque ld lj) ld) == False)) = Estado m (adicionaJogador x (jogadaJogadorM (encontraJogador x lj 0) C) 0 lj) ld 
                                        | otherwise = Estado m lj ld
jogada x (Movimenta D) (Estado m lj ld) | ld == [] && ((podeMexer (encontraJogador x lj 0) m D) == True) && ((elem True (jogadorCaminho (encontraJogador x lj 0) lj)) == False) = Estado m (adicionaJogador x (jogadaJogadorM (encontraJogador x lj 0) D) 0 lj) ld 
                                        | ((choqueNosDisparos ld) == False ) && ((podeMexer (encontraJogador x lj 0) m D) == True) && ((elem True (jogadorCaminho (encontraJogador x lj 0) lj)) == False) = Estado m (adicionaJogador x (jogadaJogadorM (encontraJogador x lj 0) D) 0 lj) ld
                                        | (((podeMexer (encontraJogador x lj 0) m D) == True) && ((elem True (jogadorCaminho (encontraJogador x lj 0) lj)) == False) && ((mapaComChoque (posicaoJogador (encontraJogador x lj 0)) (jogadorChoque ld lj) ld) == False)) = Estado m (adicionaJogador x (jogadaJogadorM (encontraJogador x lj 0) D) 0 lj) ld 
                                        | otherwise = Estado m lj ld
jogada x (Movimenta E) (Estado m lj ld) | ld == [] && ((podeMexer (encontraJogador x lj 0) m E) == True) && ((elem True (jogadorCaminho (encontraJogador x lj 0) lj)) == False) = Estado m (adicionaJogador x (jogadaJogadorM (encontraJogador x lj 0) E) 0 lj) ld 
                                        | ((choqueNosDisparos ld) == False ) && ((podeMexer (encontraJogador x lj 0) m E) == True) && ((elem True (jogadorCaminho (encontraJogador x lj 0) lj)) == False) = Estado m (adicionaJogador x (jogadaJogadorM (encontraJogador x lj 0) E) 0 lj) ld
                                        | (((podeMexer (encontraJogador x lj 0) m E) == True) && ((elem True (jogadorCaminho (encontraJogador x lj 0) lj)) == False) && ((mapaComChoque (posicaoJogador (encontraJogador x lj 0)) (jogadorChoque ld lj) ld) == False)) = Estado m (adicionaJogador x (jogadaJogadorM (encontraJogador x lj 0) E) 0 lj) ld 
                                        | otherwise = Estado m lj ld
jogada x (Movimenta B) (Estado m lj ld) | ld == [] && ((podeMexer (encontraJogador x lj 0) m B) == True) && ((elem True (jogadorCaminho (encontraJogador x lj 0) lj)) == False) = Estado m (adicionaJogador x (jogadaJogadorM (encontraJogador x lj 0) B) 0 lj) ld 
                                        | ((choqueNosDisparos ld) == False ) && ((podeMexer (encontraJogador x lj 0) m B) == True) && ((elem True (jogadorCaminho (encontraJogador x lj 0) lj)) == False) = Estado m (adicionaJogador x (jogadaJogadorM (encontraJogador x lj 0) B) 0 lj) ld 
                                        | (((podeMexer (encontraJogador x lj 0) m B) == True) && ((elem True (jogadorCaminho (encontraJogador x lj 0) lj)) == False) && ((mapaComChoque (posicaoJogador (encontraJogador x lj 0)) (jogadorChoque ld lj) ld) == False)) = Estado m (adicionaJogador x (jogadaJogadorM (encontraJogador x lj 0) B) 0 lj) ld
                                        | otherwise = Estado m lj ld

jogada x (Dispara Canhao) (Estado m lj ld) | (estavivo (encontraJogador x lj 0) == True) = Estado m lj (ld ++ [(jogadaJogadorD x (encontraJogador x lj 0) Canhao)])
                                           | otherwise = Estado m lj ld
jogada x (Dispara Laser) (Estado m lj ld) | (estavivo (encontraJogador x lj 0) == True) && (podeDispararLaser (encontraJogador x lj 0)) == True = Estado m (adicionaJogador x (municoes (encontraJogador x lj 0) Laser) 0 lj) (ld ++ [(jogadaJogadorD x (encontraJogador x lj 0) Laser)])
                                          | otherwise = Estado m lj ld
jogada x (Dispara Choque) (Estado m lj ld) | (estavivo (encontraJogador x lj 0) == True) && (podeDispararChoque (encontraJogador x lj 0)) == True = Estado m (adicionaJogador x (municoes (encontraJogador x lj 0) Choque) 0 lj) (ld ++ [(jogadaJogadorD x (encontraJogador x lj 0) Choque)])
                                           | otherwise = Estado m lj ld

-- | Encontra o jogador que efetua a jogada
encontraJogador :: Int -> [Jogador] -> Int -> Jogador
encontraJogador n (h:t) a | a == n = h
                          | otherwise = encontraJogador n t (a+1)

-- | Encontra posição do Jogador
encontraPosicaoJogador :: Jogador -> Posicao
encontraPosicaoJogador (Jogador (x,y) d v l c) = (x,y)

-- | Verifica se o jogador tem vidas             
estavivo :: Jogador -> Bool
estavivo (Jogador p d v l c) = v > 0 

-- | Verifica se o Jogador se pode mexer
podeMexer :: Jogador -> Mapa -> Direcao -> Bool
podeMexer (Jogador (x,y) d v l c) ma C | (((encontraPosicaoMatriz (x-1,y) ma) == Vazia) && ((encontraPosicaoMatriz (x-1,y+1) ma) == Vazia) && ((estavivo (Jogador (x,y) d v l c)) == True))  = True
                                       | otherwise = False
podeMexer (Jogador (x,y) d v l c) ma D | (((encontraPosicaoMatriz (x,y+1) ma) == Vazia) && ((encontraPosicaoMatriz (x+1,y+1) ma) == Vazia) && ((estavivo (Jogador (x,y) d v l c)) == True)) = True
                                       | otherwise = False
podeMexer (Jogador (x,y) d v l c) ma E | (((encontraPosicaoMatriz (x,y-1) ma) == Vazia) && ((encontraPosicaoMatriz (x+1,y-1) ma) == Vazia) && ((estavivo (Jogador (x,y) d v l c)) == True)) = True
                                       | otherwise = False
podeMexer (Jogador (x,y) d v l c) ma B | (((encontraPosicaoMatriz (x+2,y) ma) == Vazia) && ((encontraPosicaoMatriz (x+2,y+1) ma) == Vazia) && ((estavivo (Jogador (x,y) d v l c)) == True)) = True
                                       | otherwise = False


-- | Verifica se há jogador no caminho
jogadorCaminho :: Jogador -> [Jogador] -> [Bool]
jogadorCaminho j [] = [False]
jogadorCaminho j (h:t) = (auxCaminho j h) : (jogadorCaminho j t)

-- | Verifica se o jogador que queremos movimentar está ao lado de outro jogador
auxCaminho :: Jogador -> Jogador -> Bool
auxCaminho (Jogador (x,y) d v l c) (Jogador (x1,y1) di vi la ch) | d == C && x == x1 + 2 && (y == y1 || y == y1 - 1 || y == y1 + 1) = True
                                                                 | d == D && y == y1 - 2 && (x == x1 || x == x1 - 1 || x == x1 + 1) = True
                                                                 | d == B && x == x1 - 2 && (y == y1 || y == y1 - 1 || y == y1 + 1) = True
                                                                 | d == E && y == y1 + 2 && (x == x1 || x == x1 - 1 || x == x1 + 1) = True 
                                                                 | otherwise = False 

-- | Verifica se houve ou há algum disparo choque
choqueNosDisparos :: [Disparo] -> Bool 
choqueNosDisparos [] = False
choqueNosDisparos ((DisparoChoque n t) : s) = True
choqueNosDisparos (h:s) = choqueNosDisparos s

-- | Indica a posição do Jogador que fez o disparo do choque
jogadorChoque :: [Disparo] -> [Jogador] -> Posicao 
jogadorChoque ((DisparoChoque n t):s) l = posicaoJogador (encontraJogador n l 0)
jogadorChoque ((DisparoCanhao n p d):s) l = jogadorChoque s l
jogadorChoque ((DisparoLaser n p d):s) l = jogadorChoque s l


-- | Verifica se há algum choque ativo para se poder movimentar 
mapaComChoque :: Posicao -> Posicao -> [Disparo] -> Bool
mapaComChoque (x,y) (x1,y1) [] = False
mapaComChoque (x,y) (x1,y1) ld |  y <= (y1 + 3) && y >= (y1 - 3) && x <= (x1 + 3) && x >= (x1 - 3) = True
                               | otherwise = False



-- | Efetua a jogada de Movimenta no jogador 
jogadaJogadorM :: Jogador -> Direcao -> Jogador 
jogadaJogadorM (Jogador (x,y) d v l c) C | d == C = Jogador (x-1,y) d v l c
                                         | otherwise = Jogador (x,y) C v l c 
jogadaJogadorM (Jogador (x,y) d v l c) D | d == D = Jogador (x,y+1) d v l c 
                                         | otherwise = Jogador (x,y) D v l c                           
jogadaJogadorM (Jogador (x,y) d v l c) E | d == E = Jogador (x,y-1) d v l c
                                         | otherwise = Jogador (x,y) E v l c 
jogadaJogadorM (Jogador (x,y) d v l c) B | d == B = Jogador (x+1,y) d v l c
                                         | otherwise = Jogador (x,y) B v l c 

-- | Verifica se o Jogador pode disparar choque
podeDispararChoque :: Jogador -> Bool
podeDispararChoque (Jogador p d v l c) | c > 0 = True
                                       | otherwise = False

-- | Verifica se o Jogador pode disparar Laser                                       
podeDispararLaser :: Jogador -> Bool
podeDispararLaser (Jogador p d v l c) | l > 0 = True
                                      | otherwise = False



-- | Efetua as jogadas de dispara arma 
jogadaJogadorD :: Int -> Jogador -> Arma -> Disparo
jogadaJogadorD n (Jogador (x,y) d v l c) a | a == Canhao && d == C = DisparoCanhao n (x-1,y) d
                                           | a == Canhao && d == D = DisparoCanhao n (x,y+1) d
                                           | a == Canhao && d == E = DisparoCanhao n (x,y-1) d
                                           | a == Canhao && d == B = DisparoCanhao n (x+1,y) d
                                           | a == Laser && d == C = DisparoLaser n (x-1,y) d
                                           | a == Laser && d == D = DisparoLaser n (x,y+1) d
                                           | a == Laser && d == E = DisparoLaser n (x,y-1) d
                                           | a == Laser && d == B = DisparoLaser n (x+1,y) d
                                           | otherwise = DisparoChoque n 5 

-- | altera as munições do Jogador depois de disparar
municoes :: Jogador -> Arma -> Jogador
municoes (Jogador p d v l c) Choque = Jogador p d v l (c-1)
municoes (Jogador p d v l c) Laser = Jogador p d v (l-1) c  

-- | adiciona o jogador alterado na lista criada em eliminaJogador
adicionaJogador :: Int -> Jogador -> Int -> [Jogador] -> [Jogador]
adicionaJogador n j a (h:t) | a == n = j : t
                            | otherwise = h : adicionaJogador n j (a+1) t 