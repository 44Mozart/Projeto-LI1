{-|
Module       : Tarefa6_2018li1g086
Description  : Implementação do Jogo em Gloss
Copyright    : Maria Barros <a89525@alunos.uminho.pt>;
               Miguel Rocha <a89508@alunos.uminho.pt>

__INTRODUÇÃO__

* Neste módulo pretende-se desenvolver um __BOT__ que conseguisse jogar o jogo sem interação humana

__OBJETIVOS E ESTRATÉGIAS UTILIZADAS__

* Para que o __BOT__ consiga jogar sozinho e obter a melhor pontuação possível, sempre que que tem algum jogador na __mesma direção__ dispara o __Laser__, caso não tenha balas de Laser __dispara Canhão__, se não movimenta-se para o jogador __até conseguir disparar__. Caso alguma bala de canhão esteja a vir para ele, ele __roda para a Direção oposta__.

__CONCLUSÃO__

* Consideramos que o nosso ro__Bot__ consegue jogar o jogo apesar da sua estratégia não ser a melhor.

-}

module Tarefa6_2018li1g086 where

import LI11819
import Tarefa0_2018li1g086
import Tarefa1_2018li1g086
import Tarefa2_2018li1g086
import Tarefa3_2018li1g086
import Tarefa4_2018li1g086

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot n (Estado m lj ld) | dispararCanhao (encontraBot lj n) ld == True = Just (Movimenta (rodaDirecaoDisparo (encontraBot lj n) ld))
                       | roda m (encontraBot lj n) == True = Just (Movimenta (rodaAteVazio m (encontraBot lj n)))
                       | disparaCanhaoContraBot (encontraBot lj n) lj == True = Just (Dispara Canhao)
                       | disparaLaserContraBot (encontraBot lj n) lj == True  = Just (Dispara Laser)
                       | moverParaDirecao (encontraBot lj n) m == True = Just (Movimenta (direcaoJogador (encontraBot lj n)))
                       | roda m (encontraBot lj n) == True = Just (Movimenta (rodaAteVazio m (encontraBot lj n)))
                       | otherwise = Nothing

-- | encontra o Jogador que se vai movimentar

encontraBot :: [Jogador] -> Int -> Jogador
encontraBot (h:t) n | n == 0 = h
                    | otherwise = encontraBot t (n-1)

-- | Verifica se o Jogador se pode mexer
moverParaDirecao :: Jogador -> Mapa -> Bool
moverParaDirecao (Jogador (x,y) C v l c) ma | (((encontraPosicaoMatriz (x-1,y) ma) == Vazia) && ((encontraPosicaoMatriz (x-1,y+1) ma) == Vazia) && ((estavivo (Jogador (x,y) C v l c)) == True)) = True
                                            | otherwise = False
moverParaDirecao (Jogador (x,y) D v l c) ma | (((encontraPosicaoMatriz (x,y+1) ma) == Vazia) && ((encontraPosicaoMatriz (x+1,y+1) ma) == Vazia) && ((estavivo (Jogador (x,y) D v l c)) == True)) = True
                                            | otherwise = False
moverParaDirecao (Jogador (x,y) E v l c) ma | (((encontraPosicaoMatriz (x,y-1) ma) == Vazia) && ((encontraPosicaoMatriz (x+1,y-1) ma) == Vazia) && ((estavivo (Jogador (x,y) E v l c)) == True)) = True
                                            | otherwise = False
moverParaDirecao (Jogador (x,y) B v l c) ma | (((encontraPosicaoMatriz (x+2,y) ma) == Vazia) && ((encontraPosicaoMatriz (x+2,y+1) ma) == Vazia) && ((estavivo (Jogador (x,y) B v l c)) == True)) = True
                                            | otherwise = False

-- | Quando está algum bot na mesma linha ou coluna que esse bot, ele dispara

disparaLaserContraBot :: Jogador -> [Jogador] -> Bool
disparaLaserContraBot _ [] = False
disparaLaserContraBot (Jogador (x,y) d v l c) ((Jogador (x1,y1) d1 v1 l1 c1):t) | l > 0 && d == D && x == x1 && y < y1 = True
                                                                                | l > 0 && d == C && y == y1 && x1 < x = True
                                                                                | l > 0 && d == E && x == x1 && y1 < y = True
                                                                                | l > 0 && d == B && y == y1 && x < x1 = True
                                                                                | otherwise = disparaLaserContraBot (Jogador (x,y) d v l c) t

-- | Quando não tem laser, dispara canhão

disparaCanhaoContraBot :: Jogador -> [Jogador] -> Bool
disparaCanhaoContraBot _ [] = False
disparaCanhaoContraBot (Jogador (x,y) d v l c) ((Jogador (x1,y1) d1 v1 l1 c1):t) | d == D && x == x1 && y < y1 = True
                                                                                 | d == C && y == y1 && x1 < x = True
                                                                                 | d == E && x == x1 && y1 < y = True
                                                                                 | d == B && y == y1 && x < x1 = True
                                                                                 | otherwise = disparaCanhaoContraBot (Jogador (x,y) d v l c) t



-- | Foge do disparo

dispararCanhao :: Jogador -> [Disparo] -> Bool
dispararCanhao j [] = False
dispararCanhao (Jogador (x,y) d v l c) ((DisparoCanhao n (x1,y1) d1):t) | y == y1 && x1 < x || x < x1 = True
                                                                        | x == x1 && y1 < y || y < y1 = True
                                                                        | otherwise = dispararCanhao (Jogador (x,y) d v l c) t
dispararCanhao j (_:t) = dispararCanhao  j t

-- | Muda direção para atacar

rodaDirecaoDisparo :: Jogador -> [Disparo] -> Direcao
rodaDirecaoDisparo j [] = direcaoJogador j
rodaDirecaoDisparo (Jogador (x,y) d v l c) ((DisparoCanhao n (x1,y1) d1):t) | y == y1 && x1 < x && d1 == B = C
                                                                            | y == y1 && x < x1 && d1 == C = B
                                                                            | x == x1 && y1 < y && d1 == D = E
                                                                            | x == x1 && y < y1 && d1 == E = D
                                                                            | otherwise = rodaDirecaoDisparo (Jogador (x,y) d v l c) t
rodaDirecaoDisparo j (_:t) = rodaDirecaoDisparo j t


-- | Se tiver bloco Indestrutivel à frente
roda :: Mapa -> Jogador -> Bool
roda m (Jogador (x,y) d v l c) | d == C && encontraPosicaoMatriz (x-1,y) m == Bloco Indestrutivel && encontraPosicaoMatriz (x-1,y+1) m == Bloco Indestrutivel = True
                               | otherwise = False
                               | d == D && encontraPosicaoMatriz (x,y+2) m == Bloco Indestrutivel && encontraPosicaoMatriz (x+1,y+2) m == Bloco Indestrutivel = True
                               | otherwise = False
                               | d == E && encontraPosicaoMatriz (x,y-1) m == Bloco Indestrutivel && encontraPosicaoMatriz (x+1,y-1) m == Bloco Indestrutivel = True
                               | otherwise = False
                               | d == B && encontraPosicaoMatriz (x+2,y) m == Bloco Indestrutivel && encontraPosicaoMatriz (x+2,y+1) m == Bloco Indestrutivel = True
                               | otherwise = False

-- | Roda para direção do Vazio
rodaAteVazio :: Mapa -> Jogador -> Direcao
rodaAteVazio m (Jogador (x,y) D v l c) | encontraPosicaoMatriz (x,y+2) m == Vazia && encontraPosicaoMatriz (x+1,y+2) m == Vazia = D
                                       | otherwise = rodaAteVazio m (Jogador (x,y) B v l c)
rodaAteVazio m (Jogador (x,y) B v l c) | encontraPosicaoMatriz (x+2,y) m == Vazia && encontraPosicaoMatriz (x+2,y+1) m == Vazia = B
                                       | otherwise = rodaAteVazio m (Jogador (x,y) E v l c)
rodaAteVazio m (Jogador (x,y) E v l c) | encontraPosicaoMatriz (x,y-1) m == Vazia && encontraPosicaoMatriz (x+1,y-1) m == Vazia = E
                                       | otherwise = rodaAteVazio m (Jogador (x,y) C v l c)
rodaAteVazio m (Jogador (x,y) C v l c) | encontraPosicaoMatriz (x-1,y) m == Vazia && encontraPosicaoMatriz (x-1,y+1) m == Vazia = C
                                       | otherwise = rodaAteVazio m (Jogador (x,y) D v l c)
