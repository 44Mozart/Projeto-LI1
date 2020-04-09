-- | Este módulo define funções comuns da Tarefa 1 do trabalho prático.
module Tarefa1_2018li1g086 where

import LI11819
import Tarefa0_2018li1g086


-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é uma sequência de 'Instrucoes'.
testesT1 :: [Instrucoes]
testesT1 = [[Desenha,Move D,Move D,Move D,Roda,Desenha,Move D,Move D,Move D,Move D,Roda,Move E,Desenha,Move D,Move D,Move D,Roda,Move D,Desenha,Move D,Move E,Move E,Move B,Move B,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,MudaParede,Roda,Move B,Move E,Move B,Desenha,Move D,Move D,Roda,Move D,Move E,Move D,Desenha,Move D,Move D,Roda,Move D,Desenha,Move D,Move D,Roda,Move D,Move D,Move C,Move B,Desenha,Move B,Move B,Move B,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,MudaTetromino,MudaParede,Move B,Move E,Move D,Desenha,Move D,Move D,Roda,Move D,Move D,Move D,Move E,Desenha,Move D,Move D,Move D,MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,Roda,Move B,Desenha,Move D,Move D,Move D,Roda,Move C,Desenha,Move B,Move B,Move B,Move B,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,MudaParede,Roda,Move C,Move C,Desenha,Move D,Move D,Roda,Move B,Move D,Move D,Desenha,Move D,Move D,Move D,Roda,Desenha,Roda,Move D,Move D,Move D,Desenha,Move B,Move B,Move B,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,MudaTetromino,Move D,Move D,Desenha,Roda,Move D,Move D,Move D,Move D,Move E,Desenha,Move D,Roda,Move D,Move D,Desenha,Move D,Move D,Roda,Move D,Move D,Desenha,Move B,Move B,Move B,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,MudaParede,Roda,Desenha,Move D,Roda,Move D,Move D,Desenha,Move D,Roda,Move D,Move D,Desenha,Move D,Roda,Move D,Move D,Move D,Desenha,Move B,Move B,Move B,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move C,MudaTetromino,Move B,Move B,Move C,Move B,Desenha,Move D,Move D,Move D,Move D,Move D,Desenha,Move D,Move D,Move D,Desenha,Move D,Move D,Move D,Desenha,Roda,Desenha,Move E,Move E,Move E,Roda,Desenha,Move E,Move E,Move E,Move E,Move D,Roda,Desenha,Move B,Move B,Move B,Move E,Move E,Move E,Move E,Move E,MudaParede,Desenha,Move D,Move D,Move D,Move D,Roda,Desenha,Move D,Move D,Move D,Roda,Desenha,Move D,Move D,Move D,Roda,Desenha,Move B,Move B,Move B,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move D,Move D,Move D,Move D,Move D,Move D,MudaTetromino,MudaParede,Desenha,Move D,Move D,Move D,Roda,Desenha,Move D,Move D,Move D,Move D,Roda,Desenha,Move D,Move D,Move D,Roda,Move C,Move D,Desenha,Move B,Move B,Move B,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,MudaParede,Move D,Move D,Move D,Move D,Roda,Move B,Desenha,Move D,Move D,Move D,Roda,Desenha,Move D,Move D,Move D,Roda,Move D,Desenha,Roda,Move D,Move D,Move D,Move D,Move C,Desenha,Move B,Move B,Move B,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move D,Move B,MudaTetromino,Move B,MudaParede,Desenha,Move D,Move D,Roda,Move D,Move D,Move C,Desenha,Roda,Move D,Move D,Move D,Move D,Desenha,Roda,Move D,Move D,Move D,Move D,Desenha,Move B,Move B,Move B,Move B,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move D,Move D,Move D,Move D,Move D,Move D,MudaParede,Roda,Desenha,Move D,Move D,Move D,Move D,Roda,Move C,Desenha,Roda,Move D,Move D,Move D,Move D,Desenha,Move D,Move D,Roda,Move D,Move D,Desenha,Move B,Move B,Move B,MudaTetromino,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move D,Move D,Move D,Move D,Move D,Move D,Move E,MudaParede,Desenha,Roda,Move D,Move D,Move D,Move D,Move D,Desenha,Move D,Move D,Move D,Move D,Roda,Desenha,Move D,Move D,Move D,Move D,Roda,Desenha,Move B,Move B,Move B,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,MudaParede,Desenha,Move D,Move D,Move D,Move D,Roda,Desenha,Move D,Move D,Move D,Roda,Desenha,Move D,Move D,Move D,Move D,Roda,Desenha,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,MudaTetromino,Move C]]
-- * Funções principais da Tarefa 1.

-- | Aplica uma 'Instrucao' num 'Editor'.
--
--    * 'Move' - move numa dada 'Direcao'.
--
--    * 'MudaTetromino' - seleciona a 'Peca' seguinte (usar a ordem léxica na estrutura de dados),
--       sem alterar os outros parâmetros.
--
--    * 'MudaParede' - muda o tipo de 'Parede'.
--
--    * 'Desenha' - altera o 'Mapa' para incluir o 'Tetromino' atual, sem alterar os outros parâmetros.
instrucao :: Instrucao -- ^ A 'Instrucao' a aplicar.
          -> Editor    -- ^ O 'Editor' anterior.
          -> Editor    -- ^ O 'Editor' resultante após aplicar a 'Instrucao'.
instrucao (Move D) (Editor (x,y) d t pa m) = Editor (somaVetores (x,y) (direcaoParaVetor D)) d t pa m
instrucao (Move C) (Editor (x,y) d t pa m) = Editor (somaVetores (x,y) (direcaoParaVetor C)) d t pa m
instrucao (Move E) (Editor (x,y) d t pa m) = Editor (somaVetores (x,y) (direcaoParaVetor E)) d t pa m
instrucao (Move B) (Editor (x,y) d t pa m) = Editor (somaVetores (x,y) (direcaoParaVetor B)) d t pa m
instrucao MudaTetromino (Editor p d I pa m) = Editor p d J pa m
instrucao MudaTetromino (Editor p d J pa m) = Editor p d L pa m
instrucao MudaTetromino (Editor p d L pa m) = Editor p d O pa m
instrucao MudaTetromino (Editor p d O pa m) = Editor p d S pa m
instrucao MudaTetromino (Editor p d S pa m) = Editor p d T pa m
instrucao MudaTetromino (Editor p d T pa m) = Editor p d Z pa m
instrucao MudaTetromino (Editor p d Z pa m) = Editor p d I pa m
instrucao Roda (Editor p C t pa m) = Editor p D t pa m
instrucao Roda (Editor p D t pa m) = Editor p B t pa m
instrucao Roda (Editor p B t pa m) = Editor p E t pa m
instrucao Roda (Editor p E t pa m) = Editor p C t pa m
instrucao MudaParede (Editor p d t Indestrutivel m) = Editor p d t Destrutivel m
instrucao MudaParede (Editor p d t Destrutivel m) = Editor p d t Indestrutivel m

instrucao Desenha (Editor (x,y) C I pa m) = Editor (x,y) C I pa (auxDesenha [(x,y+1),(x+1,y+1),(x+2,y+1),(x+3,y+1)] pa m)
instrucao Desenha (Editor (x,y) D I pa m) = Editor (x,y) D I pa (auxDesenha [(x+1,y),(x+1,y+1),(x+1,y+2),(x+1,y+3)] pa m)
instrucao Desenha (Editor (x,y) B I pa m) = Editor (x,y) B I pa (auxDesenha [(x,y+2),(x+1,y+2),(x+2,y+2),(x+3,y+2)] pa m)
instrucao Desenha (Editor (x,y) E I pa m) = Editor (x,y) E I pa (auxDesenha [(x+2,y),(x+2,y+1),(x+2,y+2),(x+2,y+3)] pa m)
instrucao Desenha (Editor (x,y) C J pa m) = Editor (x,y) C J pa (auxDesenha [(x+2,y),(x,y+1),(x+1,y+1),(x+2,y+1)] pa m)
instrucao Desenha (Editor (x,y) D J pa m) = Editor (x,y) D J pa (auxDesenha [(x,y),(x+1,y),(x+1,y+1),(x+1,y+2)] pa m)
instrucao Desenha (Editor (x,y) B J pa m) = Editor (x,y) B J pa (auxDesenha [(x,y+1),(x,y+2),(x+1,y+1),(x+2,y+1)] pa m)
instrucao Desenha (Editor (x,y) E J pa m) = Editor (x,y) E J pa (auxDesenha [(x+2,y+2),(x+1,y),(x+1,y+1),(x+1,y+2)] pa m)
instrucao Desenha (Editor (x,y) C L pa m) = Editor (x,y) C L pa (auxDesenha [(x+2,y+1),(x+2,y+2),(x+1,y+1),(x,y+1)] pa m)
instrucao Desenha (Editor (x,y) D L pa m) = Editor (x,y) D L pa (auxDesenha [(x+1,y),(x+1,y+1),(x+1,y+2),(x+2,y)] pa m)
instrucao Desenha (Editor (x,y) B L pa m) = Editor (x,y) B L pa (auxDesenha [(x,y),(x,y+1),(x+1,y+1),(x+2,y+1)] pa m)
instrucao Desenha (Editor (x,y) E L pa m) = Editor (x,y) E L pa (auxDesenha [(x+1,y),(x+1,y+1),(x+1,y+2),(x,y+2)] pa m)
instrucao Desenha (Editor (x,y) C S pa m) = Editor (x,y) C S pa (auxDesenha [(x,y+1),(x,y+2),(x+1,y),(x+1,y+1)] pa m)
instrucao Desenha (Editor (x,y) D S pa m) = Editor (x,y) D S pa (auxDesenha [(x,y+1),(x+1,y+1),(x+1,y+2),(x+2,y+2)] pa m)
instrucao Desenha (Editor (x,y) B S pa m) = Editor (x,y) B S pa (auxDesenha [(x+1,y+1),(x+1,y+2),(x+2,y),(x+2,y+1)] pa m)
instrucao Desenha (Editor (x,y) E S pa m) = Editor (x,y) E S pa (auxDesenha [(x,y),(x+1,y),(x+1,y+1),(x+2,y+1)] pa m)
instrucao Desenha (Editor (x,y) C T pa m) = Editor (x,y) C T pa (auxDesenha [(x+1,y),(x+1,y+1),(x+1,y+2),(x+2,y+1)] pa m)
instrucao Desenha (Editor (x,y) D T pa m) = Editor (x,y) D T pa (auxDesenha [(x,y+1),(x+1,y+1),(x+2,y+1),(x+1,y)] pa m)
instrucao Desenha (Editor (x,y) B T pa m) = Editor (x,y) B T pa (auxDesenha [(x+1,y),(x+1,y+1),(x+1,y+2),(x,y+1)] pa m)
instrucao Desenha (Editor (x,y) E T pa m) = Editor (x,y) E T pa (auxDesenha [(x,y+1),(x+1,y+1),(x+2,y+1),(x+1,y+2)] pa m)
instrucao Desenha (Editor (x,y) C Z pa m) = Editor (x,y) C Z pa (auxDesenha [(x,y),(x,y+1),(x+1,y+1),(x+1,y+2)] pa m)
instrucao Desenha (Editor (x,y) D Z pa m) = Editor (x,y) D Z pa (auxDesenha [(x,y+2),(x+1,y+1),(x+1,y+2),(x+2,y+1)] pa m)
instrucao Desenha (Editor (x,y) B Z pa m) = Editor (x,y) B Z pa (auxDesenha [(x+1,y),(x+1,y+1),(x+2,y+1),(x+2,y+2)] pa m)
instrucao Desenha (Editor (x,y) E Z pa m) = Editor (x,y) E Z pa (auxDesenha [(x,y+1),(x+1,y),(x+1,y+1),(x+2,y)] pa m)
instrucao Desenha (Editor (x,y) d O pa m) = Editor (x,y) d O pa (auxDesenha [(x,y),(x+1,y),(x,y+1),(x+1,y+1)] pa m)

-- | Aplica a função atualizaPosicaoMatriz à lista das posições dos Tetrominos
auxDesenha :: [Posicao] -> Parede -> Mapa -> Mapa
auxDesenha [] a m = m
auxDesenha (h:t) pa m = auxDesenha t pa (atualizaPosicaoMatriz h (Bloco pa) m)
-- | Aplica uma sequência de 'Instrucoes' num 'Editor'.
--
-- __NB:__ Deve chamar a função 'instrucao'.
instrucoes :: Instrucoes -- ^ As 'Instrucoes' a aplicar.
           -> Editor     -- ^ O 'Editor' anterior.
           -> Editor     -- ^ O 'Editor' resultante após aplicar as 'Instrucoes'.
instrucoes [] e = e
instrucoes (h:t) e = instrucoes t (instrucao h e)

-- | Cria um 'Mapa' inicial com 'Parede's nas bordas e o resto vazio.
mapaInicial :: Dimensao -- ^ A 'Dimensao' do 'Mapa' a criar.
          -> Mapa     -- ^ O 'Mapa' resultante com a 'Dimensao' dada.
mapaInicial (x,y) = simple (Bloco Indestrutivel) y : replicate (x-2) (Bloco Indestrutivel : simple Vazia (y-2) ++ [Bloco Indestrutivel]) ++ [simple (Bloco Indestrutivel) y] 
    where
        simple p 0 = []
        simple p n = p : simple p (n-1)
        

-- | Cria um 'Editor' inicial.
--
-- __NB:__ Deve chamar as funções 'mapaInicial', 'dimensaoInicial', e 'posicaoInicial'.
editorInicial :: Instrucoes  -- ^ Uma sequência de 'Instrucoes' de forma a poder calcular a  'dimensaoInicial' e a 'posicaoInicial'.
              -> Editor      -- ^ O 'Editor' inicial, usando a 'Peca' 'I' 'Indestrutivel' voltada para 'C'.
editorInicial x = Editor (posicaoInicial x) C I Indestrutivel (mapaInicial (dimensaoInicial x))


-- | Constrói um 'Mapa' dada uma sequência de 'Instrucoes'.
--
-- __NB:__ Deve chamar as funções 'Instrucoes' e 'editorInicial'.
constroi :: Instrucoes -- ^ Uma sequência de 'Instrucoes' dadas a um 'Editor' para construir um 'Mapa'.
         -> Mapa       -- ^ O 'Mapa' resultante.
constroi is = mapaEditor (instrucoes is (editorInicial is))