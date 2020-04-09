 -- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2018li1g086 where

import LI11819
import Data.List

-- * Funções não-recursivas.

-- | Um 'Vetor' é uma 'Posicao' em relação à origem.
type Vetor = Posicao
-- ^ <<http://oi64.tinypic.com/mhvk2x.jpg vetor>>

-- ** Funções sobre vetores

-- *** Funções gerais sobre 'Vetor'es.

-- | Soma dois 'Vetor'es.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores (x,y) (x1,y1) = (x+x1,y+y1)

-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (x1,y1) (x2,y2) = (x1-x2,y1-y2)

-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Int -> Vetor -> Vetor
multiplicaVetor a (x,y) = (a*x,a*y)

-- | Roda um 'Vetor' 90º no sentido dos ponteiros do relógio, alterando a sua direção sem alterar o seu comprimento (distância à origem).
--
-- <<http://oi65.tinypic.com/2j5o268.jpg rodaVetor>>
rodaVetor :: Vetor -> Vetor
rodaVetor (x,y) = (y,-x)

-- | Espelha um 'Vetor' na horizontal (sendo o espelho o eixo vertical).
--
-- <<http://oi63.tinypic.com/jhfx94.jpg inverteVetorH>>
inverteVetorH :: Vetor -> Vetor
inverteVetorH (x,y) = (x,-y)

-- | Espelha um 'Vetor' na vertical (sendo o espelho o eixo horizontal).
--
-- <<http://oi68.tinypic.com/2n7fqxy.jpg inverteVetorV>>
inverteVetorV :: Vetor -> Vetor
inverteVetorV (x,y) = (-x,y)

-- *** Funções do trabalho sobre 'Vetor'es.

-- | Devolve um 'Vetor' unitário (de comprimento 1) com a 'Direcao' dada.
direcaoParaVetor :: Direcao -> Vetor
direcaoParaVetor C = (-1,0)
direcaoParaVetor D = (0,1)
direcaoParaVetor B = (1,0)
direcaoParaVetor E = (0,-1)

-- ** Funções sobre listas

-- *** Funções gerais sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence à lista.
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido x l = x >= 0 && x < length l

-- ** Funções sobre matrizes.

-- *** Funções gerais sobre matrizes.

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensão de uma matriz.
--
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
dimensaoMatriz :: Matriz a -> Dimensao
dimensaoMatriz [] = (0,0)
dimensaoMatriz ([]:m) = (0,0)
dimensaoMatriz mtx = (m,n)
    where
        m = length mtx
        n = length $ head mtx


-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool
ePosicaoMatrizValida (l,c) x | l >= 0 && l < length x && (c >= 0 && c < length (head x)) = True
                             | otherwise = False

-- | Verifica se a posição está numa borda da matriz.
eBordaMatriz :: Posicao -> Matriz a -> Bool
eBordaMatriz (l,c) mtx = l == 0 || c ==0 || l == m -1 || c == n - 1
    where
        m = length mtx
        n = length (head mtx)



-- *** Funções do trabalho sobre matrizes.

-- | Converte um 'Tetromino' (orientado para cima) numa 'Matriz' de 'Bool'.
--
-- <<http://oi68.tinypic.com/m8elc9.jpg tetrominos>>
tetrominoParaMatriz :: Tetromino -> Matriz Bool
tetrominoParaMatriz T = [[False,False,False],
                         [True,True,True],
                         [False,True,False]]
tetrominoParaMatriz I = [[False,True,False,False],
                         [False,True,False,False],
                         [False,True,False,False],
                         [False,True,False,False]]
tetrominoParaMatriz J = [[False,True,False],
                         [False,True,False],
                         [True,True,False]]
tetrominoParaMatriz L = [[False,True,False],
                         [False,True,False],
                         [False,True,True]]
tetrominoParaMatriz O = [[True,True],
                         [True,True]]
tetrominoParaMatriz S = [[False,True,True],
                         [True,True,False],
                         [False,False,False]]
tetrominoParaMatriz Z = [[True,True,False],
                         [False,True,True],
                         [False,False,False]]

-- * Funções recursivas.

-- ** Funções sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Devolve o elemento num dado índice de uma lista.

encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista 0 (h:t) = h
encontraIndiceLista i (h:t) = encontraIndiceLista (i-1) t

-- | Modifica um elemento num dado índice.
-- ??
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista 0 x (h:t) = x : t
atualizaIndiceLista _ _ [] = []
atualizaIndiceLista i x (h:t) = h : atualizaIndiceLista (i-1) x t

-- ** Funções sobre matrizes.

-- | Roda uma 'Matriz' 90º no sentido dos ponteiros do relógio.
--
-- <<http://oi68.tinypic.com/21deluw.jpg rodaMatriz>>
rodaMatriz :: Matriz a -> Matriz a
rodaMatriz [] = []
rodaMatriz ([]:t) = []
rodaMatriz l = reverse (map head l) : rodaMatriz (map tail l)


-- | Inverte uma 'Matriz' na horizontal.
--
-- <<http://oi64.tinypic.com/iwhm5u.jpg inverteMatrizH>>
inverteMatrizH :: Matriz a -> Matriz a
inverteMatrizH [] = []
inverteMatrizH (h:t) = reverse h : inverteMatrizH t


-- | Inverte uma 'Matriz' na vertical.
-- <<http://oi64.tinypic.com/11l563p.jpg inverteMatrizV>>
inverteMatrizV :: Matriz a -> Matriz a
inverteMatrizV m = aux0 [] m
    where
       aux0 m [] = m
       aux0 m (h:t) = aux0 (h:m) t

-- | Cria uma nova 'Matriz' com o mesmo elemento.
criaMatriz :: Dimensao -> a -> Matriz a
criaMatriz (0,_) _ = []
criaMatriz (x,y) a = criaLista y a : criaMatriz (x-1, y) a

-- | Cria uma nova Lista.
criaLista :: Int -> a -> [a]
criaLista 0 _ = []
criaLista x a = a : criaLista (x-1) a

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: Posicao -> Matriz a -> a
encontraPosicaoMatriz (0,y) (h:t) = encontraIndiceLista y h
encontraPosicaoMatriz (x,y) (h:t) = encontraPosicaoMatriz (x-1,y) t

-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (0,y) a (h:t) = atualizaIndiceLista y a h : t
atualizaPosicaoMatriz (_,_) _ [] = []
atualizaPosicaoMatriz (x,y) a (h:t) = h : atualizaPosicaoMatriz (x-1,y) a t
