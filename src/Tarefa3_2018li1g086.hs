{-|
Module       : Tarefa3_2018li1g086
Description  : Compressão e Descompressão de um Estado do Jogo
Copyright    : Maria Barros <a89525@alunos.uminho.pt>;
               Miguel Rocha <a89508@alunos.uminho.pt>

__INTRODUÇÃO__

* Neste módulo usámos duas funções principais (__'comprime'__ e __'descomprime'__) em que a primeira comprime o __Estado__ do jogo em uma __String__ e a segunda reverte o processo descomprimindo a __String__ e obtendo o __Estado__ exato que foi comprimido. 

__OBJETIVOS E ESTRATÉGIAS UTILIZADAS__

* __COMPRESSÃO:__ O objetivo, usando a função __'comprime'__, foi tentar comprimir o __Estado__ em uma __String__ que fosse o menor possível, mas que fosse possível depois aplicar a função __'descomprime'__ e conseguir descodificar a string. Para isso usamos três funções secundárias: __'comprimeMapa'__, __'comprimeListaJogador'__ e __'comprimeListaDisparos'__. Para a __Compressão do Mapa__, como o __Mapa__ é uma matriz, resolvemos atribuir um __carácter__ para cada tipo de __Peça__ e separar as linhas do Mapa com outro carácter o que fez com que o número total de carácteres da String __diminuísse bastante__. Para a __Compressão dos Jogadores__ e __Compressão dos Disparos__ usámos diferentes caracteres para representar todas as __componentes__ e usámos outros caracteres para pudermos assim descomprimir mais facilmente a __String__.

* __DESCOMPRESSÃO:__ Para a __descompressão__ usámos a função __'descomprime'__ que é formada por três funções secundárias: __'dcMapa'__, __'dcJogadorLista'__ e __'dcDisparaLista'__. A estratégia usada foi __separar__ a parte da String referente ao __Mapa__ e assim mais facilmente conseguir descomprimir o Mapa, depois __retirando__ essa parte da String, aplicar a __Descompressão da Lista de Jogadores__ até ao caso de paragem e seguidamente, após __retirar__ a parte da Lista de Jogadores da String, aplicar a __Descompressão dos Disparos__.

__CONCLUSÃO__

* Consideramos que esta tarefa foi bem executada, pois as funções __'comprime'__ e __'descomprime'__ conseguem efetuar a compressão e descompressão de qualquer __Estado__ do jogo e o __Estado__ inicialmente introduzido é exatamente igual ao __Estado__ que é produto da função __'descomprime'__. Obtemos uma taxe de compressão de __90.87%__ o que consideramos que podia ter sido mais elevada mas, em suma, cumprimos os objetivos estipulados.

-}
module Tarefa3_2018li1g086 where


import LI11819
import Tarefa0_2018li1g086
import Tarefa1_2018li1g086
import Tarefa2_2018li1g086
-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Estado'.
testesT3 :: [Estado]
testesT3 = [Estado (mapaInicial (20,20)) [Jogador (1,2) D 5 5 5,Jogador (10,10) C 5 5 5] [(DisparoCanhao 0 (1,2) D),(DisparoChoque 0 5),(DisparoLaser 1 (10,10) C)],
            Estado (mapaInicial (20,20)) [Jogador (1,2) D 5 5 5] [], Estado (mapaInicial (15,20)) [Jogador (5,2) B 0 0 5,Jogador (4,10) E 5 5 0,Jogador (4,5) D 1 2 4] [(DisparoCanhao 0 (1,2) D)]]

-- * Funções principais da Tarefa 3.

-- | Comprime um 'Estado' para formato textual.
--
-- __NB:__ A função 'show' representa um 'Estado' num formato textual facilmente legível mas extenso.
--
-- __NB:__ Uma boa solução deve representar o 'Estado' dado no mínimo número de caracteres possível.
comprime :: Estado -> String
comprime (Estado m lj ld) = comprimeMapa m ++ "#" ++ comprimeListaJogador lj ++ "*" ++ comprimeListaDisparos ld

-- | Comprime o Mapa
comprimeMapa :: Mapa -> String
comprimeMapa [] = []
comprimeMapa [x] = comprimeLinhaMapa x
comprimeMapa (h:t) = comprimeLinhaMapa h ++ "|" ++ comprimeMapa t

-- | Comprime uma linha do Mapa
comprimeLinhaMapa :: [Peca] -> String
comprimeLinhaMapa (Bloco Indestrutivel:t) = "i" ++ comprimeLinhaMapa t
comprimeLinhaMapa (Bloco Destrutivel:t) = "d" ++ comprimeLinhaMapa t
comprimeLinhaMapa (Vazia:t) = "v" ++ comprimeLinhaMapa t
comprimeLinhaMapa [] = []

-- | Comprime a lista dos Jogadores
comprimeListaJogador :: [Jogador] -> String
comprimeListaJogador [] = []
comprimeListaJogador [x] = comprimeJogador x
comprimeListaJogador (h:t) = comprimeJogador h ++ "|" ++ comprimeListaJogador t

-- | Comprime Jogador a Jogador
comprimeJogador :: Jogador -> String
comprimeJogador  (Jogador (x,y) d v l c) = show x ++ "$" ++ show y ++ "$" ++ show d ++ "$" ++ show v ++ "$" ++ show l ++ "$" ++ show c

-- | Comprime a lista de Disparos
comprimeListaDisparos :: [Disparo] -> String
comprimeListaDisparos [] = []
comprimeListaDisparos [x] = comprimeDisparo x
comprimeListaDisparos (h:t) = comprimeDisparo h ++ "|" ++ comprimeListaDisparos t

-- | Comprime Disparo a Disparo
comprimeDisparo :: Disparo -> String
comprimeDisparo (DisparoCanhao n (x,y) d) = "C" ++ "$" ++ show n ++ "$" ++ show x ++ "$" ++ show y ++ "$" ++ show d
comprimeDisparo (DisparoLaser n (x,y) d) = "L" ++ "$" ++ show n ++ "$" ++ show x ++ "$" ++ show y ++ "$" ++ show d
comprimeDisparo (DisparoChoque n t) = "S" ++ "$" ++ show n ++ "$" ++ show t

-- | Descomprime um 'Estado' no formato textual utilizado pela função 'comprime'.
--
-- __NB:__ A função 'comprime' é válida de for possível recuperar o 'Estado' utilizando a função 'descomprime', i.e.:
--
-- prop> descomprime . comprime = id
--
-- __NB:__ Esta propriedade é particularmente válida para a solução pré-definida:
--
-- prop> read . show = id
descomprime :: String -> Estado
descomprime s = Estado (dcMapa (separaMapa s)) (dcJogadorLista (separaJogador (drop ((sumStringDel s 1) +1) s))) (dcDisparaLista (drop ((sumStringDel s 2)+2) s))

-- | Divide as diferentes strings
auxString :: String -> String
auxString [] = []
auxString (h:t) | h == '|' = []
                | otherwise = h : auxString t

-- | Soma o numero de strings que já foram descomprimidas
sumStringDel :: String -> Int -> Int
sumStringDel [] _ = 0
sumStringDel (h:t) r | r == 0 = 0
                     | h == '#' = sumStringDel t (r-1)
                     | h == '*' = sumStringDel t (r-1)
                     | otherwise = 1 + sumStringDel t r

-- | Descomprime a string de mapa
dcMapa :: String -> Mapa
dcMapa [] = []
dcMapa s = dcMapaLinha (auxString s) : dcMapa (drop (length (auxString s) + 1) s)

-- | Auxiliar a dcMapa. Descomprime a string de peças.
dcMapaLinha :: String -> [Peca]
dcMapaLinha [] = []
dcMapaLinha (h:t) | h == 'i' = Bloco Indestrutivel : dcMapaLinha t
                  | h == 'd' = Bloco Destrutivel : dcMapaLinha t
                  | h == 'v' = Vazia : dcMapaLinha t
                  | otherwise = []

-- | Separa a string principal, e devolve apenas a string que se encontra antes do '#'
separaMapa :: String -> String
separaMapa [] = []
separaMapa (h:t) | h == '#' = []
                 | otherwise = h : separaMapa t

-- | Soma todas as "sub-strings" de cada string. Isto é, soma todos os elemntos da string que já forma trabalhados
sumElemDel :: String -> Int-> Int
sumElemDel [] _ = 0
sumElemDel (h:t) r | r == 0 = 0
                   | h == '$' = sumElemDel t (r-1)
                   | otherwise = 1 + sumElemDel t r

-- | Descomprime a string Jogador
dcJogadorLista :: String -> [Jogador]
dcJogadorLista [] = []
dcJogadorLista ('*':_) = []
dcJogadorLista s = dcJogador (auxString s) : dcJogadorLista (drop (length (auxString s)+1) s)

-- | Auxiliar a dcJogadorLista. Descomprime a string de um jogador
dcJogador :: String -> Jogador
dcJogador s = Jogador ((read (dcPosX s):: Int),(read (dcPosY (drop ((sumElemDel s 1)+1) s)):: Int)) (read (dcDirD (drop ((sumElemDel s 2)+2) s)) :: Direcao) (read (dcVidas (drop ((sumElemDel s 3)+3) s)):: Int) (read (dcLaser (drop ((sumElemDel s 4)+4)s)):: Int) (read (dcChoque (drop ((sumElemDel s 5)+5)s)):: Int)

-- | Descomprime o X
dcPosX :: String -> String
dcPosX ('$':_) = []
dcPosX (h:t) = h : dcPosX t

-- | Descomprime o Y
dcPosY :: String -> String
dcPosY [] = []
dcPosY ('$':_) = []
dcPosY (h:t) = h : dcPosY t

-- | Descomprime a direção
dcDirD :: String -> String
dcDirD [] = []
dcDirD ('$':_) = []
dcDirD (h:t) | h == 'C' = h : dcDirD t
             | h == 'D' = h : dcDirD t
             | h == 'E' = h : dcDirD t
             | h == 'B' = h : dcDirD t
             | otherwise = []

-- | Descomprime as Vidas
dcVidas :: String -> String
dcVidas ('$':_) = []
dcVidas (h:t) = h : dcVidas t

-- | Descomprime o numero de Laser
dcLaser :: String -> String
dcLaser ('$':_) = []
dcLaser (h:t) = h : dcLaser t

-- | Descomprime o numero de Choque
dcChoque :: String -> String
dcChoque [] = []
dcChoque ('|':_) = []
dcChoque ('*':_) = []
dcChoque (h:t) = h : dcChoque t

-- | Isola a string do jogador
separaJogador :: String -> String
separaJogador (h:t) | h=='*' = []
                    | otherwise = h : separaJogador t

-- | Descomprime a string de Disparos
dcDisparaLista :: String -> [Disparo]
dcDisparaLista [] = []
dcDisparaLista s = dcDispara (auxString s) : dcDisparaLista (drop (length (auxString s)+1)s)

-- | Auxiliar a dcDisparaLista. Descomprime um disparo
dcDispara :: String -> Disparo
dcDispara ('C':'$':t) = DisparoCanhao (read (dcIndiceJog t)::Int) ((read (dcPosX (drop ((sumElemDel t 1)+1) t)):: Int),(read (dcPosY (drop ((sumElemDel t 2)+2) t)):: Int)) (read (dcDirD (drop ((sumElemDel t 3)+3) t)) :: Direcao)
dcDispara ('L':'$':t) = DisparoLaser (read (dcIndiceJog t)::Int) ((read (dcPosX (drop ((sumElemDel t 1)+1) t)):: Int),(read (dcPosY (drop ((sumElemDel t 2)+2) t)):: Int)) (read (dcDirD (drop ((sumElemDel t 3)+3) t)) :: Direcao)
dcDispara ('S':'$':t) = DisparoChoque (read (dcIndiceJog t)::Int) (read (dcTicks (drop ((sumElemDel t 1)+1) t)):: Ticks)

-- | Descomprime o Indice do Jogador
dcIndiceJog :: String -> String
dcIndiceJog [] = []
dcIndiceJog ('$':_) = []
dcIndiceJog (h:t) = h : dcIndiceJog t

-- | Descomprime os Ticks de um disparo
dcTicks :: String -> String
dcTicks [] = []
dcTicks ('|':_) = []
dcTicks (h:t) = h : dcTicks t
