{-|
Module       : Main
Description  : Implementação do Jogo em Gloss
Copyright    : Maria Barros <a89525@alunos.uminho.pt>;
               Miguel Rocha <a89508@alunos.uminho.pt>

__INTRODUÇÃO__

* Este módulo é a implementação gráfica do jogo usando o __Gloss__ e as funções previamente definidas nas tarefas anteriores.

__OBJETIVOS E ESTRATÉGIAS UTILIZADAS__

* O objetivo foi, recebendo um __Estado__ do jogo, obter uma __representação gráfica__ do Estado e fazer os tanques __reagir a comandos__ e que esses comandos __alterassem__ o Estado do Jogo.

__CONCLUSÃO__

* Consideramos que esta tarefa tem aspetos que podem ser melhorados e que alguns comandos estão com erros. 



-}
module Main where

import LI11819
import Tarefa0_2018li1g086
import Tarefa1_2018li1g086
import Tarefa2_2018li1g086
import Tarefa4_2018li1g086
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- | Estado do Jogo
type EstadoGloss = (Estado,[Picture],[Picture],[Picture])

-- |Estado Inicial
estadoGlossInicial :: [Picture] -> [Picture] -> [Picture] -> EstadoGloss
estadoGlossInicial l1 l2 l3 = (Estado (constroi [Move D,Move D,Move D,Move D,Move D,Move B,Move B,Move B,Move B,Move B,
                                                 Move E,Move D,Desenha,Move E,Roda,MudaParede,Move E,Move C,Desenha,Move E,
                                                 Move B,Move E,Roda,Move E,Move C,Move C,Desenha,Move D,Move D,Move D,
                                                 MudaTetromino,MudaTetromino,Move D,Move D,MudaTetromino,Move C,Move C,Desenha])
                                                 [Jogador (1,1) D 5 5 5,Jogador (9,1) C 5 5 5,Jogador (1,9) B 5 5 5, Jogador (9,9) E 5 5 5] [],l1,l2,l3)


-- | Desenha Estado
desenhaEstadoGloss :: EstadoGloss -> Picture
desenhaEstadoGloss ((Estado m lj ld),l1,l2,l3) = pictures ((desenhaMapa m l1 (0,0)) ++ (desenhaJogador lj 0 l2) ++ (desenhaDisparos ld lj l3))

-- | Transforma o mapa numa lista de picture
desenhaMapa :: Mapa -> [Picture] -> (Float,Float) -> [Picture]
desenhaMapa [] l (x,y) = []
desenhaMapa (h:t) l (x,y) = (auxMapaLinhas h (x,y) l) ++ desenhaMapa t l (x+64,y)

-- | Transforma uma linha do mapa numa lista de picture
auxMapaLinhas :: [Peca] -> (Float,Float) -> [Picture] -> [Picture]
auxMapaLinhas [] (x,y) l = []
auxMapaLinhas (h:t) (x,y) [vazia,dest,indest] | h == Vazia = (translate (y-352) (x-352) vazia) : auxMapaLinhas t (x,y+64) [vazia,dest,indest]
                                              | h == Bloco Indestrutivel = (translate (y-352) (x-352) indest) : auxMapaLinhas t (x,y+64) [vazia,dest,indest]
                                              | h == Bloco Destrutivel = (translate (y-352) (x-352) dest) : auxMapaLinhas t (x,y+64) [vazia,dest,indest]
                                              | otherwise = []

-- | Transforma uma lista de jogadores numa lista de picture
desenhaJogador :: [Jogador] -> Int -> [Picture] -> [Picture]
desenhaJogador [] a l = []
desenhaJogador ((Jogador (x,y) d v l c):t) a [tank1,tank2,tank3,tank4] | a == 0 && d == C = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) (rotate 270 tank1)) : desenhaJogador t (a+1) [tank1,tank2,tank3,tank4]
                                                                       | a == 0 && d == D = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) tank1) : desenhaJogador t (a+1) [tank1,tank2,tank3,tank4]
                                                                       | a == 0 && d == B = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) (rotate 90 tank1)) : desenhaJogador t (a+1) [tank1,tank2,tank3,tank4]
                                                                       | a == 0 && d == E = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) (rotate 180 tank1)) : desenhaJogador t (a+1) [tank1,tank2,tank3,tank4]
                                                                       | a == 1 && d == C = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) (rotate 270 tank2)) : desenhaJogador t (a+1) [tank1,tank2,tank3,tank4]
                                                                       | a == 1 && d == D = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) tank2) : desenhaJogador t (a+1) [tank1,tank2,tank3,tank4]
                                                                       | a == 1 && d == B = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) (rotate 90 tank2)) : desenhaJogador t (a+1) [tank1,tank2,tank3,tank4]
                                                                       | a == 1 && d == E = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) (rotate 180 tank2)) : desenhaJogador t (a+1) [tank1,tank2,tank3,tank4]
                                                                       | a == 2 && d == C = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) (rotate 270 tank3)) : desenhaJogador t (a+1) [tank1,tank2,tank3,tank4]
                                                                       | a == 2 && d == D = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) tank3) : desenhaJogador t (a+1) [tank1,tank2,tank3,tank4]
                                                                       | a == 2 && d == B = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) (rotate 90 tank3)) : desenhaJogador t (a+1) [tank1,tank2,tank3,tank4]
                                                                       | a == 2 && d == E = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) (rotate 180 tank3)) : desenhaJogador t (a+1) [tank1,tank2,tank3,tank4]
                                                                       | a == 3 && d == C = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) (rotate 270 tank4)) : desenhaJogador t (a+1) [tank1,tank2,tank3,tank4]
                                                                       | a == 3 && d == D = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) tank4) : desenhaJogador t (a+1) [tank1,tank2,tank3,tank4]
                                                                       | a == 3 && d == B = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) (rotate 90 tank4)) : desenhaJogador t (a+1) [tank1,tank2,tank3,tank4]
                                                                       | a == 3 && d == E = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) (rotate 180 tank4)) : desenhaJogador t (a+1) [tank1,tank2,tank3,tank4]
                                                                       | otherwise = []


-- | Transforma uma lista de disparos numa lista de picture
desenhaDisparos :: [Disparo] -> [Jogador] -> [Picture] -> [Picture]
desenhaDisparos [] m lj = []
desenhaDisparos ((DisparoCanhao n (x,y) d):t) lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4] | n == 0 && d == D = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) (rotate 270 ca1)) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                           | n == 0 && d == B = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) ca1) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                           | n == 0 && d == E = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) (rotate 90 ca1)) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                           | n == 0 && d == C = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) (rotate 180 ca1)) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                           | n == 1 && d == D = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) (rotate 270 ca2)) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                           | n == 1 && d == B = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) ca2) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                           | n == 1 && d == E = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) (rotate 90 ca2)) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                           | n == 1 && d == C = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) (rotate 180 ca2)) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                           | n == 2 && d == D = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) (rotate 270 ca3)) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                           | n == 2 && d == B = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) ca3) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                           | n == 2 && d == E = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) (rotate 90 ca3)) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                           | n == 2 && d == C = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) (rotate 180 ca3)) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                           | n == 3 && d == D = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) (rotate 270 ca4)) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                           | n == 3 && d == B = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) ca4) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                           | n == 3 && d == E = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) (rotate 90 ca4)) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                           | n == 3 && d == C = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) (rotate 180 ca4)) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                           | otherwise = []
desenhaDisparos ((DisparoLaser n (x,y) d):t) lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4] | n == 0 && (d == D || d == E) = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) laV1) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                          | n == 0 && (d == C || d == B) = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) laH1) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                          | n == 1 && (d == D || d == E) = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) laV2) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                          | n == 1 && (d == C || d == B) = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) laH2) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                          | n == 2 && (d == D || d == E) = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) laV3) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                          | n == 2 && (d == C || d == B) = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) laH3) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                          | n == 3 && (d == D || d == E) = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) laV4) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                          | n == 3 && (d == C || d == B) = (translate ((64*(fromIntegral x))-352) ((64*(fromIntegral y))-352) laH4) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                          | otherwise = []
desenhaDisparos ((DisparoChoque n ti):t) lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4] | n == 0 = (translate ((64*(fromIntegral x1))-352) ((64*(fromIntegral y1))-352) ch1) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                      | n == 1 = (translate ((64*(fromIntegral x1))-352) ((64*(fromIntegral y1))-352) ch2) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                      | n == 2 = (translate ((64*(fromIntegral x1))-352) ((64*(fromIntegral y1))-352) ch3) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                      | n == 3 = (translate ((64*(fromIntegral x1))-352) ((64*(fromIntegral y1))-352) ch4) : desenhaDisparos t lj [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4]
                                                                                                                      | otherwise = []
                                                                                                                       where x1 = fst (auxDesenhaChoque (DisparoChoque n ti) lj 0)
                                                                                                                             y1 = snd (auxDesenhaChoque (DisparoChoque n ti) lj 0)
-- | Descobre a posição do disparo
auxDesenhaChoque :: Disparo -> [Jogador] -> Int -> Posicao
auxDesenhaChoque (DisparoChoque n ti) (h:t) a | n == a = posicaoJogador h
                                              | otherwise = auxDesenhaChoque (DisparoChoque n ti) t (a+1)


-- | Reage Evento
reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
reageEventoGloss ev (e,l1,l2,l3) = (reageEvento ev e,l1,l2,l3)

-- | Auxiliar reageEventoGloss, recebe um EventKey e aplica uma Instrução
reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (Char 's') Down _ _) e = jogada 0 (Movimenta B) e
reageEvento (EventKey (Char 'w') Down _ _) e = jogada 0 (Movimenta C) e
reageEvento (EventKey (Char 'a') Down _ _) e = jogada 0 (Movimenta E) e
reageEvento (EventKey (Char 'd') Down _ _) e = jogada 0 (Movimenta D) e
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) e = jogada 1 (Movimenta C) e
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) e = jogada 1 (Movimenta B) e
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) e = jogada 1 (Movimenta E) e
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) e = jogada 1 (Movimenta D) e
reageEvento (EventKey (Char 'g') Down _ _) e = jogada 2 (Movimenta B) e
reageEvento (EventKey (Char 't') Down _ _) e = jogada 2 (Movimenta C) e
reageEvento (EventKey (Char 'f') Down _ _) e = jogada 2 (Movimenta E) e
reageEvento (EventKey (Char 'h') Down _ _) e = jogada 2 (Movimenta D) e
reageEvento (EventKey (Char 'k') Down _ _) e = jogada 3 (Movimenta B) e
reageEvento (EventKey (Char 'i') Down _ _) e = jogada 3 (Movimenta C) e
reageEvento (EventKey (Char 'j') Down _ _) e = jogada 3 (Movimenta E) e
reageEvento (EventKey (Char 'l') Down _ _) e = jogada 3 (Movimenta D) e
reageEvento (EventKey (Char '1') Down _ _) e = jogada 0 (Dispara Canhao) e
reageEvento (EventKey (Char '2') Down _ _) e = jogada 0 (Dispara Laser) e
reageEvento (EventKey (Char '3') Down _ _) e = jogada 0 (Dispara Choque) e
reageEvento (EventKey (Char '4') Down _ _) e = jogada 1 (Dispara Canhao) e
reageEvento (EventKey (Char '5') Down _ _) e = jogada 1 (Dispara Laser) e
reageEvento (EventKey (Char '6') Down _ _) e = jogada 1 (Dispara Choque) e
reageEvento (EventKey (Char '7') Down _ _) e = jogada 2 (Dispara Canhao) e
reageEvento (EventKey (Char '8') Down _ _) e = jogada 2 (Dispara Laser) e
reageEvento (EventKey (Char '9') Down _ _) e = jogada 2 (Dispara Choque) e
reageEvento (EventKey (Char 'z') Down _ _) e = jogada 3 (Dispara Canhao) e
reageEvento (EventKey (Char 'x') Down _ _) e = jogada 3 (Dispara Laser) e
reageEvento (EventKey (Char 'c') Down _ _) e = jogada 3 (Dispara Choque) e
reageEvento _ e = e


-- | Reage Tempo
reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss
reageTempoGloss t (e,l1,l2,l3) = (reageTempo t e,l1,l2,l3)

-- | Auxiliar reageTempoGloss
reageTempo :: Float -> Estado -> Estado
reageTempo n e = tick e


-- * Função Principal
-- | Framerate
fr :: Int
fr = 10

-- | Display
dm :: Display
dm = InWindow "Minecraft War" (1000, 1000) (0, 0)

-- | Main Function
main :: IO ()
main = do
    vazia <- loadBMP "grass.bmp"
    dest <- loadBMP "dirt.bmp"
    indest <- loadBMP "bedrock.bmp"
    tank1 <- loadBMP "tank1.bmp"
    tank2 <- loadBMP "tank2.bmp"
    tank3 <- loadBMP "tank3.bmp"
    tank4 <- loadBMP "tank4.bmp"
    ca1 <- loadBMP "canhao1.bmp"
    ca2 <- loadBMP "canhao2.bmp"
    ca3 <- loadBMP "canhao3.bmp"
    ca4 <- loadBMP "canhao4.bmp"
    ch1 <- loadBMP "choque1.bmp"
    ch2 <- loadBMP "choque2.bmp"
    ch3 <- loadBMP "choque3.bmp"
    ch4 <- loadBMP "choque4 .bmp"
    laV1 <- loadBMP "laserV1.bmp"
    laV2 <- loadBMP "laserV2.bmp"
    laV3 <- loadBMP "laserV3.bmp"
    laV4 <- loadBMP "laserV4.bmp"
    laH1 <- loadBMP "laserH1.bmp"
    laH2 <- loadBMP "laserH2.bmp"
    laH3 <- loadBMP "laserH3.bmp"
    laH4 <- loadBMP "laserH4.bmp"
    play dm
        (greyN 0.5)
        fr
        (estadoGlossInicial [vazia,dest,indest] [tank1,tank2,tank3,tank4] [ca1,ca2,ca3,ca4,ch1,ch2,ch3,ch4,laV1,laV2,laV3,laV4,laH1,laH2,laH3,laH4])
        desenhaEstadoGloss
        reageEventoGloss
        reageTempoGloss
