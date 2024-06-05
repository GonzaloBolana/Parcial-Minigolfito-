module Library where

import PdePreludat

-- Modelo inicial
data Jugador = UnJugador
  { nombre :: String,
    padre :: String,
    habilidad :: Habilidad
  }
  deriving (Eq, Show)

data Habilidad = UnaHabilidad
  { fuerzaJugador :: Number,
    precisionJugador :: Number
  }
  deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (UnaHabilidad 25 60)

todd = UnJugador "Todd" "Ned" (UnaHabilidad 15 80)

rafa = UnJugador "Rafa" "Gorgory" (UnaHabilidad 10 1)

data Tiro = UnTiro
  { velocidad :: Number,
    precision :: Number,
    altura :: Number
  }
  deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b

-- Punto 1A:
type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro {velocidad = 10, precision = (precisionJugador habilidad) * 2, altura = 0}

madera :: Palo
madera habilidad = UnTiro {velocidad = 100, precision = (precisionJugador habilidad) / 2, altura = 5}

hierro :: Number -> Palo
hierro n habilidad = UnTiro {velocidad = (fuerzaJugador habilidad) * n, precision = (precisionJugador habilidad) / n, altura = max 0 (n - 3)}

-- Punto 1B:
palos :: [Palo]
palos = [putter, madera] ++ map hierro [1 .. 10]

-- Punto 2:

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)

-- Composicion y pointfree
golpe' :: Palo -> Jugador -> Tiro
golpe' palo = palo . habilidad

-- Punto 3:

tiroFrenado = UnTiro 0 0 0

tiroprueba = UnTiro 10 95 0

tiroprueba2 = UnTiro 21 80 6

type Obstaculo = Tiro -> Tiro

superaObstaculo :: (Tiro -> Bool) -> (Tiro -> Tiro) -> Obstaculo
superaObstaculo condicion efecto tiroInicial
  | condicion tiroInicial = efecto tiroInicial
  | otherwise = tiroFrenado

-- TUNEL:
{-
tunelConRampita :: Obstaculo
tunelConRampita tiroInicial
  | superaTunelConRampita tiroInicial = efectoTunel tiroInicial
  | otherwise = tiroFrenado
-}
tunelConRampita :: Obstaculo
tunelConRampita = superaObstaculo superaTunelConRampita efectoTunel

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita tiro = (precision tiro) > 90

efectoTunel :: Obstaculo
efectoTunel tiro = tiro {velocidad = (velocidad tiro) * 2, precision = 100, altura = 0}

-- LAGUNA:

{--
laguna :: Number -> Obstaculo
laguna largo tiroInicial
  | superaLaguna tiroInicial = efectoLaguna largo tiroInicial
  | otherwise = tiroFrenado
--}

laguna :: Number -> Obstaculo
laguna largo = superaObstaculo superaLaguna (efectoLaguna largo)

superaLaguna :: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && (between 1 5 . altura) tiro

efectoLaguna :: Number -> Obstaculo
efectoLaguna largo tiro = tiro {altura = altura tiro / largo}

-- HOYO:
{-

hoyo :: Obstaculo
hoyo tiroInicial
  | superaHoyo tiroInicial = efectoHoyo tiroInicial
  | otherwise = tiroFrenado

-}

hoyo :: Obstaculo
hoyo = superaObstaculo superaHoyo efectoHoyo

superaHoyo :: Tiro -> Bool
superaHoyo tiro = (between 5 20 . velocidad) tiro && (precision tiro) > 95

efectoHoyo _ = tiroFrenado

-- PUNTO 4.a:

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (paloUtilObstaculo jugador obstaculo) palos

paloUtilObstaculo :: Jugador -> Obstaculo -> Palo -> Bool
paloUtilObstaculo jugador obstaculo palo = obstaculo (golpe jugador palo) /= tiroFrenado

-- PUNTO 4.B:
-- Sin recursividad
obstaculosConsecutivos :: [Obstaculo] -> Tiro -> Number
obstaculosConsecutivos listaobstaculo tiroInicial = length . takeWhile (tiroSuperaObstaculo tiroInicial) $ listaobstaculo

tiroSuperaObstaculo :: Tiro -> Obstaculo -> Bool
tiroSuperaObstaculo tiro obstaculo = obstaculo tiro /= tiroFrenado

-- Con Recursividad
obstaculosConsecutivos' :: [Obstaculo] -> Tiro -> Number
obstaculosConsecutivos' [] _ = 0
obstaculosConsecutivos' (obstaculo : obstaculos) tiro
  | obstaculo tiro == tiroFrenado = 0
  | otherwise = 1 + obstaculosConsecutivos' obstaculos (obstaculo tiro)

-- PUNTO 4.C:
paloMasUtil :: [Obstaculo] -> Jugador -> Palo
paloMasUtil obstaculos jugador = paloUtil obstaculos jugador (palosUtiles jugador (head obstaculos))

paloUtil :: [Obstaculo] -> Jugador -> [Palo] -> Palo
paloUtil _ _ [palo] = palo
paloUtil obstaculos jugador (palocabeza : palosiguiente : palos)
  | obstaculosSuperados obstaculos jugador palocabeza >= obstaculosSuperados obstaculos jugador palosiguiente = paloUtil obstaculos jugador (palocabeza : palos)
  | otherwise = paloUtil obstaculos jugador (palosiguiente : palos)

obstaculosSuperados :: [Obstaculo] -> Jugador -> Palo -> Number
obstaculosSuperados obstaculos jugador palo = obstaculosConsecutivos obstaculos (golpe jugador palo)

-- PUNTO 5:
