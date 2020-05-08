import Text.Show.Functions()
--import Data.List (genericLength)


--Record Syntax!
data Jugador = Jugador { 
    nombreDelJugador :: String,
    cantDinero :: Int,
    tacticaJugador :: String,
    propiedadesAdquiridas :: [(String, Int)],
    accionesJugador :: [Jugador -> Jugador]

    } deriving (Show)

type Acciones = Jugador -> Jugador
type Propiedades = (String, Int)


----------------------------------------------------------------------------
--Modelar a Carolina y Manuel.
----------------------------------------------------------------------------

carolina :: Jugador
carolina = Jugador "Carolina" 500 "Accionista" [] [pasarPorElBanco, pagarAAccionistas]

manuel :: Jugador
manuel = Jugador "Manuel" 500 "Oferente singular" [] [pasarPorElBanco, enojarse] 

--PREGUNTA! porque si los modelo asi (como estan abajo en comentario) me tiran warning en violeta, 
--y otra pregunta si no pongo los [] me tira error, es necesario ponerlos aunque no tengamos nada?

--carolina = Jugador { nombreDelJugador = "Carolina", cantDinero = 500, tacticaJugador = "Accionista", accionesJugador = [pagarAAccionistas, pasarPorElBanco] }
--manuel = Jugador { nombreDelJugador = "Manuel", cantDinero = 500, tacticaJugador = "Oferente Singular", accionesJugador = [pasarPorElBanco, enojarse] }


---------------------------------------------------------------------------
--Modelar las acciones.
---------------------------------------------------------------------------

--pasarPorElBanco: aumenta el dinero del jugador en $40 y cambia su táctica a “Comprador compulsivo”.
pasarPorElBanco :: Acciones
pasarPorElBanco unJugador = unJugador  {cantDinero  = cantDinero unJugador + 40 , tacticaJugador  = "Comprador compulsivo"}

--enojarse: suma $50 y agrega gritar a sus acciones.
enojarse :: Acciones
enojarse unJugador = unJugador { cantDinero = cantDinero unJugador + 50, accionesJugador = accionesJugador unJugador ++ [gritar]}

--gritar : agrega “AHHHH” al principio de su nombre.
gritar :: Acciones
gritar unJugador = unJugador { nombreDelJugador = "AHHHH" ++ nombreDelJugador unJugador }

--pagarAAccionistas: resta $100 para todos los casos excepto que la táctica sea “Accionista”, en ese caso suma $200.
cumpleTactica :: Jugador -> Bool
cumpleTactica unJugador = tacticaJugador unJugador== "Accionista"

pagarAAccionistas :: Acciones
pagarAAccionistas unJugador 
    | not(cumpleTactica unJugador) = unJugador { cantDinero = cantDinero unJugador - 100 }
     | otherwise =   unJugador { cantDinero = cantDinero unJugador + 200 }
  

{-- subastar: al momento de una subasta solo quienes tengan como tácticas “Oferente singular” o 
-“Accionista” podrán ganar la propiedad. Ganar implica restar el precio de la propiedad de su
--dinero y sumar la nueva adquisición a sus propiedades.
-}
cumpleTacticaParaPropiedad :: Jugador -> Bool 
cumpleTacticaParaPropiedad unJugador  = tacticaJugador unJugador == "Oferente singular" || tacticaJugador unJugador == "Accionista"
 
subastar :: Propiedades -> Acciones
subastar  unaPropiedad unJugador | cumpleTacticaParaPropiedad unJugador = unJugador {cantDinero = cantDinero unJugador - (snd unaPropiedad), propiedadesAdquiridas = propiedadesAdquiridas unJugador ++ [unaPropiedad]}   
    | otherwise = unJugador


----------------------------------------------------------------------------------------------------------------------------------------------------

--cobrarAlquileres: suma $10 por cada propiedad barata y $20 por cada propiedad cara obtenida. Las propiedades baratas son aquellas cuyo precio es menor a $150.-

esBarata :: Propiedades -> Bool
esBarata unaPropiedad = (snd unaPropiedad) < 150
esCara :: Propiedades -> Bool
esCara unaPropiedad = not (esBarata unaPropiedad)

asignarValor :: Propiedades -> Int
asignarValor unaPropiedad  |  esBarata unaPropiedad = 10
  | otherwise = 20

cobrarAlquileres :: Acciones
cobrarAlquileres unJugador = unJugador { cantDinero = cantDinero unJugador + (sum.(map asignarValor).propiedadesAdquiridas) unJugador }



