import Text.Show.Functions()
--import Data.List (genericLength)



--Record Syntax!

data Jugador = Jugador { 
    nombreDelJugador :: String,
    cantDinero :: Int,
    tacticaJugador :: String,
    propiedadesAdquiridas :: [Propiedad],
    accionesJugador :: [Accion]

    } deriving (Show)

type Accion = Jugador -> Jugador
type Propiedad = (String, Int)


----------------------------------------------------------------------------
--Modelar a Carolina y Manuel.
----------------------------------------------------------------------------

carolina :: Jugador
carolina = Jugador "Carolina" 500 "Accionista" [] [pasarPorElBanco, pagarAAccionistas]

manuel :: Jugador
manuel = Jugador "Manuel" 500 "Oferente singular" [] [pasarPorElBanco, enojarse] 

---------------------------------------------------------------------------
--Modelar las acciones.
---------------------------------------------------------------------------

--pasarPorElBanco: aumenta el dinero del jugador en $40 y cambia su táctica a “Comprador compulsivo”.

pasarPorElBanco :: Accion
pasarPorElBanco unJugador = unJugador  {cantDinero  = cantidadDinero unJugador 40 , tacticaJugador  = "Comprador compulsivo"}
                                                       
--enojarse: suma $50 y agrega gritar a sus acciones.
enojarse :: Accion

-------------------------[[ issue 14: modifique lo de agregar elementos a una lista ]]

enojarse unJugador = unJugador { cantDinero = cantidadDinero unJugador 50 , accionesJugador =  gritar : (accionesJugador unJugador) }



-------------------------------------------------------------------------------------------------
--cantidadDinero :: Int -> Jugador -> Jugador
--cantidadDinero unMonto unJugador = unJugador { cantDinero = cantDinero unJugador + unMonto }
cantidadDinero :: Jugador -> Int -> Int
cantidadDinero unJugador unDinero = (cantDinero  unJugador) +  unDinero 

------------------------------------------------------------------------------------------------------



--gritar : agrega “AHHHH” al principio de su nombre.
gritar :: Accion
gritar unJugador = unJugador { nombreDelJugador = "AHHHH" ++ nombreDelJugador unJugador }


--pagarAAccionistas: resta $100 para todos los casos excepto que la táctica sea “Accionista”, en ese caso suma $200.

-------------------------------------------------------------------------------------
--esDeTactica :: String -> Jugador -> Jugador
--esDeTactica unaTactica unJugador = unJugador { tacticaJugador = unaTactica }
esDeTactica :: String -> Jugador -> Bool
esDeTactica  unatactica unJugador = tacticaJugador unJugador == unatactica
------------------------------------------------------------------------------------



--------------------------------[[ issue 15: ya modifique y elimine la funcion cumpleTactica]]
pagarAAccionistas :: Accion
pagarAAccionistas unJugador 
    | esDeTactica "Accionista" unJugador = unJugador { cantDinero =  cantidadDinero unJugador (200) }
     | otherwise = unJugador {  cantDinero = cantidadDinero unJugador (-100)  }
  


{-- subastar: al momento de una subasta solo quienes tengan como tácticas “Oferente singular” o 
-“Accionista” podrán ganar la propiedad. Ganar implica restar el precio de la propiedad de su
--dinero y sumar la nueva adquisición a sus propiedades.
-}


--------------------------------[[ issue 15: ya modifique el uso de la funcion esDeTactica ]]
cumpleTacticaParaPropiedad :: Jugador -> Bool 
cumpleTacticaParaPropiedad unJugador  = esDeTactica "Oferente singular" unJugador || esDeTactica "Accionista" unJugador
 

----------------------------------[[issue 14: modifique lo de agregar elementos a una lista ]]

subastar :: Propiedad -> Accion
subastar  unaPropiedad unJugador | cumpleTacticaParaPropiedad unJugador = unJugador {cantDinero = cantDinero unJugador - (precio unaPropiedad), propiedadesAdquiridas = unaPropiedad : (propiedadesAdquiridas unJugador) }   
    | otherwise = unJugador


--cobrarAlquileres: suma $10 por cada propiedad barata y $20 por cada propiedad cara obtenida. Las propiedades baratas son aquellas cuyo precio es menor a $150.-

precio :: Propiedad -> Int 
precio (_, dinero) = dinero

esBarata :: Propiedad -> Bool
esBarata unaPropiedad =  precio unaPropiedad < 150

asignarValor :: Propiedad -> Int
asignarValor unaPropiedad  |  esBarata unaPropiedad = 10
  | otherwise = 20

precioDelAlquiler :: [Propiedad] -> Int
precioDelAlquiler = sum.map asignarValor

cobrarAlquileres :: Accion
cobrarAlquileres unJugador = unJugador { cantDinero = cantidadDinero unJugador ((precioDelAlquiler.propiedadesAdquiridas) unJugador) }


 



