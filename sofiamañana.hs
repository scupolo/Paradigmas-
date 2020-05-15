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
---pobrar en consola: pasarPorElBanco carolina
manuel :: Jugador
manuel = Jugador "Manuel" 500 "Oferente singular" [] [pasarPorElBanco, enojarse] 

---------------------------------------------------------------------------
--Modelar las acciones.
---------------------------------------------------------------------------

--pasarPorElBanco: aumenta el dinero del jugador en $40 y cambia su táctica a “Comprador compulsivo”.

pasarPorElBanco :: Accion
pasarPorElBanco  unJugador  =  (cambiarDinero 40 . cambiarTactica "Comprador compulsivo")unJugador

cambiarDinero :: Int -> Jugador -> Jugador
cambiarDinero unMonto unJugador = unJugador { cantDinero = cantDinero unJugador + unMonto }

cambiarTactica :: String -> Jugador -> Jugador
cambiarTactica unaTactica unJugador = unJugador { tacticaJugador = unaTactica }

                                                       
--enojarse: suma $50 y agrega gritar a sus acciones.
enojarse :: Accion
-------------------------[[ issue 14: modifique lo de agregar elementos a una lista y mejore la funcion ]]
enojarse unJugador =  (cambiarDinero 50. nuevaAccion gritar) unJugador

nuevaAccion :: Accion -> Accion
nuevaAccion unaAccion unJugador = unJugador {accionesJugador =  gritar : (accionesJugador unJugador) }

--gritar : agrega “AHHHH” al principio de su nombre.
gritar :: Accion
gritar unJugador = unJugador { nombreDelJugador = "AHHHH" ++ nombreDelJugador unJugador }

--pagarAAccionistas: resta $100 para todos los casos excepto que la táctica sea “Accionista”, en ese caso suma $200.

--------------------------------[[ issue 15: ya modifique y elimine la funcion cumpleTactica]]
pagarAAccionistas :: Accion
pagarAAccionistas unJugador 
    | esDeTactica "Accionista" unJugador = cambiarDinero 200 unJugador
     | otherwise = cambiarDinero  (-100) unJugador 
  
esDeTactica :: String -> Jugador -> Bool
esDeTactica  unatactica unJugador = tacticaJugador unJugador == unatactica


{-- subastar: al momento de una subasta solo quienes tengan como tácticas “Oferente singular” o 
-“Accionista” podrán ganar la propiedad. Ganar implica restar el precio de la propiedad de su
--dinero y sumar la nueva adquisición a sus propiedades.
-}

--------------------------------[[ issue 15: ya modifique el uso de la funcion esDeTactica ]]
cumpleTacticaParaPropiedad :: Jugador -> Bool 
cumpleTacticaParaPropiedad unJugador  = esDeTactica "Oferente singular" unJugador || esDeTactica "Accionista" unJugador
 

----------------------------------[[issue 14: modifique lo de agregar elementos a una lista ]]

subastar :: Propiedad -> Accion
subastar  unaPropiedad unJugador | cumpleTacticaParaPropiedad unJugador = nuevaPropiedad unaPropiedad unJugador
    | otherwise = unJugador

precio :: Propiedad -> Int 
precio (_, dinero) = dinero


nuevaPropiedad :: Propiedad -> Accion
nuevaPropiedad unaPropiedad unJugador = unJugador {cantDinero = cantDinero unJugador - (precio unaPropiedad), propiedadesAdquiridas = unaPropiedad : (propiedadesAdquiridas unJugador) }   


-----------------------------------------------------------------------------------------------------------------------------------

--cobrarAlquileres: suma $10 por cada propiedad barata y $20 por cada propiedad cara obtenida. Las propiedades baratas son aquellas cuyo precio es menor a $150.-

esBarata :: Propiedad -> Bool
esBarata unaPropiedad =  precio unaPropiedad < 150

asignarValor :: Propiedad -> Int
asignarValor unaPropiedad  |  esBarata unaPropiedad = 10
  | otherwise = 20

precioDelAlquiler :: [Propiedad] -> Int
precioDelAlquiler = sum.map asignarValor

-------------------------------- [[ issue 7: mejore las funciones ]]
cobrarAlquileres :: Accion
cobrarAlquileres unJugador = unJugador { cantDinero = cantDinero unJugador + ((precioDelAlquiler.propiedadesAdquiridas) unJugador) }

 ---tener en cuenta: composicion en terminal (gritar.pasarPorElBanco)carolina

------------------------------------------------------------------------------------------segunda parte TP

{-hacerBerrinchePor: cuando una persona hace un berrinche por una propiedad se le suman $10 y se la hace gritar, 
la persona sigue haciendo berrinche hasta que llegue a comprar la propiedad que quiere. -}

hacerBrerrinchePor :: Propiedad -> Accion
hacerBrerrinchePor unaPropiedad unJugador 
         | tieneDineroParaComprarPropiedad unaPropiedad unJugador = nuevaPropiedad unaPropiedad unJugador
        |otherwise = hacerBrerrinchePor unaPropiedad ((cambiarDinero 10 . gritar) unJugador)

tieneDineroParaComprarPropiedad :: Propiedad -> Jugador -> Bool
tieneDineroParaComprarPropiedad unaPropiedad unJugador = cantDinero unJugador >= precio unaPropiedad 

--probar consola: casablanca = ("blanca",30)
--poner puedeComprar casablanca carolina
--probrar hacerBerrinchePor casablanca carolina


--últimaRonda, que dado un participante retorna una acción equivalente a todas sus acciones.
                       
ultimaRonda :: Jugador -> Accion 
ultimaRonda unJugador = foldl1 (.) (accionesJugador unJugador)
--consola: (ultimaRonda carolina) manuel - con foldl1, todas las acciones de carolina las compone y arma una sola y se la aplica a manuel

podraJugarEnUltimaRonda :: Jugador -> Jugador --Accion                    
podraJugarEnUltimaRonda unJugador = (ultimaRonda unJugador) unJugador


dineroQueSeTieneEnUltimaRonda :: Jugador -> Int
dineroQueSeTieneEnUltimaRonda unJugador = (cantDinero.podraJugarEnUltimaRonda) unJugador


--Hacer una función juegoFinal la cual toma dos participantes y devuelve al ganador. 
juegoFinal :: Jugador  -> Jugador -> Jugador 
juegoFinal unJugador otroJugador | dineroQueSeTieneEnUltimaRonda unJugador > dineroQueSeTieneEnUltimaRonda otroJugador = podraJugarEnUltimaRonda unJugador
                                  | otherwise = podraJugarEnUltimaRonda unJugador
           





