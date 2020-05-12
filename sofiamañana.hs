import Text.Show.Functions()
--import Data.List (genericLength)


--Record Syntax!

--[[ mejore la expresividad en los tipos]]
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

--[[ ya modifique y delegue a la funcion cantidadDinero ]]

pasarPorElBanco :: Accion
pasarPorElBanco unJugador = unJugador  {cantDinero  = cantidadDinero unJugador 40 , tacticaJugador  = "Comprador compulsivo"}
                                                       
--enojarse: suma $50 y agrega gritar a sus acciones.
enojarse :: Accion


-- [[ no me permite poner accionesJugador unJugador  : [gritar] ]]
enojarse unJugador = unJugador { cantDinero = cantidadDinero unJugador 50 , accionesJugador =  accionesJugador unJugador ++ [gritar] }

cantidadDinero :: Jugador -> Int -> Int
cantidadDinero unJugador unDinero = (cantDinero  unJugador) +  unDinero 

--gritar : agrega “AHHHH” al principio de su nombre.
gritar :: Accion
gritar unJugador = unJugador { nombreDelJugador = "AHHHH" ++ nombreDelJugador unJugador }


--pagarAAccionistas: resta $100 para todos los casos excepto que la táctica sea “Accionista”, en ese caso suma $200.

-- [[ modifique y delegue a la funcion esDeTactica ]]

esDeTactica :: String -> Jugador -> Bool
esDeTactica  unatactica unJugador = tacticaJugador unJugador == unatactica

cumpleTactica :: Jugador -> Bool
cumpleTactica unJugador = esDeTactica "Accionista" unJugador

pagarAAccionistas :: Accion
pagarAAccionistas unJugador 
    | cumpleTactica unJugador = unJugador { cantDinero =  cantidadDinero unJugador (200) }
     | otherwise = unJugador {  cantDinero = cantidadDinero unJugador (-100)  }
  


{-- subastar: al momento de una subasta solo quienes tengan como tácticas “Oferente singular” o 
-“Accionista” podrán ganar la propiedad. Ganar implica restar el precio de la propiedad de su
--dinero y sumar la nueva adquisición a sus propiedades.
-}

cumpleTacticaParaPropiedad :: Jugador -> Bool 
cumpleTacticaParaPropiedad unJugador  = tacticaJugador unJugador == "Oferente singular" || tacticaJugador unJugador == "Accionista"
 

--[[ mejore la funcion subastar en delegar a la funcion precio pero sigo sin saber como usar el : en lugar de ++ ]] 
subastar :: Propiedad -> Accion
subastar  unaPropiedad unJugador | cumpleTacticaParaPropiedad unJugador = unJugador {cantDinero = cantDinero unJugador - (precio unaPropiedad), propiedadesAdquiridas = propiedadesAdquiridas unJugador ++ [unaPropiedad]}   
    | otherwise = unJugador


----------------------------------------------------------------------------------------------------------------------------------------------------

--cobrarAlquileres: suma $10 por cada propiedad barata y $20 por cada propiedad cara obtenida. Las propiedades baratas son aquellas cuyo precio es menor a $150.-

--[[ elimine la funcion que no usaba y delegue a la funcion precio ]] 
precio :: Propiedad -> Int 
precio (_, dinero) = dinero

esBarata :: Propiedad -> Bool
esBarata unaPropiedad =  precio unaPropiedad < 150

asignarValor :: Propiedad -> Int
asignarValor unaPropiedad  |  esBarata unaPropiedad = 10
  | otherwise = 20

--[[ ya mejore la funcion cobrar alquileres]]

cobrarAlquileres :: Accion
cobrarAlquileres unJugador = unJugador { cantDinero = cantidadDinero unJugador ((sum.(map asignarValor).propiedadesAdquiridas) unJugador) }


 



