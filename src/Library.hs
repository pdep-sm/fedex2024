module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

{-1 
Crear el modelo necesario que mejor se adapte para la solución.

Los envíos tienen distintas características como información básica, éstas son: lugar de origen (ciudad y país), lugar de destino (idem), peso en kilogramos, precio base, categorías que brindan información del contenido (por ejemplo: tecnología, libro, música, mueble...etc), y varios impuestos asociados (IVA, Aduanero, etc.).

Además:
Indique el tipo de un cargo.
Indique el tipo de un impuesto.
-}
data Envio = Envio {
        origen :: Lugar,
        destino :: Lugar,
        peso :: Number,
        precioBase :: Number,
        categorias :: [Categoria],
        impuestos :: [Impuesto]
    } deriving Show

data Lugar = Lugar {
        ciudad :: String,
        pais :: String
    } deriving Show

type Categoria = String
type Impuesto = Envio -> Number -- ?
type Cargo = Envio -> Envio -- ?


{- 2 
Envío con origen en Buenos Aires, Argentina y con destino Utrecht, Países Bajos, de 2 kg. de peso, precio base de $220, con las categorías de música y tecnología, sin impuestos.
-}
envio1 = Envio {
    origen = Lugar "Buenos Aires" "Argentina",
    destino = Lugar "Utrecht" "Países Bajos",
    peso = 2,
    precioBase = 220,
    categorias = ["musica", "tecnologia"],
    impuestos = []
}

{-
Envío con origen California, Estados Unidos y con destino Miami, Estado Unidos, de 5 kg. de peso, precio base $1500, con categoría de libros, y con IVA e impuesto extraño.
-}
envio2 = Envio {
    origen = Lugar "California" "Estados Unidos",
    destino = Lugar "Miami" "Estados Unidos",
    peso = 5,
    precioBase = 1500,
    categorias = ["libros"],
    impuestos = [iva, extranio]
}

{- Impuestos -}
-- IVA: 20% del precio
iva = aplicaImpuesto (\ _ -> True) 0.2 -- o (const True)
-- Multicategoría: 1% del precio, se aplica cuando el envío tiene más de 3 categorías.
multicategoria = aplicaImpuesto ((>3).length.categorias) 0.01
-- Aduanero: 3%, pero sólo cuando el envío es internacional. Un pedido es internacional cuando los países de origen y destino difieren.
aduanero = aplicaImpuesto esInternacional 0.03 
-- Impuesto extraño: 10%, sólo si tiene precio par.
extranio = aplicaImpuesto (even.precioBase) 0.1 

aplicaImpuesto condicion factor envio
    | condicion envio = (* factor) . precioBase $ envio
    | otherwise       = 0

{- 2 cargos
Cargo categórico: Si el envío tiene una categoría X, se computa un porcentaje dado del  precio base.
Cargo por sobrepeso: Si el peso es menor o igual a un peso dado (en Kg.), no se afecta el precio. En el caso de ser mayor se le suma $80 por cada kilo que lo supere.
Cargo arbitrario: $50 adicionales. Porque sí.

- Un cargo categórico de “tecnología” de 18%.
-}
aplicarCargo condicion funcion envio
    | condicion envio = envio{ precioBase = funcion . precioBase $ envio }
    | otherwise       = envio

-- cargoCategorico :: String -> Number -> Cargo
cargoCategorico categoria factor =
    aplicarCargo (elem categoria . categorias) (* (1 + factor))

-- cargoTecnologia :: Cargo
cargoTecnologia = cargoCategorico "tecnología" 0.18

-- cargoSobrepeso :: Number -> Cargo
cargoSobrepeso pesoLimite envio =
    aplicarCargo ((pesoLimite<). peso) (+ (80 * (peso envio - pesoLimite))) envio

cargoArbitrario = aplicarCargo (const True) (+50)

{- 3 
Sobre el precio...
a. Saber si el precio base de un envío cuesta más que un valor determinado N.
b. Conocer si un envío es barato. Decimos que es barato si vale $1300 o menos (precio base).
-}

precioMayorA valor = (>valor) . precioBase

esBarato  = not. precioMayorA 1300

{-4 
Sobre los lugares...
a. Saber si un envío se dirige a un país determinado.
b. Dado un envío, determinar si es local o es internacional. Es local cuando los países de origen y de destino son iguales.
-}

seDirigeA unPais = (unPais==).pais.destino

esLocal envio = seDirigeA (pais.origen $ envio) envio
esInternacional = not.esLocal

{-5
A partir de un conjunto de envíos, obtener aquellos que tienen ciertas categorías.
Nota: No se puede usar expresiones lambda, definiciones locales ni funciones auxiliares.
-}

enviosConCategoria necesarias envios = 
    filter (flip all necesarias.flip elem.categorias) envios

--        filter (\ e -> all ( `elem` (categorias e) ) necesarias) envios
-- `necesarias` incluido en `categorias`














