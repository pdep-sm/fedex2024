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


esInternacional _ = True -- mockeada para que compile




















