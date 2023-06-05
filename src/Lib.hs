module Lib () where

--Aclaracion: El enunciado define la data como tuplas, pero los exámenes más cercanos en el tiempo no suelen usar esta metodologia y lo que se suele hacer es data, entonces lo voy a hacer así

--Punto 1:  
type Propuestas = [Propuesta]

data Propuesta = Propuesta{
    nombreAlumno   :: String,
    nombreProyecto :: String,
    skills         :: [Skill],
    añoDeLenguaje  :: Int
}

type Skill = String

data Mentor = Mentor{
    nombre             :: String,
    proyectosDeInteres :: [String],
    criterioDeVotacion :: Propuesta -> Int
}

puntosSegun :: Propuesta -> Mentor -> Int
puntosSegun unaPropuesta unMentor = (criterioDeVotacion unMentor) unaPropuesta + (sumarSiEsDeInteres unaPropuesta) unMentor

sumarSiEsDeInteres :: Propuesta -> Mentor -> Int
sumarSiEsDeInteres unaPropuesta unMentor
    | esDeInteres unaPropuesta unMentor = 1
    | otherwise                         = 0

esDeInteres :: Propuesta -> Mentor -> Bool
esDeInteres unaPropuesta = (elem (nombreProyecto unaPropuesta) . proyectosDeInteres)

propuestaTest :: Propuesta
propuestaTest = Propuesta "sarasa" "sar" ["comer"] 1900

mentor :: Mentor
mentor = Mentor "asd" ["sar"] (\propuesta -> (length.skills) propuesta)

puntajeTotal :: Propuesta -> [Mentor] -> Int
puntajeTotal unaPropuesta  =  sum . map (puntosSegun unaPropuesta)

--Punto 3:
propuestaConChances :: [Propuesta] -> [Propuesta]
propuestaConChances = filter (masDeN 3. skills)

masDeN :: (Ord a) => Int -> [a] -> Bool
masDeN unaCantidad = (>unaCantidad) . length


--Punto 4:
ranking ::  [Mentor] -> [Propuesta] -> [Resultado]
ranking unosMentores = map (resultados unosMentores)

type Resultado = (String, String, Int)

resultados :: [Mentor] -> Propuesta -> Resultado
resultados = armarTuplasDe3Elementos  puntajeTotal 


--Punto 5:
prupuestasDeInteres :: Mentor -> [Propuesta] -> [Propuesta]
prupuestasDeInteres unMentor = filter (flip esDeInteres unMentor)

--Punto 6:
resultadoConMasVotos :: [Mentor] -> [Propuesta] -> Resultado
resultadoConMasVotos unosMentores = (elDeMayor puntaje . ranking unosMentores)

elDeMayor :: (Ord b) => (a -> b) -> [a] -> a
elDeMayor transformador = foldl1 (mayorSegun transformador)

mayorSegun :: (Ord b) => (a -> b) -> a -> a -> a
mayorSegun transformador unValor otroValor
    | transformador unValor > transformador otroValor = unValor
    | otherwise                                       = otroValor

puntaje :: Resultado -> Int
puntaje (_, _, unPuntaje) = unPuntaje

--Punto 7:
nombreMentorMasInteresado :: Propuesta -> [Mentor] -> String
nombreMentorMasInteresado unaPropuesta  = nombre . elDeMayor (puntosSegun unaPropuesta)


--Punto 8:
proyectosElegidos :: [Mentor] -> [Propuesta] -> [(String , String, String)]
proyectosElegidos unosMentores =  map (armarPropuestaElegida unosMentores) . filter (esElegido unosMentores)

esElegido :: [Mentor] -> Propuesta -> Bool
esElegido unosMentores  = ((>12) . flip puntajeTotal unosMentores)

armarPropuestaElegida :: [Mentor] -> Propuesta -> (String , String, String)
armarPropuestaElegida   = armarTuplasDe3Elementos nombreMentorMasInteresado


armarTuplasDe3Elementos :: (Propuesta -> [Mentor] -> a) -> [Mentor] -> Propuesta ->  (String, String, a)
armarTuplasDe3Elementos unaFuncion unosMentores unaPropuesta   = (nombreAlumno unaPropuesta, nombreProyecto unaPropuesta, unaFuncion unaPropuesta unosMentores)

--Punto 9
{-
De la forma en que lo hice, que se agreguen nuevas propiedades no es problema. Si se llegan a eliminar propiedades, sí afectaría pq hay funciones que dejarían de exisitir y, en algunos casos, las funciones que se crearon dejarían de tener sentido.
-}

