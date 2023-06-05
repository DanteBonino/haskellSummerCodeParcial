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

--Punto 2:
puntajeTotal :: Propuesta -> [Mentor] -> Int
puntajeTotal unaPropuesta  =  sum . map (puntosSegun unaPropuesta)

