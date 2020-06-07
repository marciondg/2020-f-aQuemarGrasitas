module Lib where
import Data.List
import Text.Show.Functions

laVerdad = True
--De cada gimnasta nos interesa saber su edad, su peso y su coeficiente de tonificación.
data Gimnasta = UnGimnasta{
    nombre :: String,
    edad :: Float,
    peso :: Float,
    coeficienteTonificacion :: Float
}deriving(Show)

pancho = UnGimnasta "Francisco" 40.0 120.0 1.0
andres = UnGimnasta "Andy" 22.0 80.0 6.0
gaston = UnGimnasta "Violin Infumable" 22.0 103.0 3.0
type Tiempo = Float
--Los ejercicios que se pueden hacer son funciones que dado un gimnasta y una cantidad de minutos, 
--retorna a el gimnasta luego de realizar el ejercicio. 
type Ejercicio = Tiempo->Gimnasta->Gimnasta

-- EJ: Gimnasta no hace nada (y por ende queda igual que al principio sin importar cuánto tiempo lo realice) podría ser:
relax :: Ejercicio
relax _ gimnasta = gimnasta
{- 
===========Punto 1===========
Saber si alguien está saludable, lo cual se cumple si no está obeso y tiene una tonificación mayor a 5. 
Alguien es obeso si pesa más de 100 kilos.  -}

saludable :: Gimnasta->Bool
saludable gimnasta = (not.estaObeso) gimnasta && ((>5).coeficienteTonificacion) gimnasta  

estaObeso :: Gimnasta -> Bool
estaObeso gimnasta = ((>100).peso) gimnasta 

{- 
===========Punto 2===========
Hacer que el gimnasta queme una cantidad de calorías, lo que produce que baje de peso.
•Si el gimnasta es obeso, baja 1 kilo cada 150 calorías quemadas.
•Si no es obeso pero tiene más de 30 años y las calorías quemadas son más de 200, baja siempre un kilo.
•En cualquier otro caso se baja la cantidad de calorías quemadas dividido por el producto entre el peso y la edad del gimnasta.  -}

quemarCalorias :: Float -> Gimnasta -> Gimnasta
quemarCalorias calorias gimnasta  
--    | estaObeso gimnasta = reducirPeso (fromIntegral (floor (calorias / 150.0))) gimnasta
    | estaObeso gimnasta = reducirPeso (calorias / 150.0) gimnasta          
    | (not.estaObeso) gimnasta && ((>30).edad) gimnasta && (>200) calorias = reducirPeso 1 gimnasta
    | otherwise = reducirPeso (calorias / (peso gimnasta * edad gimnasta)) gimnasta

reducirPeso :: Float->Gimnasta->Gimnasta
reducirPeso cantidad gimnasta = gimnasta {peso= peso gimnasta - cantidad}
                                    
{- 
===========Punto 3===========
Desarrollar las funciones para los ejercicios caminataEnCinta, entrenamientoEnCinta, pesas, colina y montania sabiendo que:
•La cinta quema calorías en función de la velocidad promedio alcanzada durante el ejercicio, 
quemando 1 caloría por la velocidad promedio por minuto.
    ♦La caminata es un ejercicio en cinta con velocidad constante de 5 km/h. 
    ♦El entrenamiento en cinta arranca en 6 km/h y cada 5 minutos incrementa la velocidad en 1 km/h, 

> caminata 40 pancho
(40, 118.6 , 1) ­­­ quema 200 calorías (1*5*40)
    con lo cual la velocidad máxima dependerá de los minutos de entrenamiento.
-}
type Velocidad = Float
caminataEnCinta :: Ejercicio
caminataEnCinta tiempo = quemarCalorias $ (*5) tiempo

entrenamientoEnCinta :: Ejercicio
entrenamientoEnCinta tiempo = quemarCalorias $ velocidadPromedio tiempo * tiempo

velocidadPromedio :: Tiempo -> Velocidad
velocidadPromedio tiempo = (velocidadInicial + velocidadFinal tiempo) / 2

velocidadInicial :: Velocidad
velocidadInicial = 6

velocidadFinal :: Tiempo->Velocidad
velocidadFinal tiempo = velocidadInicial + (tiempo/5)
{- 
•Las pesas tonifican la décima parte de los kilos a levantar si se realiza por más de 10 minutos, sino nada.  -}

pesas :: Ejercicio
pesas tiempo gimnasta | tiempo > 10 = tonificar (peso gimnasta / 10) gimnasta
                      | otherwise = gimnasta

tonificar :: Float->Gimnasta->Gimnasta
tonificar cantidad gimnasta = gimnasta {coeficienteTonificacion = cantidad + coeficienteTonificacion gimnasta}

{- 
•La colina quema 2 calorías por minuto multiplicado por la inclinación de la colina.  -}

colina :: Float -> Ejercicio
colina inclinacion tiempo = quemarCalorias (2*tiempo*inclinacion)

{- 
•La montaña son 2 colinas sucesivas (cada una con la mitad de duración respecto de los minutos totales indicados), 
donde la segunda colina tiene una inclinación de 3 más que la inclinación inicial elegida. 
Además de hacer perder peso por las calorías quemadas por las colinas, 
este ejercicio incrementa en una unidad la tonificación de la gimnasta. Resolver usando composición y aplicación parcial.  -}


montaña :: Float -> Ejercicio 
montaña inclinacion tiempo = tonificar 1.segundaColina inclinacion tiempo .primerColina inclinacion tiempo

primerColina :: Float -> Ejercicio
primerColina inclinacion tiempo = colina inclinacion (tiempo/2)

segundaColina :: Float -> Ejercicio
segundaColina inclinacion tiempo = colina ((+3) inclinacion) ((/2) tiempo)

{-
============= Punto 4 =================

Rutina de ejercicios:
a. Dada una rutina (data con un nombre, duración total y lista de ejercicios específicos) y
una persona, obtener a la persona luego de realizar la rutina. La cantidad de minutos
dedicada a cada ejercicio es la misma.
Mostrar un ejemplo de uso usando todos los ejercicios del punto anterior.
Resolver de dos formas:
● Con recursividad
● Con fold-}

data Rutina = UnaRutina {
    nombreRutina :: String,
    duracion :: Float,
    ejercicios :: [Ejercicio]
} deriving (Show)
{- 

nombreRutina :: Rutina->String
nombreRutina (nombre, _, _) = nombre

tiempoRutina :: Rutina->Tiempo
tiempoRutina (_, tiempo, _) = tiempo

ejerciciosRutina :: Rutina -> [Ejercicio]
ejerciciosRutina (_,_,ejercicios) = ejercicios

rutinaCompleta :: Rutina
rutinaCompleta = ("Completa", 60, listaDeEjercicios)
 -}
 
listaEjerciciosCompleta = [pesas,caminataEnCinta,entrenamientoEnCinta,colina 5,montaña 5,relax]

rutinaCompleta :: Rutina
rutinaCompleta = UnaRutina "Completa" 60 listaEjerciciosCompleta

realizarEjercicio :: Tiempo->Gimnasta->Ejercicio ->Gimnasta
realizarEjercicio tiempo gimnasta ejercicio = ejercicio tiempo gimnasta

-- Fold
realizarRutinaFold :: Rutina -> Gimnasta -> Gimnasta
realizarRutinaFold rutina gimnasta = foldl ((realizarEjercicio.tiempoPorEjercicio) rutina) gimnasta (ejercicios rutina)


--Recursividad

-- si bien es posible hacerlo recursivamente de una, es mucho más sencillo delegar la parte recursiva del problema a otra función
realizarRutinaRec :: Rutina -> Gimnasta -> Gimnasta
realizarRutinaRec rutina gimnasta = ejercitar (tiempoPorEjercicio rutina) (ejercicios rutina) gimnasta

ejercitar :: Tiempo->[Ejercicio]->Gimnasta->Gimnasta
ejercitar _ [] gimnasta = gimnasta
ejercitar tiempo (ejercicio:restoEjercicios) gimnasta = ejercitar tiempo restoEjercicios (ejercicio tiempo gimnasta)

tiempoPorEjercicio :: Rutina -> Tiempo
tiempoPorEjercicio rutina = (duracion rutina) / genericLength (ejercicios rutina)

{-
b. Dada una rutina y una persona, obtener el resumen de rutina que es una tupla con el
nombre de la misma, los kilos perdidos y la tonificación ganada por la persona al
realizarla.
-}
type ResumenRutina = (String, Float, Float)
resumenRutina :: Rutina -> Gimnasta -> ResumenRutina
resumenRutina rutina gimnasta = (nombreRutina rutina, kilosPerdidos rutina gimnasta, tonificacionGanada rutina gimnasta) 

kilosPerdidos :: Rutina->Gimnasta ->Float
kilosPerdidos rutina gimnasta = peso gimnasta - (peso.realizarRutinaFold rutina) gimnasta

tonificacionGanada :: Rutina->Gimnasta ->Float
tonificacionGanada rutina gimnasta = (coeficienteTonificacion.realizarRutinaFold rutina) gimnasta - coeficienteTonificacion gimnasta

{- 
============= Punto 5 =================
Dada una lista de rutinas, obtener un resumen de todas las que (individualmente) 
pueden llevar a un gimnasta dado a estar saludable.  -}

resumenDeRutinasSaludables :: Gimnasta->[Rutina]->[ResumenRutina]
resumenDeRutinasSaludables gimnasta rutinas = map (`resumenRutina` gimnasta) (listaRutinasSaludables gimnasta rutinas)

listaRutinasSaludables :: Gimnasta->[Rutina]->[Rutina]
listaRutinasSaludables gimnasta rutinas = filter (`rutinaSaludable` gimnasta) rutinas

rutinaSaludable :: Rutina->Gimnasta->Bool
rutinaSaludable rutina = saludable.realizarRutinaFold rutina


rutinaCorta = UnaRutina "Corta" 30 [pesas,caminataEnCinta,relax]
rutinaNeverPony = UnaRutina "Pura Sangre" 120 [pesas,pesas,pesas,pesas,pesas,caminataEnCinta,colina 5,colina 10,montaña 5,montaña 10]
rutinaMarcio = UnaRutina "Marcio" 360 [relax,relax]
rutinaNacho = UnaRutina "Nacho" 60 [pesas,pesas]