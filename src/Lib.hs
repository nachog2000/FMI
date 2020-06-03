module Lib where
import Text.Show.Functions

laVerdad = True

type Receta = [Pais -> Pais]
-- 1)

data Pais = UnPais {
    ingresoPerCapitaEnDolares :: Float,
    poblacionActivaSectorPublico :: Int,
    poblacionActivaSectorPrivado :: Int,
    recursosNaturales :: [String],
    deudaConFMIEnMillonesDeDolares :: Float
}deriving (Show)

namibia :: Pais
namibia = UnPais {
    ingresoPerCapitaEnDolares = 4140,
    poblacionActivaSectorPublico = 400000,
    poblacionActivaSectorPrivado = 650000,
    recursosNaturales = ["Mineria","Ecoturismo"],
    deudaConFMIEnMillonesDeDolares = 50
}

--  2)

fmiPrestaMillonesDeDolares :: Float -> Pais -> Pais
fmiPrestaMillonesDeDolares deuda pais = pais {deudaConFMIEnMillonesDeDolares = deudaConFMIEnMillonesDeDolares pais +  deuda * 1.5} 


reducirIngresosPublicos :: Int -> Pais -> Pais
reducirIngresosPublicos cant pais = pais {
    poblacionActivaSectorPublico =  poblacionActivaSectorPublico pais - cant,
    ingresoPerCapitaEnDolares = ingresoPerCapitaEnDolares pais - ingresoPerCapitaEnDolares pais * disminuirIngresoPerCapita cant                                        
    }
disminuirIngresoPerCapita :: Int -> Float
disminuirIngresoPerCapita cantidadPuestos 
 | cantidadPuestos > 100 = 0.2
 | otherwise = 0.15

explotacionDeRecursosNaturales :: String -> Pais -> Pais
explotacionDeRecursosNaturales recurso  =  disminuirDeudaFMI . (dejarSinRecursosNaturales recurso)
dejarSinRecursosNaturales :: String -> Pais -> Pais   
dejarSinRecursosNaturales recurso pais = pais {recursosNaturales = filter (/=recurso) (recursosNaturales pais)}
disminuirDeudaFMI :: Pais -> Pais
disminuirDeudaFMI pais = pais {deudaConFMIEnMillonesDeDolares = deudaConFMIEnMillonesDeDolares pais - 2}

blindaje :: Pais -> Pais
blindaje pais = (fmiPrestaMillonesDeDolares (pbi pais * 0.5) . reducirIngresosPublicos 500) pais


pbi :: Pais -> Float
pbi pais = ingresoPerCapitaEnDolares pais * fromIntegral (poblacionActiva pais)
poblacionActiva :: Pais -> Int
poblacionActiva pais = poblacionActivaSectorPrivado pais + poblacionActivaSectorPrivado pais


-- 3)

receta :: Receta
receta = [fmiPrestaMillonesDeDolares 200, explotacionDeRecursosNaturales "Mineria"]

aplicarReceta :: Receta -> Pais -> Pais
aplicarReceta receta pais = foldr ($) pais receta


-- 4)
puedenZafar :: [Pais] -> [Pais]
puedenZafar = filter $ elem "Petroleo". recursosNaturales

totalDeudaFMI :: [Pais] -> Float
totalDeudaFMI = sum. map deudaConFMIEnMillonesDeDolares

