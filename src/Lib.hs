module Lib where
import Text.Show.Functions

laVerdad = True


-- 1)

data Pais = UnPais {
    ingresoPerCapitaEnDolares :: Float,
    poblacionActivaSectorPublico :: Int,
    poblacionActivaSectorPrivado :: Int,
    recursosNaturales :: [String],
    deudaConFMIEnMillonesDeDolares :: Int
}deriving (Show)

namibia :: Pais
namibia = UnPais {
    ingresoPerCapitaEnDolares = 4140,
    poblacionActivaSectorPublico = 400000,
    poblacionActivaSectorPrivado = 650000,
    recursosNaturales = ["Mineria","Ecoturismo"],
    deudaConFMIEnMillonesDeDolares = 50
}