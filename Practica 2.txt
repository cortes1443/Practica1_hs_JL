import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.Concurrent (threadDelay)
import Data.Maybe (mapMaybe)

-- Definición del tipo de datos para representar la información de un estudiante
data Estudiante = Estudiante {
    idEstudiante :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime
} deriving (Show, Read)

-- Función para registrar la entrada de un estudiante a la universidad
registrarEntrada :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarEntrada id tiempo universidad =
    Estudiante id tiempo Nothing : universidad

-- Función para registrar la salida de un estudiante de la universidad
registrarSalida :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarSalida id tiempo =
    map (
        \v -> if idEstudiante v == id then v { salida = Just tiempo } else v
    )

-- Función para buscar un estudiante por su ID
buscarEstudiante :: String -> [Estudiante] -> Maybe Estudiante
buscarEstudiante id universidad =
    find (\v -> idEstudiante v == id) universidad

-- Función para calcular el tiempo que un estudiante ha permanecido en la universidad
tiempoEnUniversidad :: Estudiante -> UTCTime -> NominalDiffTime
tiempoEnUniversidad estudiante tiempoActual =
    case salida estudiante of
        Just tiempoSalida -> diffUTCTime tiempoSalida (entrada estudiante)
        Nothing           -> diffUTCTime tiempoActual (entrada estudiante)

-- Función para guardar la información de los estudiantes en un archivo de texto
guardarUniversidad :: [Estudiante] -> IO ()
guardarUniversidad universidad = do
    resultado <- reintentar 5 (writeFile "universidad.txt" (unlines (map mostrarEstudiante universidad)))
    case resultado of
        Left ex -> putStrLn $ "Error guardando el registro de la universidad: " ++ show ex
        Right _ -> putStrLn "Registro guardado en el archivo universidad.txt."

-- Función para reintentar una operación en caso de error
reintentar :: Int -> IO a -> IO (Either IOException a)
reintentar 0 accion = catch (accion >>= return . Right) (\(ex :: IOException) -> return (Left ex))
reintentar n accion = do
    resultado <- catch (accion >>= return . Right) (\(ex :: IOException) -> return (Left ex))
    case resultado of
        Left _ -> do
            threadDelay 1000000  -- Esperar 1 segundo antes de reintentar
            reintentar (n - 1) accion
        Right val -> return (Right val)

-- Función para cargar la información de los estudiantes desde un archivo de texto
cargarUniversidad :: IO [Estudiante]
cargarUniversidad = do
    resultado <- try (readFile "universidad.txt") :: IO (Either IOException String)
    case resultado of
        Left ex -> do
            putStrLn $ "Error cargando el registro de la universidad: " ++ show ex
            return []
        Right contenido -> do
            let lineas = lines contenido
            return (map leerEstudiante lineas)
    where
        leerEstudiante linea = read linea :: Estudiante

-- Función para mostrar la información de un estudiante
mostrarEstudiante :: Estudiante -> String
mostrarEstudiante estudiante =
    idEstudiante estudiante ++ "," ++ show (entrada estudiante) ++ "," ++ show (salida estudiante)

-- Función para leer la información de los estudiantes desde un archivo de texto
leerUniversidad :: IO [Estudiante]
leerUniversidad = do
    contenido <- readFile "universidad.txt"
    let lineas = lines contenido
    return (mapMaybe parsearEstudiante lineas)
    where
        parsearEstudiante :: String -> Maybe Estudiante
        parsearEstudiante linea = case words linea of
            [id, entrada, salida] -> Just $ Estudiante id (read entrada) (readMaybeSalida salida)
            _ -> Nothing

        readMaybeSalida :: String -> Maybe UTCTime
        readMaybeSalida "Nothing" = Nothing
        readMaybeSalida salidaStr = Just (read salidaStr)

-- Función principal del programa
main :: IO ()
main = do
    universidad <- cargarUniversidad
    putStrLn "¡Bienvenido al Sistema de Gestión de Estudiantes en la Universidad!"
    cicloPrincipal universidad

-- Función para el ciclo principal del programa
cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal universidad = do
    putStrLn "Seleccione una opción:"
    putStrLn "1. Registrar entrada de estudiante"
    putStrLn "2. Registrar salida de estudiante"
    putStrLn "3. Buscar estudiante por ID"
    putStrLn "4. Listar los estudiantes de la universidad"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese la ID del estudiante:"
            idEstudiante <- getLine
            tiempoActual <- getCurrentTime
            let universidadActualizada = registrarEntrada idEstudiante tiempoActual universidad
            guardarUniversidad universidadActualizada
            cicloPrincipal universidadActualizada
        "2" -> do
            putStrLn "Ingrese la ID del estudiante:"
            idEstudiante <- getLine
            tiempoActual <- getCurrentTime
            let universidadActualizada = registrarSalida idEstudiante tiempoActual universidad
            guardarUniversidad universidadActualizada
            cicloPrincipal universidadActualizada
        "3" -> do
            putStrLn "Ingrese la ID del estudiante:"
            idEstudiante <- getLine
            tiempoActual <- getCurrentTime
            case buscarEstudiante idEstudiante universidad of
                Just estudiante -> do
                    let tiempoTotal = tiempoEnUniversidad estudiante tiempoActual
                    putStrLn $ "El estudiante con ID " ++ idEstudiante ++ " se encuentra en la universidad."
                    putStrLn $ "Tiempo en la universidad: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Estudiante no encontrado en la universidad."
            cicloPrincipal universidad
        "4" -> do
            putStrLn "Lista de estudiantes dentro de la universidad:"
            universidadActualizada <- leerUniversidad
            mapM_ (\v -> putStrLn $ "ID: " ++ idEstudiante v ++ ", Entrada: " ++ show (entrada v) ++ ", Salida: " ++ show (salida v)) universidadActualizada
            cicloPrincipal universidadActualizada
        "5" -> putStrLn "Terminando..."
        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal universidad
