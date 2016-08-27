import System.Exit
import CabalMessage (message)

main = putStrLn message >> exitFailure
