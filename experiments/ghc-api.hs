import GHC
import GHC.Paths (libdir )

main =
  defaultErrorHandler $
  defaultCleanupHandler $
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    setSessionDynFlags (dflags{ hscTarget = HscNothing })
    t <- guessTarget "A.hs" Nothing
    setTargets [t]
    result <- load LoadAllTargets
    if (suceeded result) then putStrLn "Yay!"
                         else putStrLn "D'oh!"
