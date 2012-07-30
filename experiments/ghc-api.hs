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
    session <- getSession
    if (succeeded result) then liftIO $ putStrLn "Yay!"
                          else liftIO $ putStrLn "D'oh!"
