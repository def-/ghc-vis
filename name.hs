import GHC
import HscTypes
import OccName
import Name

import Linker
import RtClosureInspect hiding ( Closure, getClosureData )

import GhcMonad
import HscTypes
import Id
import Name
import Var hiding ( varName )
import VarSet
import UniqSupply
import TcType
import GHC
import Outputable
import PprTyThing
import MonadUtils
import Exception

import Control.Monad
import Data.List
import Data.Maybe
import Data.IORef

import System.IO
import GHC.Exts

import GHC.Paths ( libdir )

--import Control.Monad.Ghc

force = False
bindThings = False

a = GHC.runGhc (Just libdir) $ do
  x <- return 1
  go emptyTvSubst "x"

tryid str = do
  tythings <- (catMaybes . concat) `liftM`
    mapM (\w -> GHC.parseName w >>=
    mapM GHC.lookupName)
    (words str)
  let ids = [id | AnId id <- tythings]
  return ids

-- Do the obtainTerm--bindSuspensions-computeSubstitution dance
go :: GhcMonad m => TvSubst -> String -> m (TvSubst, Term)
go subst str = do
    ids <- tryid str
    let id = head ids
    let id' = id `setIdType` substTy subst (idType id)
    term_    <- GHC.obtainTermFromId maxBound force id'
    term     <- tidyTermTyVars term_
    term'    <- if bindThings &&
                   False == isUnliftedTypeKind (termType term)
                  then bindSuspensions term
                  else return term
  -- Before leaving, we compare the type obtained to see if it's more specific
  --  Then, we extract a substitution,
  --  mapping the old tyvars to the reconstructed types.
    let reconstructed_type = termType term
    hsc_env <- getSession
    case (improveRTTIType hsc_env (idType id) (reconstructed_type)) of
      Nothing     -> return (subst, term')
      Just subst' -> do { traceOptIf Opt_D_dump_rtti
                            (fsep $ [text "RTTI Improvement for", ppr id,
                             text "is the substitution:" , ppr subst'])
                        ; return (subst `unionTvSubst` subst', term')}


tidyTermTyVars :: GhcMonad m => Term -> m Term
tidyTermTyVars t =
  withSession $ \hsc_env -> do
  let env_tvs      = tyThingsTyVars $ ic_tythings $ hsc_IC hsc_env
      my_tvs       = termTyVars t
      tvs          = env_tvs `minusVarSet` my_tvs
      tyvarOccName = nameOccName . tyVarName
      tidyEnv      = (initTidyOccEnv (map tyvarOccName (varSetElems tvs))
                     , env_tvs `intersectVarSet` my_tvs)
  return$ mapTermType (snd . tidyOpenType tidyEnv) t

-- From ghc/compiler/ghci/Debugger.hs
-- | Give names, and bind in the interactive environment, to all the suspensions
--   included (inductively) in a term
bindSuspensions :: GhcMonad m => Term -> m Term
bindSuspensions t = do
      hsc_env <- getSession
      inScope <- GHC.getBindings
      let ictxt        = hsc_IC hsc_env
          prefix       = "_t"
          alreadyUsedNames = map (occNameString . nameOccName . getName) inScope
          availNames   = map ((prefix++) . show) [(1::Int)..] \\ alreadyUsedNames
      availNames_var  <- liftIO $ newIORef availNames
      (t', stuff)     <- liftIO $ foldTerm (nameSuspensionsAndGetInfos availNames_var) t
      let (names, tys, hvals) = unzip3 stuff
      let ids = [ mkVanillaGlobal name ty 
                | (name,ty) <- zip names tys]
          new_ic = extendInteractiveContext ictxt (map AnId ids)
      liftIO $ extendLinkEnv (zip names hvals)
      modifySession $ \_ -> hsc_env {hsc_IC = new_ic }
      return t'
     where

--    Processing suspensions. Give names and recopilate info
        nameSuspensionsAndGetInfos :: IORef [String] ->
                                       TermFold (IO (Term, [(Name,Type,HValue)]))
        nameSuspensionsAndGetInfos freeNames = TermFold
                      {
                        fSuspension = doSuspension freeNames
                      , fTerm = \ty dc v tt -> do
                                    tt' <- sequence tt
                                    let (terms,names) = unzip tt'
                                    return (Term ty dc v terms, concat names)
                      , fPrim    = \ty n ->return (Prim ty n,[])
                      , fNewtypeWrap  = 
                                \ty dc t -> do 
                                    (term, names) <- t
                                    return (NewtypeWrap ty dc term, names)
                      , fRefWrap = \ty t -> do
                                    (term, names) <- t 
                                    return (RefWrap ty term, names)
                      }
        doSuspension freeNames ct ty hval _name = do
          name <- atomicModifyIORef freeNames (\x->(tail x, head x))
          n <- newGrimName name
          return (Suspension ct ty hval (Just n), [(n,ty,hval)])

-- From ghc/compiler/ghci/Debugger.hs
--    Create new uniques and give them sequentially numbered names
newGrimName :: MonadIO m => String -> m Name
newGrimName userName  = do
    us <- liftIO $ mkSplitUniqSupply 'b'
    let unique  = uniqFromSupply us
        occname = mkOccName varName userName
        name    = mkInternalName unique occname noSrcSpan
    return name

traceOptIf :: GhcMonad m => DynFlag -> SDoc -> m ()
traceOptIf flag doc = do
  dflags <- GHC.getSessionDynFlags
  when (dopt flag dflags) $ liftIO $ printForUser stderr alwaysQualify doc

