import GHC.HeapView
import System.Mem
import System.Mem.StableName
import System.IO.Unsafe
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

instance Ord Box
  where compare (Box x) (Box y) = unsafePerformIO $ (do
          a <- makeStableName x
          b <- makeStableName y
          return $ compare (hashStableName a) (hashStableName b)
          )

type HeapEntry = (Maybe String, Closure)
type HeapMap   = Map Box HeapEntry

walkHeap :: a -> IO HeapMap
walkHeap a = do
  --performGC
  go (asBox a) Map.empty
  where go b l = case Map.lookup b l of
                   Just _  -> return l
                   Nothing -> do
                     c' <- getBoxedClosureData b
                     l' <- foldM (\l x -> go x l) (Map.insert b (Nothing, c') l) (allPtrs c')
                     return $ (Map.insert b (Nothing, c') l')

walkHeapDepth a d = do
  performGC
  go (asBox a) d Map.empty
  where go b d l | d == 0 = return l
                 | otherwise = case Map.lookup b l of
                     Just _  -> return l
                     Nothing -> do
                       c' <- getBoxedClosureData b
                       l' <- foldM (\l x -> go x (d - 1) l) (Map.insert b (Nothing, c') l) (allPtrs c')
                       return $ (Map.insert b (Nothing, c') l')

a x = 2 * x

--demoBCO x = do
--    a <- getClosureData x
--    b <- getBoxedClosureData $ fun a -- BCO
--    demoBCO2 b
--
--demoBCO2 b = do
--    c <- getBoxedClosureData $ bcoptrs b
--    ds <- mapM getBoxedClosureData (mccPayload c)
--    let e = head $ filter isBCO ds
--    putStrLn $ show e
--    demoBCO2 e
--  where isBCO (BCOClosure _ _ _ _ _ _ _) = True
--        isBCO _ = False
--
--getBCO b = do
--    c <- getBoxedClosureData $ bcoptrs b
--    ds <- mapM getBoxedClosureData (mccPayload c)
--    return $ head $ filter isBCO ds
--  where isBCO (BCOClosure _ _ _ _ _ _ _) = True
--        isBCO _ = False
--
--
-- fromList [(0x00007ff2b4e299c8
-- ,(Nothing,MutArrClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = MUT_ARR_PTRS_FROZEN, srtlen = 0}, mccPtrs = 2, mccSize = 3, mccPayload = [0x0000000041710d60,0x00007ff2b4e9ac10]}))
-- ,(0x00007ff2b4e9ac10
-- ,(Nothing,BCOClosure {info = StgInfoTable {ptrs = 4, nptrs = 0, tipe = BCO, srtlen = 0}, instrs = 0x00007ff2b5321000, literals = 0x00007ff2b4e9ac48, bcoptrs = 0x00007ff2b4e9ac58, arity = 2, size = 7, bitmap = 2}))
-- ,(0x00007ff2b4e9ac90
-- ,(Nothing,BCOClosure {info = StgInfoTable {ptrs = 4, nptrs = 0, tipe = BCO, srtlen = 0}, instrs = 0x00007ff2b531aa70, literals = 0x00007ff2b4e9acc8, bcoptrs = 0x00007ff2b4e9acd8, arity = 2, size = 7, bitmap = 2}))
-- ,(0x00007ff2b4e9acd8
-- ,(Nothing,MutArrClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = MUT_ARR_PTRS_FROZEN, srtlen = 0}, mccPtrs = 4, mccSize = 5, mccPayload = [0x00007ff2b54af338,0x00007ff2b60e7ee0/1,0x00007ff2b4e9ad18,0x0000000041710c50]}))
-- ,(0x0000000041710c50
-- ,(Nothing,FunClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = FUN_STATIC, srtlen = 0}, ptrArgs = [], dataArgs = []}))
-- ,(0x0000000041710c78
-- ,(Nothing,FunClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = FUN_STATIC, srtlen = 0}, ptrArgs = [], dataArgs = []}))
-- ,(0x00007ff2b4e9ad70
-- ,(Nothing,MutArrClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = MUT_ARR_PTRS_FROZEN, srtlen = 0}, mccPtrs = 1, mccSize = 2, mccPayload = [0x0000000041710c78]}))
-- ,(0x00007ff2b4e9ad50
-- ,(Nothing,ArrWordsClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = ARR_WORDS, srtlen = 0}, bytes = 16, arrWords = [2,1077696840]}))
-- ,(0x00007ff2b60e7f20
-- ,(Nothing,ArrWordsClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = ARR_WORDS, srtlen = 0}, bytes = 40, arrWords = [5629499534213120,9288678526418957,12884967425,21476147201,12947857518886938]}))
-- ,(0x00007ff2b4e9ad18
-- ,(Nothing,BCOClosure {info = StgInfoTable {ptrs = 4, nptrs = 0, tipe = BCO, srtlen = 0}, instrs = 0x00007ff2b60e7f20, literals = 0x00007ff2b4e9ad50, bcoptrs = 0x00007ff2b4e9ad70, arity = 1, size = 7, bitmap = 1}))
-- ,(0x00007ff2b60e8da8
-- ,(Nothing,ThunkClosure {info = StgInfoTable {ptrs = 3, nptrs = 0, tipe = THUNK, srtlen = 19}, ptrArgs = [0x00007ff2b60e7de0/1,0x00007ff2b60e7e10/1,0x00007ff2b60e7e28/3], dataArgs = []}))
-- ,(0x00007ff2b60e8d90/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 2, nptrs = 0, tipe = CONSTR_2_0, srtlen = 5}, ptrArgs = [0x00007ff2b60e7df8/1,0x00007ff2b60e7de0/1], dataArgs = [], pkg = "ghc-7.4.1", modl = "CoreSyn", name = "AnnLet"}))
-- ,(0x00007ff2b60e8220/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 2, nptrs = 0, tipe = CONSTR_2_0, srtlen = 0}, ptrArgs = [0x00007ff2b60e8da8,0x00007ff2b60e8d90/1], dataArgs = [], pkg = "ghc-prim", modl = "GHC.Tuple", name = "(,)"}))
-- ,(0x00007ff2b60e0718
-- ,(Nothing,ThunkClosure {info = StgInfoTable {ptrs = 1, nptrs = 0, tipe = THUNK_1_0, srtlen = 1}, ptrArgs = [0x00007ff2b60e8d90/1], dataArgs = []}))
-- ,(0x00007ff2b60e8dd0/2
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 2, nptrs = 0, tipe = CONSTR_2_0, srtlen = 1}, ptrArgs = [0x00007ff2b60e8178/3,0x0000000002946ed0/1], dataArgs = [], pkg = "ghc-prim", modl = "GHC.Types", name = ":"}))
-- ,(0x0000000002902568/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 0, nptrs = 1, tipe = CONSTR_NOCAF_STATIC, srtlen = 0}, ptrArgs = [], dataArgs = [0], pkg = "ghc-prim", modl = "GHC.Types", name = "I#"}))
-- ,(0x00007ff2b60e8238/3
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 2, nptrs = 0, tipe = CONSTR_2_0, srtlen = 2}, ptrArgs = [0x0000000002902568/1,0x00007ff2b60e8dd0/2], dataArgs = [], pkg = "ghc-7.4.1", modl = "CoreSyn", name = "Breakpoint"}))
-- ,(0x00007ff2b5317b90/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 2, nptrs = 0, tipe = CONSTR_2_0, srtlen = 7}, ptrArgs = [0x00007ff2b60e8238/3,0x00007ff2b60e8220/1], dataArgs = [], pkg = "ghc-7.4.1", modl = "CoreSyn", name = "AnnTick"}))
-- ,(0x00007ff2b53188e0
-- ,(Nothing,ThunkClosure {info = StgInfoTable {ptrs = 1, nptrs = 0, tipe = THUNK_1_0, srtlen = 1}, ptrArgs = [0x00007ff2b5317b90/1], dataArgs = []}))
-- ,(0x00007ff2b60e8d30
-- ,(Nothing,ThunkClosure {info = StgInfoTable {ptrs = 2, nptrs = 0, tipe = THUNK_2_0, srtlen = 4294967295}, ptrArgs = [0x00007ff2b60e7d30/1,0x0000000002946ed0/1], dataArgs = []}))
-- ,(0x00007ff2b60e8d50/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 2, nptrs = 0, tipe = CONSTR_2_0, srtlen = 0}, ptrArgs = [0x00007ff2b60e7d90,0x00007ff2b60e7d68/1], dataArgs = [], pkg = "ghc-prim", modl = "GHC.Tuple", name = "(,)"}))
-- ,(0x00007ff2b60e81f0
-- ,(Nothing,ThunkClosure {info = StgInfoTable {ptrs = 2, nptrs = 0, tipe = THUNK_2_0, srtlen = 0}, ptrArgs = [0x00007ff2b60e8d50/1,0x00007ff2b60e8d30], dataArgs = []}))
-- ,(0x00007ff2b60e8d68/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 2, nptrs = 2, tipe = CONSTR, srtlen = 0}, ptrArgs = [0x00007ff2b60e7db0/2,0x00007ff2b60e7dc8/2], dataArgs = [1627389952,2048], pkg = "containers-0.4.2.1", modl = "Data.IntMap", name = "Bin"}))
-- ,(0x00007ff2b60e8210/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 1, nptrs = 0, tipe = CONSTR_1_0, srtlen = 0}, ptrArgs = [0x00007ff2b60e8d68/1], dataArgs = [], pkg = "ghc-7.4.1", modl = "CorePrep", name = "CPE"}))
-- ,(0x00007ff2b5317b70
-- ,(Nothing,ThunkClosure {info = StgInfoTable {ptrs = 2, nptrs = 0, tipe = THUNK_2_0, srtlen = 0}, ptrArgs = [0x00007ff2b60e8210/1,0x00007ff2b60e81f0], dataArgs = []}))
-- ,(0x00007ff2b5317b40/2
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 4, nptrs = 1, tipe = CONSTR, srtlen = 1}, ptrArgs = [0x00007ff2b60e8178/3,0x00007ff2b60e81b0/1,0x00007ff2b60e81c0/2,0x0000000002930608/1], dataArgs = [2], pkg = "containers-0.4.2.1", modl = "Data.Map", name = "Bin"}))
-- ,(0x00007ff2b60e8dd0/2
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 2, nptrs = 0, tipe = CONSTR_2_0, srtlen = 1}, ptrArgs = [0x00007ff2b60e8178/3,0x0000000002946ed0/1], dataArgs = [], pkg = "ghc-prim", modl = "GHC.Types", name = ":"}))
-- ,(0x00007ff2b4e299f8
-- ,(Nothing,BCOClosure {info = StgInfoTable {ptrs = 4, nptrs = 0, tipe = BCO, srtlen = 0}, instrs = 0x00007ff2b4e299a0, literals = 0x00007ff2b4e29990, bcoptrs = 0x00007ff2b4e299c8, arity = 0, size = 6, bitmap = 0}))
-- ,(0x00007ff2b4e29a38
-- ,(Nothing,APClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = AP, srtlen = 0}, arity = 35903792, n_args = 0, fun = 0x00007ff2b4e299f8, payload = []}))
-- ,(0x00007ff2b4e9ac48
-- ,(Nothing,ArrWordsClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = ARR_WORDS, srtlen = 0}, bytes = 0, arrWords = []}))
-- ,(0x0000000002930608/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = CONSTR_NOCAF_STATIC, srtlen = 0}, ptrArgs = [], dataArgs = [], pkg = "containers-0.4.2.1", modl = "Data.Map", name = "Tip"}))
-- ,(0x00007ff2b5321000
-- ,(Nothing,ArrWordsClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = ARR_WORDS, srtlen = 0}, bytes = 48, arrWords = [6755399441055744,281479271678006,281487861743644,8444257891581954,8590065667,12947861813657626]}))
-- ,(0x00007ff2b60e8d20/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 0, nptrs = 1, tipe = CONSTR_0_1, srtlen = 0}, ptrArgs = [], dataArgs = [1], pkg = "base", modl = "GHC.Word", name = "W16#"}))
-- ,(0x00007ff2b4e9ac10
-- ,(Nothing,BCOClosure {info = StgInfoTable {ptrs = 4, nptrs = 0, tipe = BCO, srtlen = 0}, instrs = 0x00007ff2b5321000, literals = 0x00007ff2b4e9ac48, bcoptrs = 0x00007ff2b4e9ac58, arity = 2, size = 7, bitmap = 2}))
-- ,(0x0000000041710d58/1
-- ,(Nothing,FunClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = FUN_STATIC, srtlen = 0}, ptrArgs = [], dataArgs = []}))
-- ,(0x00007ff2b60e8ce8/3
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 5, nptrs = 1, tipe = CONSTR, srtlen = 2}, ptrArgs = [0x00007ff2b60e7ca0/1,0x00007ff2b60e7cc8/3,0x0000000002720280/2,0x0000000002718120/1,0x00007ff2b60e7ce0/1], dataArgs = [1929382908], pkg = "ghc-7.4.1", modl = "Var", name = "Id"}))
-- ,(0x00000000403c7a78/1
-- ,(Nothing,FunClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = FUN_STATIC, srtlen = 0}, ptrArgs = [], dataArgs = []}))
-- ,(0x00000000403c7b98/1
-- ,(Nothing,FunClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = FUN_STATIC, srtlen = 1}, ptrArgs = [], dataArgs = []}))
-- ,(0x00000000403c7b60/1
-- ,(Nothing,FunClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = FUN_STATIC, srtlen = 1}, ptrArgs = [], dataArgs = []}))
-- ,(0x00000000403c7b10/2
-- ,(Nothing,FunClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = FUN_STATIC, srtlen = 0}, ptrArgs = [], dataArgs = []}))
-- ,(0x00007ff2b60e81c0/2
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 4, nptrs = 1, tipe = CONSTR, srtlen = 1}, ptrArgs = [0x00007ff2b60e8ce8/3,0x00007ff2b60e8d20/1,0x0000000002930608/1,0x0000000002930608/1], dataArgs = [1], pkg = "containers-0.4.2.1", modl = "Data.Map", name = "Bin"}))
-- ,(0x00000000403c7b18/2
-- ,(Nothing,FunClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = FUN_STATIC, srtlen = 0}, ptrArgs = [], dataArgs = []}))
-- ,(0x00000000403c7b20/2
-- ,(Nothing,FunClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = FUN_STATIC, srtlen = 0}, ptrArgs = [], dataArgs = []}))
-- ,(0x0000000041710d60
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 7, nptrs = 0, tipe = CONSTR_STATIC, srtlen = 0}, ptrArgs = [0x00000000403c7b20/2,0x00000000403c7b18/2,0x00000000403c7b10/2,0x00000000403c7b60/1,0x00000000403c7b98/1,0x00000000403c7a78/1,0x0000000041710d58/1], dataArgs = [], pkg = "base", modl = "GHC.Num", name = "D:Num"}))
-- ,(0x00007ff2b4e299c8
-- ,(Nothing,MutArrClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = MUT_ARR_PTRS_FROZEN, srtlen = 0}, mccPtrs = 2, mccSize = 3, mccPayload = [0x0000000041710d60,0x00007ff2b4e9ac10]}))
-- ,(0x00007ff2b4e29990
-- ,(Nothing,ArrWordsClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = ARR_WORDS, srtlen = 0}, bytes = 0, arrWords = []}))
-- ,(0x00007ff2b4e299a0
-- ,(Nothing,ArrWordsClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = ARR_WORDS, srtlen = 0}, bytes = 20, arrWords = [2814749767106560,1407456487931909,3014657]}))
-- ,(0x00007ff2b4e29a38
-- ,(Nothing,APClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = AP, srtlen = 0}, arity = 35903792, n_args = 0, fun = 0x00007ff2b4e299f8, payload = []}))
-- ,(0x00007ff2b4e299f8
-- ,(Nothing,BCOClosure {info = StgInfoTable {ptrs = 4, nptrs = 0, tipe = BCO, srtlen = 0}, instrs = 0x00007ff2b4e299a0, literals = 0x00007ff2b4e29990, bcoptrs = 0x00007ff2b4e299c8, arity = 0, size = 6, bitmap = 0}))
-- ,(0x00007ff2b60e81b0/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 0, nptrs = 1, tipe = CONSTR_0_1, srtlen = 0}, ptrArgs = [], dataArgs = [0], pkg = "base", modl = "GHC.Word", name = "W16#"}))
-- ,(0x00007ff2b60df0f8/2
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 4, nptrs = 1, tipe = CONSTR, srtlen = 1}, ptrArgs = [0x00007ff2b60e0568/3,0x00007ff2b60dfc88/1,0x0000000002930608/1,0x00007ff2b60dfc98/2], dataArgs = [2], pkg = "containers-0.4.2.1", modl = "Data.Map", name = "Bin"}))
-- ,(0x00007ff2b60e06f0
-- ,(Nothing,ThunkClosure {info = StgInfoTable {ptrs = 3, nptrs = 0, tipe = THUNK, srtlen = 0}, ptrArgs = [0x00007ff2b60df0e8/1,0x00007ff2b60df0f8/2,0x00007ff2b60e8dd0/2], dataArgs = []}))
-- ,(0x00007ff2b60e7ee0/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 3, nptrs = 1, tipe = CONSTR, srtlen = 0}, ptrArgs = [0x00007ff2b532f3d8/1,0x00007ff2b60e06f0,0x00007ff2b60e0718], dataArgs = [0], pkg = "ghc-7.4.1", modl = "ByteCodeInstr", name = "BreakInfo"}))
-- ,(0x00007ff2b4e9ac58
-- ,(Nothing,MutArrClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = MUT_ARR_PTRS_FROZEN, srtlen = 0}, mccPtrs = 3, mccSize = 4, mccPayload = [0x00007ff2b54af338,0x00007ff2b531aa30/1,0x00007ff2b4e9ac90]}))
-- ,(0x00007ff2b54af338
-- ,(Nothing,ArrWordsClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = ARR_WORDS, srtlen = 0}, bytes = 368, arrWords = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]}))
-- ,(0x00007ff2b531aa30/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 3, nptrs = 1, tipe = CONSTR, srtlen = 0}, ptrArgs = [0x00007ff2b532f3d8/1,0x00007ff2b53188b8,0x00007ff2b53188e0], dataArgs = [1], pkg = "ghc-7.4.1", modl = "ByteCodeInstr", name = "BreakInfo"}))
-- ,(0x00007ff2b532f3d8/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 2, nptrs = 0, tipe = CONSTR_2_0, srtlen = 0}, ptrArgs = [0x00007ff2b532fc20/1,0x00007ff2b532fbe8/1], dataArgs = [], pkg = "ghc-7.4.1", modl = "Module", name = "Module"}))
-- ,(0x00007ff2b532fc20/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 2, nptrs = 4, tipe = CONSTR, srtlen = 0}, ptrArgs = [0x00007ff2b5fd0ae8/2,0x00007ff2b5fd0b00/2], dataArgs = [194,4,4,140680410297392], pkg = "ghc-7.4.1", modl = "FastString", name = "FastString"}))
-- ,(0x00007ff2b5fd0ae8/2
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 2, nptrs = 0, tipe = CONSTR_2_0, srtlen = 1}, ptrArgs = [0x00007ff2b5e1f420,0x00007ff2b5fce760/1], dataArgs = [], pkg = "base", modl = "GHC.ForeignPtr", name = "MallocPtr"}))
-- ,(0x00007ff2b5e1f420
-- ,(Nothing,ArrWordsClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = ARR_WORDS, srtlen = 0}, bytes = 4, arrWords = [140679211213165]}))
-- ,(0x00007ff2b5fce760/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 1, nptrs = 0, tipe = CONSTR_1_0, srtlen = 0}, ptrArgs = [0x00007ff2b5fcdf88], dataArgs = [], pkg = "base", modl = "GHC.STRef", name = "STRef"}))
-- ,(0x00007ff2b5fd0b00/2
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 1, nptrs = 0, tipe = CONSTR_1_0, srtlen = 1}, ptrArgs = [0x00007ff2b5fce770], dataArgs = [], pkg = "ghc-7.4.1", modl = "FastString", name = "UTF8Encoded"}))
-- ,(0x00007ff2b5fce770
-- ,(Nothing,MutVarClosure {info = StgInfoTable {ptrs = 1, nptrs = 0, tipe = MUT_VAR_CLEAN, srtlen = 0}, var = 0x000000000293a3f0/1}))
-- ,(0x00007ff2b532fbe8/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 2, nptrs = 4, tipe = CONSTR, srtlen = 0}, ptrArgs = [0x00007ff2b5fd0ac0/2,0x00007ff2b5fd0ad8/2], dataArgs = [1872,4,4,140680424534272], pkg = "ghc-7.4.1", modl = "FastString", name = "FastString"}))
-- ,(0x00007ff2b5fd0ac0/2
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 2, nptrs = 0, tipe = CONSTR_2_0, srtlen = 1}, ptrArgs = [0x00007ff2b6bb30f0,0x00007ff2b5fce740/1], dataArgs = [], pkg = "base", modl = "GHC.ForeignPtr", name = "MallocPtr"}))
-- ,(0x00007ff2b6bb30f0
-- ,(Nothing,ArrWordsClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = ARR_WORDS, srtlen = 0}, bytes = 4, arrWords = [140679211213133]}))
-- ,(0x00007ff2b5fce740/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 1, nptrs = 0, tipe = CONSTR_1_0, srtlen = 0}, ptrArgs = [0x00007ff2b5fcdf78], dataArgs = [], pkg = "base", modl = "GHC.STRef", name = "STRef"}))
-- ,(0x00007ff2b5fd0ad8/2
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 1, nptrs = 0, tipe = CONSTR_1_0, srtlen = 1}, ptrArgs = [0x00007ff2b5fce750], dataArgs = [], pkg = "ghc-7.4.1", modl = "FastString", name = "UTF8Encoded"}))
-- ,(0x00007ff2b5fce750
-- ,(Nothing,MutVarClosure {info = StgInfoTable {ptrs = 1, nptrs = 0, tipe = MUT_VAR_CLEAN, srtlen = 0}, var = 0x000000000293a3f0/1}))
-- ,(0x00007ff2b60e8178/3
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 5, nptrs = 1, tipe = CONSTR, srtlen = 2}, ptrArgs = [0x00007ff2b60e8c60/1,0x00007ff2b60e8c88/1,0x0000000002720280/2,0x0000000002718120/1,0x00007ff2b60e8c98/1], dataArgs = [1929382912], pkg = "ghc-7.4.1", modl = "Var", name = "Id"}))
-- ,(0x00007ff2b53188b8
-- ,(Nothing,ThunkClosure {info = StgInfoTable {ptrs = 3, nptrs = 0, tipe = THUNK, srtlen = 0}, ptrArgs = [0x00007ff2b5317b30/1,0x00007ff2b5317b40/2,0x00007ff2b5317b70], dataArgs = []}))
-- ,(0x00007ff2b5317b30/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 0, nptrs = 1, tipe = CONSTR_0_1, srtlen = 0}, ptrArgs = [], dataArgs = [2], pkg = "base", modl = "GHC.Word", name = "W16#"}))
-- ,(0x00007ff2b5317b40/2
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 4, nptrs = 1, tipe = CONSTR, srtlen = 1}, ptrArgs = [0x00007ff2b60e8178/3,0x00007ff2b60e81b0/1,0x00007ff2b60e81c0/2,0x0000000002930608/1], dataArgs = [2], pkg = "containers-0.4.2.1", modl = "Data.Map", name = "Bin"}))
-- ,(0x00007ff2b60e8178/3
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 5, nptrs = 1, tipe = CONSTR, srtlen = 2}, ptrArgs = [0x00007ff2b60e8c60/1,0x00007ff2b60e8c88/1,0x0000000002720280/2,0x0000000002718120/1,0x00007ff2b60e8c98/1], dataArgs = [1929382912], pkg = "ghc-7.4.1", modl = "Var", name = "Id"}))
-- ,(0x00007ff2b60e8c60/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 3, nptrs = 1, tipe = CONSTR, srtlen = 0}, ptrArgs = [0x000000000271a6f0/3,0x00007ff2b60e7c88/1,0x00007ff2b5fceab8/2], dataArgs = [1929382912], pkg = "ghc-7.4.1", modl = "Name", name = "Name"}))
-- ,(0x00007ff2b60e8c88/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 1, nptrs = 0, tipe = CONSTR_1_0, srtlen = 0}, ptrArgs = [0x00007ff2b60e8250/2], dataArgs = [], pkg = "ghc-7.4.1", modl = "TypeRep", name = "TyVarTy"}))
-- ,(0x0000000002720280/2
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 1, nptrs = 0, tipe = CONSTR_STATIC, srtlen = 1}, ptrArgs = [0x0000000002720df8/1], dataArgs = [], pkg = "ghc-7.4.1", modl = "Var", name = "LocalId"}))
-- ,(0x0000000002718120/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = CONSTR_NOCAF_STATIC, srtlen = 0}, ptrArgs = [], dataArgs = [], pkg = "ghc-7.4.1", modl = "IdInfo", name = "VanillaId"}))
-- ,(0x00007ff2b60e8c98/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 9, nptrs = 0, tipe = CONSTR, srtlen = 0}, ptrArgs = [0x0000000002717a20,0x0000000002717998,0x0000000002728a58/1,0x0000000002718180/1,0x0000000002718170/1,0x0000000002711708,0x00000000027162f0/1,0x000000000293a3f0/1,0x000000000293a3f0/1], dataArgs = [], pkg = "ghc-7.4.1", modl = "IdInfo", name = "IdInfo"}))
-- ,(0x00007ff2b60e8d90/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 2, nptrs = 0, tipe = CONSTR_2_0, srtlen = 5}, ptrArgs = [0x00007ff2b60e7df8/1,0x00007ff2b60e7de0/1], dataArgs = [], pkg = "ghc-7.4.1", modl = "CoreSyn", name = "AnnLet"}))
-- ,(0x00007ff2b531aa30/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 3, nptrs = 1, tipe = CONSTR, srtlen = 0}, ptrArgs = [0x00007ff2b532f3d8/1,0x00007ff2b53188b8,0x00007ff2b53188e0], dataArgs = [1], pkg = "ghc-7.4.1", modl = "ByteCodeInstr", name = "BreakInfo"}))
-- ,(0x00007ff2b4e9ac90
-- ,(Nothing,BCOClosure {info = StgInfoTable {ptrs = 4, nptrs = 0, tipe = BCO, srtlen = 0}, instrs = 0x00007ff2b531aa70, literals = 0x00007ff2b4e9acc8, bcoptrs = 0x00007ff2b4e9acd8, arity = 2, size = 7, bitmap = 2}))
-- ,(0x00007ff2b531aa70
-- ,(Nothing,ArrWordsClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = ARR_WORDS, srtlen = 0}, bytes = 56, arrWords = [7881299347898368,281474976710710,562958543421467,563078802571269,4295229441,844446406344706,12947861813919770]}))
-- ,(0x00007ff2b4e9acc8
-- ,(Nothing,ArrWordsClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = ARR_WORDS, srtlen = 0}, bytes = 0, arrWords = []}))
-- ,(0x00007ff2b4e9acd8
-- ,(Nothing,MutArrClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = MUT_ARR_PTRS_FROZEN, srtlen = 0}, mccPtrs = 4, mccSize = 5, mccPayload = [0x00007ff2b54af338,0x00007ff2b60e7ee0/1,0x00007ff2b4e9ad18,0x0000000041710c50]}))
-- ,(0x00007ff2b54af338
-- ,(Nothing,ArrWordsClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = ARR_WORDS, srtlen = 0}, bytes = 368, arrWords = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]}))
-- ,(0x00007ff2b60e7ee0/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 3, nptrs = 1, tipe = CONSTR, srtlen = 0}, ptrArgs = [0x00007ff2b532f3d8/1,0x00007ff2b60e06f0,0x00007ff2b60e0718], dataArgs = [0], pkg = "ghc-7.4.1", modl = "ByteCodeInstr", name = "BreakInfo"}))
-- ,(0x00007ff2b532f3d8/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 2, nptrs = 0, tipe = CONSTR_2_0, srtlen = 0}, ptrArgs = [0x00007ff2b532fc20/1,0x00007ff2b532fbe8/1], dataArgs = [], pkg = "ghc-7.4.1", modl = "Module", name = "Module"}))
-- ,(0x00007ff2b532fc20/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 2, nptrs = 4, tipe = CONSTR, srtlen = 0}, ptrArgs = [0x00007ff2b5fd0ae8/2,0x00007ff2b5fd0b00/2], dataArgs = [194,4,4,140680410297392], pkg = "ghc-7.4.1", modl = "FastString", name = "FastString"}))
-- ,(0x00007ff2b532fbe8/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 2, nptrs = 4, tipe = CONSTR, srtlen = 0}, ptrArgs = [0x00007ff2b5fd0ac0/2,0x00007ff2b5fd0ad8/2], dataArgs = [1872,4,4,140680424534272], pkg = "ghc-7.4.1", modl = "FastString", name = "FastString"}))
-- ,(0x00007ff2b60e06f0
-- ,(Nothing,ThunkClosure {info = StgInfoTable {ptrs = 3, nptrs = 0, tipe = THUNK, srtlen = 0}, ptrArgs = [0x00007ff2b60df0e8/1,0x00007ff2b60df0f8/2,0x00007ff2b60e8dd0/2], dataArgs = []}))
-- ,(0x00007ff2b60df0e8/1
-- ,(Nothing,ConsClosure {info = StgInfoTable {ptrs = 0, nptrs = 1, tipe = CONSTR_0_1, srtlen = 0}, ptrArgs = [], dataArgs = [2], pkg = "base", modl = "GHC.Word", name = "W16#"}))]
