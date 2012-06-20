{-# LANGUAGE TemplateHaskell #-}
module Pr where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

pr :: Name -> ExpQ
pr n = [| putStrLn ( $(lift (nameBase n ++ " = ")) ++ show $(varE n) ) |]

-- let x = 3
-- $(varE (mkName "x"))

--a = $(\x -> varE (mkName x))

eval = \x -> "GHC.Vis.evalP " ++ head (words x) ++ " \"" ++ last (words x) ++ "\""
