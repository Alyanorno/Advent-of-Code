{-# LANGUAGE TemplateHaskell #-}

module Template where

import Language.Haskell.TH
import Control.Monad (liftM)

generateDay :: [Int] -> Q [Dec]
generateDay ds = liftM (flip (:) []) $ funD name clauses
	where
	name = mkName "day"
	names = concat $ map ((\a -> [a, a ++ "_2"]).(++) "day".show) ds
	patterns = map (return . LitP . StringL) names
	bodies = map f names
	f :: String -> ExpQ
	f n
		| n == "day7" = [| putStrLn . id . $(n') |]
		| n == "day7_2" = [| putStrLn . id . $(n') |]
--		| n == "day18" = [| putStrLn . id . $(n') |]
		| otherwise = [| print . $(n') |]
		where n' = varE . mkName $ n
	clauses = zipWith (\body pattern -> clause [pattern] (normalB body) []) bodies patterns

