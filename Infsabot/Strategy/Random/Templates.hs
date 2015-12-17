{-# LANGUAGE FlexibleInstances #-}
module Infsabot.Strategy.Random.Templates(
        crDecls, isExpr
    ) where

import Language.Haskell.TH
import Data.List
import Data.Function
import Control.Monad.State.Lazy

import Control.Arrow(second)
import Infsabot.Strategy.ExprTree.Interface()

constructorsFor :: Name -> Q [(Name, [Type])]
constructorsFor name = do
        info <- reify name
        let declarations = declarationsFor info
        return $ map (second (map snd) . constructor) declarations
    where
    constructor (NormalC cons types) = (cons, types)
    constructor _ = error "Non-basic constructor syntax not supported"

declarationsFor :: Info -> [Con]
declarationsFor (TyConI (DataD _ _ _ declarations _)) = declarations
declarationsFor u = error . ("Error here: " ++) . show $ u

crDecls :: Name -> Q [Dec]
crDecls name = liftM (: []) $ instanceD (cxt []) inst clauses
    where
    inst = return $ AppT (ConT $ mkName "ComplexityRandom") (ConT name)
    clauses = map ($ name) [cRand, allComplex]

allComplex :: Name -> Q Dec
allComplex name = do
    constructors <- constructorsFor name
    let matches = map complex constructors
    let body = NormalB $ CaseE (sym "tree") matches
    return . FunD (mkName "complexity") $ [Clause [VarP $ mkName "tree"] body []]

complex :: (Name, [Type]) -> Match
complex (constructor, types) = Match (ConP constructor patterns) (NormalB body) []
        where
        names = zipWith variableName decls [1..length types]
        patterns = map VarP names
        decls :: [(Exp, Bool)]
        decls = zipWith getComplexity (map fromType types) names
        body = foldr (+++) (LitE $ IntegerL 1) . filter snd $ decls
        (x, _) +++ y = InfixE (Just x) (VarE $ mkName "+") (Just y)
        variableName :: (a, Bool) -> Int -> Name
        variableName (_, real) n = mkName varN
            where varN = if real then "x" ++ show n else "_"

cRand :: Name -> Q Dec
cRand name = do
        constructors <- constructorsFor name
        minCR <- mapM getMinCRand constructors
        let lessThan = InfixE Nothing (sym "<=") (Just $ sym "currentComplexity")
        let filtList = AppE (sym "filter") (InfixE (Just lessThan) (sym ".") (Just $ sym "snd"))
        let unfilteredList = ListE minCR
        let filteredList = AppE filtList unfilteredList
        let appropriateConstructors = AppE (AppE (sym "mapM") (sym "fst")) filteredList
        let bindConstructors = BindS (VarP . mkName $ "cons") appropriateConstructors
        let retVal = AppE (sym "state") $ AppE (sym "choice") (sym "cons")
        let doexpr = AppE (sym "runState") $ DoE [bindConstructors, NoBindS retVal]
        return $ FunD (mkName "cRandom") . return $ Clause [VarP $ mkName "currentComplexity"] (NormalB doexpr) []

sym :: String -> Exp
sym = VarE . mkName

getMinCRand :: (Name, [Type]) -> Q Exp -- minC, rand
getMinCRand (n, ts) = do
    let cRandForThis = getCRandForSingleConstructor (n, ts)
    let minCExpression
            = LitE . IntegerL . fromIntegral .
                length .  filter (isExpr . fromType) $ ts
    return . TupE $ [cRandForThis, minCExpression]



getCRandForSingleConstructor :: (Name, [Type]) -> Exp
getCRandForSingleConstructor (name, types)= DoE $ [part] ++ assignments ++ [construct]
    where
    vars = map (("n"++) . show) [1..numberOfNs]
    part = BindS (ListP $ map (VarP . mkName) vars)
        $ AppE (sym "state") $ AppE (AppE (sym "getPartition") (sym "currentComplexity"))
        $ LitE . IntegerL . fromIntegral $ numberOfNs
    (assignments, (numberOfNs, numberOfUs)) = runState (mapM (state . getRandom) types) (0, 0)
    construct = NoBindS $ AppE (sym "return")
        $ foldl AppE (ConE name) $ map (VarE . mkName . ("u"++) . show) [1..numberOfUs]

type ConsumedCount = (Int, Int)

getRandom :: Type -> ConsumedCount -> (Stmt, ConsumedCount)
getRandom typ (n, m) = (BindS (VarP $ mkName $ "u" ++ show (n + 1)) $ AppE (sym "state") rhs, new)
    where
    usecrandom = isExpr (fromType typ)
    new
        | usecrandom = (n + 1, m + 1)
        | otherwise = (n, m + 1)
    rhs
        | usecrandom
            = (AppE `on` VarE) (mkName "cRandom") $ mkName $ "n" ++ show (n + 1)
        | otherwise = sym "random"

getComplexity :: Name -> Name -> (Exp, Bool)
getComplexity typ nam
    | isExpr typ               = (AppE (sym "complexity") (VarE nam), True)
    | otherwise                = (LitE $ IntegerL 0, False)

fromType :: Type -> Name
fromType (ConT typ) = typ
fromType _ = error "Non-basic constructor syntax not supported"

isExpr :: Name -> Bool
isExpr name = "Expr" `isPrefixOf` nameBase name  || "RP" == nameBase name
