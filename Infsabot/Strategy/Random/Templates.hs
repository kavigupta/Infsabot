{-# LANGUAGE FlexibleInstances #-}
module Infsabot.Strategy.Random.Templates(
        crDecls, isExpr
    ) where

import Language.Haskell.TH
import Data.List
import Data.Function
import Control.Monad.State.Lazy
import Control.Applicative((<$>))

import Control.Arrow(second)
import Infsabot.Strategy.ExprTree.Interface()

{-
    Takes a name of a constructor and outputs a list of constructors,
        in the form of a tuple of the name of the constructor and the
        types it takes as arguments.
-}
constructorsFor :: Name -> Q [(Name, [Type])]
constructorsFor name = do
        info <- reify name
        let declarations = declarationsFor info
        return $ map (second (map snd) . constructor) declarations
    where
    constructor (NormalC cons types) = (cons, types)
    constructor _ = error "Non-basic constructor syntax not supported"
    -- Gets the declarations for the given reified name
    declarationsFor :: Info -> [Con]
    declarationsFor (TyConI (DataD _ _ _ declarations _)) = declarations
    declarationsFor u = error . ("Error here: " ++) . show $ u

{-
    Generates ComplexityRandom declarations for the given name.
-}
crDecls :: Name -> Q [Dec]
crDecls name = (: []) <$> instanceD (cxt []) inst clauses
    where
    inst = return $ AppT (ConT $ mkName "ComplexityRandom") (ConT name)
    clauses = map ($ name) [generateCRandom, generateComplexity]

{-
    Fills in the complexity function for the ComplexityRandom
        class.
-}
generateComplexity :: Name -> Q Dec
generateComplexity name = do
    constructors <- constructorsFor name
    let matches = map complex constructors
    let body = NormalB $ CaseE (sym "tree") matches
    return . FunD (mkName "complexity") $ [Clause [VarP $ mkName "tree"] body []]

complex :: (Name, [Type]) -> Match
complex (constructor, types) = Match (ConP constructor patterns) (NormalB body) []
        where
        {-
            The names of the local variables to this function. They represent
                the children of the given expression tree
        -}
        originalNames = map (mkName . ("x" ++) . show) [1..length types]
        patterns = zipWith getUnder originalNames exps
        body = foldr (+++) (LitE $ IntegerL 1) exps
        exps = zipWith getComplexity (map fromType types) originalNames
        (x, _) +++ y = InfixE (Just x) (VarE $ mkName "+") (Just y)
        getUnder :: Name -> (a, Bool) -> Pat
        getUnder name (_, use) = VarP $ if use then name else mkName "_"

generateCRandom :: Name -> Q Dec
generateCRandom name = do
        constructors <- constructorsFor name
        minCR <- mapM getMinCRand constructors
        let lessThan = InfixE Nothing (sym "<=") (Just $ infi (sym "currentComplexity") (sym "-") (LitE $ IntegerL 1))
        let filteredList = sym "filter" $$ infi lessThan (sym ".") (sym "snd") $$ ListE minCR
        let appropriateConstructors = sym "mapM" $$ sym "fst" $$ filteredList
        let bindConstructors = BindS (VarP . mkName $ "cons") appropriateConstructors
        let retVal = sym "state" $$ (sym "choice" $$ sym "cons")
        let doexpr = sym "runState" $$ DoE [bindConstructors, NoBindS retVal]
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

infixl 0 $$
($$) :: Exp -> Exp -> Exp
($$) = AppE

infi :: Exp -> Exp -> Exp -> Exp
infi x op y = InfixE (Just x) op (Just y)

getCRandForSingleConstructor :: (Name, [Type]) -> Exp
getCRandForSingleConstructor (name, types)= DoE $ [part] ++ assignments ++ [construct]
    where
    vars = map (("n"++) . show) [1..numberOfNs]
    part = BindS (ListP $ map (VarP . mkName) vars)
        $ AppE (sym "state") $ AppE (AppE (sym "getPartition") (infi (sym "currentComplexity") (sym "-") (LitE $ IntegerL 1)))
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

-- Gets the name of the given type
fromType :: Type -> Name
fromType (ConT typ) = typ
fromType _ = error "Non-basic constructor syntax not supported"

-- Whether or not a name is an expression type or a primitive.
isExpr :: Name -> Bool
isExpr name = "Expr" `isPrefixOf` nameBase name  || "RP" == nameBase name
