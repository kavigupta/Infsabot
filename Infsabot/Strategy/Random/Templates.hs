{-# LANGUAGE FlexibleInstances #-}
module Infsabot.Strategy.Random.Templates(
        crDecls
    ) where

import Language.Haskell.TH
import Data.List
import Data.Function

import Control.Applicative
import Control.Arrow(second)
import Infsabot.Strategy.ExprTree.Interface()

import Data.Monoid

symComplexity, symComplexityRandom,
    symRand, symState, symGetPart, symCount,
    symReturn :: Name
symComplexity = mkName "complexity"
symComplexityRandom = mkName "ComplexityRandom"
symRand = mkName "random"
symState = mkName "state"
symReturn = mkName "return"
symGetPart = mkName "getPartition"
symCount = mkName "count"
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

minComplexity :: Name -> Q Nat
minComplexity name = do
        constructors <- constructorsFor name
        let consByLeaves = sortBy (compare `on` getLeaves) constructors
        subs <- mapM getSubComplex consByLeaves
        return $ minimum subs

data Nat = Z | S Nat deriving Show

instance Eq Nat where
    Z == Z          = True
    Z == (S _)      = False
    (S _) == Z      = False
    (S n) == (S m)  = n == m

instance Ord Nat where
    Z `compare` Z           = EQ
    Z `compare` (S _)       = LT
    (S _) `compare` Z       = GT
    (S a) `compare` (S b)   = a `compare` b

instance Monoid Nat where
    mempty              = Z
    Z `mappend` x       = x
    x `mappend` Z       = x
    (S x) `mappend` y   = S $ x `mappend` y

toNumeric :: (Num a) => Nat -> a
toNumeric Z = 0
toNumeric (S n) = 1 + toNumeric n

getSubComplex :: (Name, [Type]) -> Q Nat
getSubComplex (_, types) = do
        mins <- mapM (minComplexity . fromType) types
        return $ S Z `mappend` mconcat mins
getLeaves :: (Name, [Type]) -> Int
getLeaves = length . filter (isExpr . fromType) . snd

crDecls :: Name -> Q Dec
crDecls name = do
        constructors <- constructorsFor name
        instanceD (cxt []) (return $ AppT (ConT symComplexityRandom) (ConT name))
            $ cRand name : map complex constructors

complex :: (Name, [Type]) -> Q Dec
complex (constructor, types) = (FunD symComplexity . return) <$> getClause
    where
    getClause = clause [return $ ConP constructor patterns] (return $ NormalB body) []
        where
        names = map (mkName . ("x" ++) . show) [1..length types]
        patterns = map VarP names
        body = foldr (+++) (LitE $ IntegerL 1) exps
        exps = zipWith getComplexity (map fromType types) names
        x +++ y = InfixE (Just x) (VarE $ mkName "+") (Just y)

cRand :: Name -> Q Dec
cRand name = do
        constructors <- constructorsFor name
        minCR <- mapM getMinCRand constructors
        let lessThan = InfixE Nothing (sym "<=") (Just $ sym "currentComplexity")
        let filtList = AppE (sym "filter") (InfixE (Just lessThan) (sym ".") (Just $ sym "fst"))
        let unfilteredList = ListE minCR
        let filteredList = AppE filtList unfilteredList
        let appropriateConstructors = AppE (AppE (sym "mapM") (sym "snd")) filteredList
        let bindConstructors = BindS (VarP . mkName $ "cons") appropriateConstructors
        let retVal = AppE (sym "choice") (sym "cons")
        let doexpr = DoE [bindConstructors, NoBindS retVal]
        return $ FunD (mkName "cRand") . return $ Clause [VarP $ mkName "currentComplexity"] (NormalB doexpr) []

sym :: String -> Exp
sym = VarE . mkName

getMinCRand :: (Name, [Type]) -> Q Exp -- minC, rand
getMinCRand (n, ts) = do
    minC <- minComplexity n
    let cRandForThis = getCRandForSingleConstructor (n, ts)
    let minCExpression = LitE . IntegerL . toNumeric $ minC
    return . TupE $ [cRandForThis, minCExpression]

getCRandForSingleConstructor :: (Name, [Type]) -> Exp
getCRandForSingleConstructor (name, types)= DoE $ [part] ++ assignments ++ [construct]
    where
    ntypes = length types
    vars = map (("n"++) . show) [1..ntypes]
    part = BindS (ListP $ map (VarP . mkName) vars) $ AppE (VarE symState) (AppE (VarE symGetPart) (VarE symCount))
    assignments = zipWith (getRandom ntypes) types [1..ntypes]
    construct = NoBindS $ foldl AppE (VarE symReturn) $ VarE name : map (VarE . mkName . ("u"++) . show) [1..ntypes]

getRandom :: Int -> Type -> Int -> Stmt
getRandom ma typ n = BindS (VarP $ mkName $ "u" ++ show n) $ AppE (VarE symState) rhs
    where
    rhs
        | isExpr (fromType typ) = VarE symRand
        | otherwise
            = AppE ((AppE `on` VarE) symGetPart $ mkName $ "n" ++ show n) (LitE $ IntegerL $ fromIntegral ma)

getComplexity :: Name -> Name -> Exp
getComplexity typ nam
    | isExpr typ               = AppE (VarE symComplexity) (VarE nam)
    | otherwise                = LitE $ IntegerL 1

fromType :: Type -> Name
fromType (ConT typ) = typ
fromType _ = error "Non-basic constructor syntax not supported"

isExpr :: Name -> Bool
isExpr name = "Expr" `isPrefixOf` nameBase name  || "RP" == nameBase name
