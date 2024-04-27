{-# LANGUAGE LambdaCase #-}
module Plume.TypeChecker.Checker.Switch where

import Data.Map qualified as Map
import Plume.Syntax.Abstract qualified as Pre
import Plume.Syntax.Common.Literal
import Plume.Syntax.Common.Pattern qualified as Pre
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.TLIR qualified as Post
import Plume.TypeChecker.Constraints.Solver (unifiesWith)
import Plume.TypeChecker.Checker.Datatype (datatypes, tA)
import Plume.TypeChecker.Constraints.Unification (doesUnifyWith)
import Data.List (nub)
import System.IO.Pretty (printWarningFromString)
import Plume.TypeChecker.TLIR.Internal.Pretty (prettyToString, prettyPat)

synthSwitch :: Infer -> Infer
synthSwitch infer (Pre.ESwitch scrutinee cases) = local id $ do
  -- Infer the scrutinee and the cases
  (t, scrutinee') <- extractFromArray $ infer scrutinee
  (tys, cases') <- mapAndUnzipM (synthCase infer t) cases
  let (ts', expr) = unzip tys  

  (ret, xs) <- case ts' of
    [] -> throw EmptyMatch
    (ret : xs) -> return (ret, xs)
  
  (exprTy, xs') <- case expr of
    [] -> return (ret, [])
    (x : xs'') -> return (x, xs'')

  -- Unify the return type with the type of the case expressions
  forM_ xs $ unifiesWith ret

  -- Unify the scrutinee type with the type of the patterns
  forM_ xs' $ unifiesWith exprTy

  patternSp <- case cases' of
    [] -> throw EmptyMatch
    [(pat, _)] -> project pat
    _ -> foldCustom (\a b -> UnionSpace [a, b]) <$> mapM (project . fst) cases'
  uncovered <- simplify =<< (TypeSpace t `subtractSpaceWith` patternSp)
  
  if uncovered == EmptySpace then pure ()
  else do
    let msg = case uncovered of
          TypeSpace ty -> "Consider adding a default case for the type " <> show ty
          _ -> "Consider adding missing cases for " <> showSpace uncovered
    throw (ExhaustivenessError msg)

  -- TODO: Implement a check for redundancy that would work
  -- checkRedudancy t (map fst cases')

  pure (exprTy, [Post.ESwitch scrutinee' cases'])
synthSwitch _ _ = throw $ CompilerError "Only switches are supported"

allPatsExcept :: [Post.Pattern] -> Post.Pattern -> [Post.Pattern]
allPatsExcept xs x = filter (/= x) xs

checkRedudancy :: PlumeType -> [Post.Pattern] -> Checker ()
checkRedudancy _ [] = pure ()
checkRedudancy _ xs = do
  void $ foldlM (\acc p -> do
    pat <- project p
    whenM (pat `isRedundantIn` acc) $ do
      pos <- fetchPosition
      let pat' = prettyToString (prettyPat p)
      liftIO $ printWarningFromString
        mempty
        ( "Pattern " <> pat' <> " is redundant"
        , Nothing
        , pos
        )
        "while performing typechecking"
    (:[]) <$> simplify (UnionSpace (acc <> [pat]))) [] xs

isRedundantIn :: Space -> [Space] -> Checker Bool
isRedundantIn (VariablePoint _ _) (VariablePoint _ _:_) = pure True
isRedundantIn sp@(VariablePoint x _) (ConstructorSpace _ _ _ : ys)
  | x /= "?" = sp `isRedundantIn` ys
  | otherwise = pure True
isRedundantIn sp@(VariablePoint x _) (TypeSpace _ : ys)
  | x /= "?" = sp `isRedundantIn` ys
  | otherwise = pure True
isRedundantIn sp@(VariablePoint _ _) (ConstantPoint _ _ : ys) = sp `isRedundantIn` ys
isRedundantIn sp@(ConstantPoint c _) (ConstantPoint c' _ : ys)
  | c == c' = pure True
  | otherwise = sp `isRedundantIn` ys
isRedundantIn (ConstantPoint _ _) (VariablePoint _ _ : _) = pure True
isRedundantIn (ConstructorSpace k _ ss) (ConstructorSpace k' _ ss' : ys)
  | k == k' = do
    b <- anyM (\(x, y) -> x `isRedundantIn` [y]) (zip ss ss')
    if b then pure True
    else isRedundantIn (UnionSpace ss) ys
  | otherwise = isRedundantIn (UnionSpace ss) ys
isRedundantIn (ConstructorSpace _ _ _) (VariablePoint _ _ : _) = pure True
isRedundantIn sp@(ConstructorSpace _ _ _) (TypeSpace _ : ys) = sp `isRedundantIn` ys
isRedundantIn sp@(ConstructorSpace _ _ _) (ConstantPoint _ _ : ys) = sp `isRedundantIn` ys
isRedundantIn (TypeSpace _) (TypeSpace _ : _) = pure True
isRedundantIn (TypeSpace _) (VariablePoint "?" _ : _) = pure True
isRedundantIn (TypeSpace _) (VariablePoint _ _ : _) = pure True
isRedundantIn sp@(TypeSpace _) (ConstantPoint _ _ : ys) = sp `isRedundantIn` ys
isRedundantIn (UnionSpace ss) ys = do
  b <- anyM (flip isRedundantIn ys) ss
  if b then pure True
  else pure False
isRedundantIn sp (_:ys) = sp `isRedundantIn` ys
isRedundantIn _ [] = pure False

synthCase
  :: Infer
  -> PlumeType
  -> (Pre.Pattern, Pre.Expression)
  -> Checker ((PlumeType, PlumeType), (Post.Pattern, Post.Expression))
synthCase infer scrutTy (pat, expr) = local id $ do
  -- Synthesize the pattern and infer the expression
  (patTy, patExpr, patEnv) <- synthPattern pat
  (exprTy, expr') <- local id . extractFromArray $ localEnv patEnv (infer expr)

  -- Pattern type should unify with the scrutinee type
  scrutTy `unifiesWith` patTy
  pure ((patTy, exprTy), (patExpr, expr'))

-- | Locally perform an action without changing the environment globally
localEnv :: Map Text PlumeScheme -> Checker a -> Checker a
localEnv env action = do
  vars <- gets (typeEnv . environment)
  insertEnvWith @"typeEnv" (<>) env
  res <- action
  replaceEnv @"typeEnv" vars
  pure res

-- | Synthesizing a pattern consists of inferring the type of the pattern
-- | like regular expressions, but also returning the environment created 
-- | by the pattern (e.g. variables in the pattern).
synthPattern
  :: Pre.Pattern
  -> Checker (PlumeType, Post.Pattern, Map Text PlumeScheme)
synthPattern Pre.PWildcard = do
  t <- fresh
  pure (t, Post.PWildcard t, mempty)
synthPattern (Pre.PVariable name) = do
  t <- searchEnv @"datatypeEnv" name
  case t of
    Just t' -> do
      inst <- instantiate t'
      return (inst, Post.PSpecialVar name inst, mempty)
    Nothing -> do
      ty <- fresh
      return
        ( ty
        , Post.PVariable name ty
        , Map.singleton name ty
        )
synthPattern (Pre.PLiteral l) = do
  let (ty, l') = typeOfLiteral l
  pure (ty, Post.PLiteral l', mempty)
synthPattern (Pre.PConstructor name pats) = do
  t <- searchEnv @"datatypeEnv" name
  case t of
    Just t' -> do
      inst <- instantiate t'
      ret <- fresh
      (patsTy, pats', env) <- mapAndUnzip3M synthPattern pats
      inst `unifiesWith` (patsTy :->: ret)
      return (ret, Post.PConstructor name inst pats', mconcat env)
    Nothing -> throw $ UnboundVariable name
synthPattern (Pre.PList pats slice) = do
  tv <- fresh
  (patsTy, pats', env) <- mapAndUnzip3M synthPattern pats
  forM_ patsTy (`unifiesWith` tv)

  slRes <- maybeM slice synthPattern

  case slRes of
    Just (slTy, sl', slEnv) -> do
      slTy `unifiesWith` TList tv
      return (TList tv, Post.PList tv pats' (Just sl'), mconcat env <> slEnv)
    Nothing -> return (TList tv, Post.PList tv pats' Nothing, mconcat env)
synthPattern (Pre.PSlice n) = do
  tv <- fresh
  return
    ( TList tv
    , Post.PVariable n (TList tv)
    , Map.singleton n (TList tv)
    )

typeOfLiteral :: Literal -> (PlumeType, Literal)
typeOfLiteral (LInt i) = (TInt, LInt i)
typeOfLiteral (LFloat f) = (TFloat, LFloat f)
typeOfLiteral (LBool b) = (TBool, LBool b)
typeOfLiteral (LString s) = (TString, LString s)
typeOfLiteral (LChar c) = (TChar, LChar c)

-- | Function that maps monadic actions over a list and then unzips the result
-- | into three separate lists.
mapAndUnzip3M :: (Monad m) => (a -> m (b, c, d)) -> [a] -> m ([b], [c], [d])
mapAndUnzip3M f xs = do
  (bs, cs, ds) <- unzip3 <$> mapM f xs
  pure (bs, cs, ds)

data Space
  = EmptySpace
  | TypeSpace PlumeType
  | UnionSpace [Space]
  | ConstructorSpace Text [PlumeType] [Space]
  | VariablePoint Text PlumeType
  | ConstantPoint Constant PlumeType
  deriving (Eq, Show)

data Constant
  = Literal Literal
  | EnumVar Text
  deriving (Eq, Show)


unionWith :: Space -> Space -> Checker Space
unionWith EmptySpace s = pure s
unionWith s EmptySpace = pure s
unionWith (TypeSpace t1) (TypeSpace t2) = do
  b1 <- liftIO $ doesUnifyWith t1 t2
  if b1 then pure (TypeSpace t1)
  else pure (UnionSpace [TypeSpace t1, TypeSpace t2])
unionWith (UnionSpace xs) (UnionSpace ys) = pure (UnionSpace (xs <> ys))
unionWith (UnionSpace xs) y = UnionSpace <$> mapM (`unionWith` y) xs
unionWith x (UnionSpace ys) = UnionSpace <$> mapM (unionWith x) ys
unionWith (ConstructorSpace t1 tys1 xs) (ConstructorSpace t2 tys2 ys) = do
  let b1 = t1 == t2
  let b2 = tys1 == tys2
  if b1 && b2 then do
    zs <- zipWithM unionWith xs ys
    pure (ConstructorSpace t1 tys1 zs)
  else pure (UnionSpace [ConstructorSpace t1 tys1 xs, ConstructorSpace t2 tys2 ys])
unionWith (TypeSpace t) (ConstructorSpace k tys xs) = do
  b1 <- k `isInstanceOf` t
  if b1 then pure (ConstructorSpace k tys xs)
  else pure (UnionSpace [TypeSpace t, ConstructorSpace k tys xs])
unionWith (ConstructorSpace k tys xs) (TypeSpace t) = do
  b1 <- k `isInstanceOf` t
  if b1 then pure (ConstructorSpace k tys xs)
  else pure (UnionSpace [TypeSpace t, ConstructorSpace k tys xs])
unionWith (ConstantPoint c t) (ConstantPoint c' t') = do
  b1 <- liftIO $ doesUnifyWith t t'
  if b1 && c == c' then pure (ConstantPoint c t)
  else pure (UnionSpace [ConstantPoint c t, ConstantPoint c' t'])
unionWith (ConstantPoint c t) (TypeSpace t') = do
  b1 <- liftIO $ doesUnifyWith t t'
  if b1 then pure (ConstantPoint c t)
  else pure (UnionSpace [ConstantPoint c t, TypeSpace t'])
unionWith (TypeSpace t) (ConstantPoint c t') = do
  b1 <- liftIO $ doesUnifyWith t t'
  if b1 then pure (ConstantPoint c t')
  else pure (UnionSpace [TypeSpace t, ConstantPoint c t'])
unionWith (VariablePoint x t) (VariablePoint y t') = do
  b1 <- liftIO $ doesUnifyWith t t'
  if x == y && b1 then pure (VariablePoint x t)
  else pure (UnionSpace [VariablePoint x t, VariablePoint y t'])
unionWith (VariablePoint x t) (TypeSpace t') = do
  b1 <- liftIO $ doesUnifyWith t t'
  if b1 then pure (VariablePoint x t)
  else pure (UnionSpace [VariablePoint x t, TypeSpace t'])
unionWith (TypeSpace t) (VariablePoint x t') = do
  b1 <- liftIO $ doesUnifyWith t t'
  if b1 then pure (VariablePoint x t')
  else pure (UnionSpace [TypeSpace t, VariablePoint x t'])
unionWith (VariablePoint x t) (ConstructorSpace k tys xs) = do
  b1 <- k `isInstanceOf` t
  if b1 then pure (ConstructorSpace k tys xs)
  else pure (UnionSpace [VariablePoint x t, ConstructorSpace k tys xs])
unionWith (ConstructorSpace k tys xs) (VariablePoint x t) = do
  b1 <- k `isInstanceOf` t
  if b1 then pure (ConstructorSpace k tys xs)
  else pure (UnionSpace [VariablePoint x t, ConstructorSpace k tys xs])
unionWith x y = pure (UnionSpace [x, y])

isRedundant :: Space -> [Space] -> Checker Bool
isRedundant x xs = 
  (==EmptySpace) <$> (simplify =<< x `subtractSpaceWith` UnionSpace xs)

getCons :: Space -> Checker PlumeType
getCons (ConstructorSpace name tys _) = do
  m <- readIORef datatypes
  let m' = Map.toList m
  let f = find (\(_, x) -> Map.member name x) m'

  case f of 
    Just (_, x) -> do
      let t = x Map.! name
      inst <- instantiate t
      let args = getArguments inst
      zipWithM_ unifiesWith tys args
      return (getHeader inst)
    Nothing -> throw $ UnboundVariable name
getCons _ = throw $ CompilerError "Invalid space"

simplify :: Space -> Checker Space
simplify (ConstructorSpace t tys xs) = do
  xs' <- mapM simplify xs
  pure $ if EmptySpace `elem` xs' 
    then EmptySpace
    else ConstructorSpace t tys xs'
simplify (UnionSpace xs) = do
  xs' <- mapM simplify xs
  let xs'' = nub $ concatMap (\case UnionSpace ys -> ys; x -> [x]) xs'
  let xs''' = filter (/= EmptySpace) xs''

  pure $ case xs''' of
    [] -> EmptySpace
    [x] -> x
    res -> UnionSpace res
simplify (TypeSpace t) = do
  b1 <- canDecompose t
  b2 <- null <$> decompose t
  pure $ if b1 && b2 then EmptySpace else TypeSpace t
simplify x = pure x 

canDecompose :: PlumeType -> Checker Bool
canDecompose TBool = return True
canDecompose (TypeVar l) = do
  content <- readIORef l
  case content of
    Link t -> canDecompose t
    _ -> return False
canDecompose t@(TypeId name) | isNotPrimitiveType t = do
  m <- readIORef datatypes
  case Map.lookup name m of
    Just _ -> return True
    Nothing -> return False
canDecompose (TypeApp (TypeId name) _) = do
  m <- readIORef datatypes
  case Map.lookup name m of
    Just _ -> return True
    Nothing -> return False
canDecompose _ = return False

true, false :: Constant
true = Literal $ LBool True
false = Literal $ LBool False

isNotPrimitiveType :: PlumeType -> Bool
isNotPrimitiveType TInt = False
isNotPrimitiveType TFloat = False
isNotPrimitiveType TBool = False
isNotPrimitiveType TString = False
isNotPrimitiveType TChar = False
isNotPrimitiveType _ = True

getArguments :: PlumeType -> [PlumeType]
getArguments (args :->: _) = args
getArguments _ = []

decompose :: PlumeType -> Checker [Space]
decompose (TypeVar l) = do
  content <- readIORef l
  case content of
    Link t -> decompose t
    _ -> return []
decompose TBool = return [ConstantPoint false TBool, ConstantPoint true TBool]
decompose t@(TypeId name) | isNotPrimitiveType t = do
  m <- readIORef datatypes
  case Map.lookup name m of
    Just t' -> mapM (\(key, val) -> do
        inst <- instantiate val
        let args = getArguments inst
        return $ ConstructorSpace key args (map TypeSpace args)
      ) (Map.toList t')
    Nothing -> throw $ UnboundVariable name
decompose t@(TypeApp (TypeId name) _) = do
  m <- readIORef datatypes
  case Map.lookup name m of
    Just t' -> mapM (\(key, val) -> do
        inst <- instantiate val
        let args = getArguments inst
        let header = getHeader inst
        header `unifiesWith` t
        return $ ConstructorSpace key args (map TypeSpace args)
      ) (Map.toList t')
    Nothing -> throw $ UnboundVariable name
decompose _ = return []

getHeader :: PlumeType -> PlumeType
getHeader (_ :->: t) = t
getHeader t = t

isSubspaceOf :: Space -> Space -> Checker Bool
isSubspaceOf a b = (==EmptySpace) <$> a `subtractSpaceWith` b

intersectWith :: Space -> Space -> Checker Space
intersectWith a b = case (a, b) of
  (EmptySpace, _) -> return EmptySpace
  (_, EmptySpace) -> return EmptySpace
  (_, UnionSpace ss) -> do
    ss' <- mapM (intersectWith a) ss
    let ss'' = filter (/= EmptySpace) ss'
    return $ UnionSpace ss''
  (UnionSpace ss, _) -> do
    ss' <- mapM (`intersectWith` b) ss
    let ss'' = filter (/= EmptySpace) ss'
    return $ UnionSpace ss''
  (TypeSpace t, TypeSpace t') -> do
    b1 <- liftIO $ doesUnifyWith t t'
    b2 <- canDecompose t
    b3 <- canDecompose t'
    if b1 then pure a
    else if b2 then tryDecomposeA t
    else if b3 then tryDecomposeB t'
    else pure EmptySpace
  (ConstructorSpace t _ _, TypeSpace t') -> do
    b1 <- t `isInstanceOf` t'
    b2 <- canDecompose t'

    if b1 then pure a
    else if b2 then tryDecomposeB t'
    else pure EmptySpace
  (TypeSpace t, ConstructorSpace t' _ _) -> do
    b1 <- t' `isInstanceOf` t
    b2 <- canDecompose t
    if b1 then pure b
    else if b2 then tryDecomposeA t
    else pure EmptySpace
  (ConstructorSpace t _ ss, ConstructorSpace t' tys ss') -> do
    let b1 = t /= t'
    b2 <- anyM (\(x, y) -> (==EmptySpace) <$> (simplify =<< x `intersectWith` y)) (zip ss ss')
    if b1 || b2 then pure EmptySpace
    else do
      ss'' <- zipWithM intersectWith ss ss'
      return $ ConstructorSpace t tys ss''
  (ConstantPoint c _, ConstantPoint c' _) -> return $ if c == c' then a else EmptySpace
  (ConstantPoint _ t, TypeSpace t') -> do
    b1 <- liftIO $ doesUnifyWith t t'
    b2 <- canDecompose t'
    if b1 then pure a
    else if b2 then tryDecomposeB t'
    else pure EmptySpace
  (ConstantPoint _ _, _) -> return EmptySpace
  (TypeSpace t, ConstantPoint _ t') -> do
    b1 <- liftIO $ doesUnifyWith t t'
    b2 <- canDecompose t
    if b1 then pure b
    else if b2 then tryDecomposeA t
    else pure EmptySpace
  (_, ConstantPoint _ _) -> return EmptySpace
  (VariablePoint x _, VariablePoint y _) -> return $ if x == y then a else EmptySpace
  (VariablePoint _ t, TypeSpace t') -> do
    b1 <- liftIO $ doesUnifyWith t t'
    b2 <- canDecompose t'
    if b1 then pure a
    else if b2 then tryDecomposeB t'
    else pure EmptySpace
  (TypeSpace t, VariablePoint _ t') -> do
    b1 <- liftIO $ doesUnifyWith t t'
    b2 <- canDecompose t
    if b1 then pure b
    else if b2 then tryDecomposeA t
    else pure EmptySpace
  (ConstructorSpace k _ _, VariablePoint _ t) -> do
    b1 <- k `isInstanceOf` t
    if b1 then TypeSpace t `intersectWith` a
    else pure EmptySpace
  (VariablePoint _ t, ConstructorSpace k _ _) -> do
    b1 <- k `isInstanceOf` t
    if b1 then TypeSpace t `intersectWith` b
    else pure EmptySpace
  where
    tryDecomposeA :: PlumeType -> Checker Space
    tryDecomposeA t = do
      sp <- decompose t
      let un = UnionSpace sp
      un `intersectWith` b

    tryDecomposeB :: PlumeType -> Checker Space
    tryDecomposeB t = do
      sp <- decompose t
      let un = UnionSpace sp
      a `intersectWith` un

subtractSpaceWith :: Space -> Space -> Checker Space
subtractSpaceWith a b = case (a, b) of
  (EmptySpace, _) -> return EmptySpace
  (_, EmptySpace) -> return a
  (TypeSpace t, TypeSpace t') -> do
    b1 <- liftIO $ doesUnifyWith t t'
    b2 <- canDecompose t
    b3 <- canDecompose t'
    if b1 then pure EmptySpace
    else if b2 then tryDecomposeA t
    else if b3 then tryDecomposeB t'
    else pure a
  (TypeSpace t1, ConstructorSpace {}) -> do
    b1 <- canDecompose t1
    if b1 then tryDecomposeA t1
    else pure a
  (_, UnionSpace ss) -> foldlM subtractSpaceWith a ss
  (UnionSpace ss, _) -> do
    ss' <- mapM (`subtractSpaceWith` b) ss
    return $ UnionSpace ss'
  (ConstructorSpace t1 _ _, TypeSpace t2) -> do
    b1 <- t1 `isInstanceOf` t2
    sim <- simplify a
    b2 <- canDecompose t2

    if b1 || sim == EmptySpace then pure EmptySpace
    else if b2 then tryDecomposeB t2
    else pure a
  (ConstructorSpace t1 tys ss, ConstructorSpace t2 _ ss') -> do
    let b1 = t1 /= t2
    b2 <- anyM 
      (\(x, y) -> 
        (==EmptySpace) <$> (simplify =<< (x `intersectWith` y))) 
      (zip ss ss')
    b3 <- allM (uncurry isSubspaceOf) (zip ss ss')
    if b1 || b2 then pure a
    else if b3 then pure EmptySpace
    else do
      ss'' <- zipWithM subtractSpaceWith ss ss'

      let res = zip ss'' [0..]
      let res' = map (\(ri, i) -> ConstructorSpace t1 tys (updateAt i ri ss)) res
      simplify (UnionSpace res')
  (ConstantPoint c _, ConstantPoint c' _) -> return $ if c == c' then EmptySpace else a
  (ConstantPoint _ t, TypeSpace t') -> do
    b1 <- liftIO $ doesUnifyWith t t'
    b2 <- canDecompose t'
    if b1 then pure EmptySpace
    else if b2 then tryDecomposeB t'
    else pure a
  (ConstantPoint _ _, _) -> return a
  (TypeSpace t1, ConstantPoint _ _) -> do
    b1 <- canDecompose t1
    if b1 then tryDecomposeA t1
    else pure a
  (_, ConstantPoint _ _) -> return a
  (VariablePoint x _, VariablePoint y _) -> return $ if x == y then EmptySpace else a
  (VariablePoint _ t, TypeSpace t') -> do
    b1 <- liftIO $ doesUnifyWith t t'
    b2 <- canDecompose t'
    if b1 then pure EmptySpace
    else if b2 then tryDecomposeB t'
    else pure a
  (TypeSpace t, VariablePoint _ t') -> do
    b1 <- liftIO $ doesUnifyWith t t'
    b2 <- canDecompose t
    if b1 then pure EmptySpace
    else if b2 then tryDecomposeA t
    else pure a
  (VariablePoint "?" _, _) -> return EmptySpace
  (_, VariablePoint "?" _) -> return EmptySpace
  (VariablePoint _ t, ConstructorSpace k _ _) -> do
    b1 <- k `isInstanceOf` t
    if b1 then TypeSpace t `subtractSpaceWith` a
    else return a
  (ConstructorSpace k _ _, VariablePoint _ t) -> do
    b1 <- k `isInstanceOf` t
    if b1 then a `subtractSpaceWith` TypeSpace t
    else return a
  where
    tryDecomposeA :: PlumeType -> Checker Space
    tryDecomposeA t = do
      sp <- decompose t
      let un = UnionSpace sp
      un `subtractSpaceWith` b
    
    tryDecomposeB :: PlumeType -> Checker Space
    tryDecomposeB t = do
      sp <- decompose t
      let un = UnionSpace sp
      a `subtractSpaceWith` un

project :: Post.Pattern -> Checker Space
project (Post.PVariable x t) = return $ VariablePoint x t
project (Post.PLiteral l) = do
  let (ty, l') = typeOfLiteral l
  return $ ConstantPoint (Literal l') ty
project (Post.PConstructor t (args :->: _) pats) = do
  ps <- mapM project pats
  return $ ConstructorSpace t args ps
project (Post.PSpecialVar t _) = return $ ConstructorSpace t [] []
project (Post.PWildcard t) = return (VariablePoint "?" t) 
project (Post.PList _ [] _) = return (ConstructorSpace "nil" [] [])
project (Post.PList t xs Nothing) = do
  ps <- mapM project xs
  return $ buildCons t ps (ConstructorSpace "nil" [] [])
project (Post.PList t pats (Just sl)) = do
  ps <- mapM project pats
  sl' <- project sl
  return $ buildCons t ps sl'
project (Post.PConstructor {}) = throw $ CompilerError "Invalid pattern"

buildCons :: PlumeType -> [Space] -> Space -> Space
buildCons _ [] x = x
buildCons t (x:xs) y = ConstructorSpace "cons" [t, TList t] [x, buildCons t xs y]

-- | Function that updates a list at a given index
updateAt :: Int -> a -> [a] -> [a]
updateAt i x xs = case splitAt i xs of
  (before, _:after) -> before ++ x : after
  _ -> xs

foldCustom :: (a -> a -> a) -> [a] -> a
foldCustom f (x:xs) = foldl' f x xs
foldCustom _ [] = error "Empty list"

isInstanceOf :: Text -> PlumeType -> Checker Bool
isInstanceOf "nil" t = liftIO $ doesUnifyWith t (TList tA)
isInstanceOf "cons" t = liftIO $ doesUnifyWith t (TList tA)
isInstanceOf "true" t = liftIO $ doesUnifyWith t TBool
isInstanceOf "false" t = liftIO $ doesUnifyWith t TBool
isInstanceOf name t1 = do
  t2 <- searchEnv @"datatypeEnv" name
  case t2 of
    Just (_ :->: header) -> liftIO $ doesUnifyWith t1 header
    Just header -> liftIO $ doesUnifyWith t1 header
    Nothing -> return False

showSpace :: Space -> String
showSpace EmptySpace = "nothing"
showSpace (TypeSpace t) = show t
showSpace (UnionSpace xs) = showUnionSp xs
showSpace (ConstructorSpace "cons" _ (x:_)) = "[" ++ showSpace x ++ "]"
showSpace (ConstructorSpace "nil" _ _) = "[]"
showSpace (ConstructorSpace t _ []) = toString t
showSpace (ConstructorSpace "tuple" _ xs) = "(" ++ showCtorSp xs ++ ")"
showSpace (ConstructorSpace t _ xs) = toString t ++ "(" ++ showCtorSp xs ++ ")"
showSpace (VariablePoint x _) = toString x
showSpace (ConstantPoint (Literal c) _) = showLiteral c
showSpace (ConstantPoint (EnumVar x) _) = toString x

showLiteral :: Literal -> String
showLiteral (LInt i) = show i
showLiteral (LFloat f) = show f
showLiteral (LBool b) = if b then "true" else "false"
showLiteral (LString s) = show s
showLiteral (LChar c) = show c

showCtorSp :: [Space] -> String
showCtorSp [x] = showSpace x
showCtorSp (x:xs) = showSpace x <> ", " <> showCtorSp xs
showCtorSp [] = ""

showUnionSp :: [Space] -> String
showUnionSp [x] = showSpace x
showUnionSp [x, y] = showSpace x <> ", and " <> showSpace y
showUnionSp (x:xs) = showSpace x <> ", " <> showUnionSp xs
showUnionSp [] = ""

removeCtor :: [Space] -> Text -> [Space]
removeCtor xs t = filter (\case ConstructorSpace t' _ _ -> t /= t'; _ -> True) xs