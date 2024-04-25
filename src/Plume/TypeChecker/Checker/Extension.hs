{-# LANGUAGE LambdaCase #-}

module Plume.TypeChecker.Checker.Extension where

import Data.Map qualified as Map
import Plume.Syntax.Abstract qualified as Pre
import Plume.Syntax.Common.Annotation
import Plume.Syntax.Common.Type qualified as Pre
import Plume.TypeChecker.Checker.Closure (createEnvFromAnnotations)
import Plume.TypeChecker.Checker.Monad
import Plume.TypeChecker.Constraints.Solver
import Plume.TypeChecker.Monad.Conversion
import Plume.TypeChecker.TLIR qualified as Post
import Plume.Syntax.Common.Type (getGenericName)
import Plume.TypeChecker.Constraints.Unification (doesUnifyWith, compressPaths)

type Converted = Either PlumeType PlumeType

synthExt :: Infer -> Infer
synthExt
  infer
  (Pre.ETypeExtension generics (Annotation extVar extTy) methods) = do
    methods' <-
      mapM
        ( synthExtMember
            infer
            (extVar, extTy, generics)
        )
        methods

    pure (TUnit, methods')
synthExt _ _ = throw $ CompilerError "Only type extensions are supported"

synthExtMember
  :: Infer
  -> (Text, Pre.PlumeType, [Pre.PlumeGeneric])
  -> Pre.ExtensionMem
  -> Checker Post.Expression
-- Synthesize a fully annotated extension member. The difference is that we
-- don't need to do many inference steps, we can just infer the body and
-- check if the inferred type is the same as the annotated one.
--
-- Remember that infered types are always the final type, annotations just help
-- the user to write better and more readable code.
synthExtMember 
  infer (extVar, preExtTy, extGens) 
  e@(Pre.ExtDeclaration gens ann c@(Pre.EClosure args ret body)) 
  | isExtAnnotated e || isAnnotated c = do
    let gens' = extGens <> gens
    void (mapM convert gens' :: Checker [PlumeType])

    extTy <- convert preExtTy
    convertedArgs <- convert args
    convertedRet  <- convert ret

    let preFun = (extTy : map (.annotationValue) convertedArgs) :->: convertedRet
        argsSchemes = createEnvFromAnnotations convertedArgs
        schemes = Map.insert extVar extTy argsSchemes

    insertEnvWith @"typeEnv" Map.union schemes

    let preExt = MkExtension ann.annotationName extTy preFun
    exts <- gets extensions
    preExts <- liftIO $ removeDuplicates $ preExt : exts
    modifyIORef' checkState $ \s -> s {extensions = preExts}

    enterLevel

    (inferedTy, body') <- 
      local (\s -> s {returnType = Just convertedRet}) . extractFromArray $ 
        infer body

    inferedTy `unifiesWith` convertedRet

    deleteManyEnv @"genericsEnv" (map getGenericName (extGens <> gens))
    deleteManyEnv @"typeEnv" (Map.keys schemes)

    compressedFun <- liftIO $ compressPaths preFun
    let compressedExt = MkExtension ann.annotationName extTy compressedFun
    preExts' <- liftIO $ removeDuplicates $ compressedExt : exts
    modifyIORef' checkState $ \s -> s {extensions = preExts'}
    
    exitLevel

    pure $
      Post.EExtensionDeclaration
        ann.annotationName
        extTy
        (Annotation extVar extTy)
        (Post.EClosure convertedArgs inferedTy body')
synthExtMember 
  infer (extVar, preExtTy, extGens) 
  (Pre.ExtDeclaration gens ann (Pre.EClosure args ret body)) = do
    let gens' = extGens <> gens
    void (mapM convert gens' :: Checker [PlumeType])

    extTy :: PlumeType           <- convert preExtTy
    convertedArgs :: [Converted] <- mapM (convertMaybe . fst . annotationValue) args
    convertedRet :: Converted    <- convertMaybe ret

    preFun :: PlumeType <- fresh

    let argsNames = map annotationName args
        env = createEnvFromConverted $ zip argsNames convertedArgs
        schemes = Map.insert extVar extTy env

    insertEnvWith @"typeEnv" Map.union schemes

    let preExt = MkExtension ann.annotationName extTy preFun
    exts <- gets extensions
    preExts <- liftIO $ removeDuplicates $ preExt : exts
    modifyIORef' checkState $ \s -> s {extensions = preExts}

    let convertedRet' = getConverted convertedRet

    enterLevel
    (inferedTy, body') <- 
      local (\s -> s {returnType = Just convertedRet'}) .  extractFromArray $ 
        infer body

    let extFunTy = (extTy : map getConverted convertedArgs) :->: inferedTy
    convertedRet' `unifiesWith` inferedTy
    preFun `unifiesWith` extFunTy

    deleteManyEnv @"genericsEnv" (map getGenericName gens')

    compressedFun <- liftIO $ compressPaths preFun
    let compressedExt = MkExtension ann.annotationName extTy compressedFun
    preExts' <- liftIO $ removeDuplicates $ compressedExt : exts
    modifyIORef' checkState $ \s -> s {extensions = preExts'}

    exitLevel

    let newArgs = zipWith Annotation argsNames (map getConverted convertedArgs)

    pure $
      Post.EExtensionDeclaration
        ann.annotationName
        extTy
        (Annotation extVar extTy)
        (Post.EClosure newArgs inferedTy body')
synthExtMember _ _ _ = throw $ CompilerError "Only extension members are supported"

createEnvFromConverted :: [(Text, Converted)] -> Map Text PlumeScheme
createEnvFromConverted ((name, Right ty) : xs) = Map.insert name ty (createEnvFromConverted xs)
createEnvFromConverted ((name, Left ty) : xs) = Map.insert name ty (createEnvFromConverted xs)
createEnvFromConverted [] = mempty

-- synthExtMember
--   :: Infer
--   -> (Text, Pre.PlumeType, [Pre.PlumeGeneric])
--   -> Pre.ExtensionMem
--   -> Checker Post.Expression
-- synthExtMember
--   infer
--   (extVar, preExtTy, extGens)
--   ( Pre.ExtDeclaration
--       generics
--       (Annotation name _)
--       (Pre.EClosure args ret body)
--     ) = local id $ do
--     convertedGenerics :: [PlumeType] <- mapM convert (extGens <> generics)

--     extTy :: PlumeType <- convert preExtTy
--     convertedArgs :: [Annotation PlumeType] <- convert args
--     convertedRet :: PlumeType <- convert ret

--     -- Building a pre-function type with the converted arguments and return type
--     -- and then building the first extension template, used to allow recursive
--     -- extension calls
--     let preFun = (extTy : map (.annotationValue) convertedArgs) :->: convertedRet

--     let ext = MkExtension name extTy preFun
    
--     modifyIORef' checkState $ \s ->
--       s {extensions = removeDuplicates $ ext : s.extensions}

--     -- Creating a new environment with the arguments as type schemes
--     let argSchemes = createEnvFromAnnotations convertedArgs
--     let schemes = Map.insert extVar extTy argSchemes
--     insertEnvWith @"typeEnv" Map.union schemes

--     let isFinalSignature = all (isJust . fst . annotationValue) args && isJust ret

--     enterLevel
--     ((retTy, body'), finalExt) <- case convertedGenerics of
--       -- If there are no generics, we can just infer the expression
--       [] -> do
--         (retTy, expr) <- local (\s -> s {returnType = Just convertedRet}) $ extractFromArray (infer body)
--         retTy `unifiesTo` convertedRet

--         let closureTy = (extTy : map (.annotationValue) convertedArgs) :->: retTy

--         when isFinalSignature $ closureTy `unifiesTo` preFun

--         let scheme = if isFinalSignature
--               then preFun
--               else closureTy

--         let newExt = MkExtension name extTy scheme

--         let finalRetTy = if isFinalSignature
--               then convertedRet
--               else retTy

--         pure ((finalRetTy, expr), newExt)

--       -- Otherwise, that's kind of the same trick used for variable
--       -- declarations: we need to locally solve the generated constraints
--       -- from the body in order to get rid of the unbound type generics.
--       _ -> do
--         ((retTy, expr), cs) <- local (\s -> s {returnType = Just convertedRet}) . getLocalConstraints $ extractFromArray $ infer body

--         let closureTy = (extTy : map (.annotationValue) convertedArgs) :->: retTy

--         c1 <- createConstraint (closureTy :~: preFun)
--         c2 <- createConstraint (retTy :~: convertedRet)

--         let cs' = cs.tyConstraints <> if isFinalSignature then [c1, c2] else []

--         writeIORef cyclicCounter 0
--         solve cs'

--         let newScheme = if isFinalSignature 
--               then preFun
--               else closureTy

--         let newExt = MkExtension name extTy newScheme

--         exts <- gets extensions
--         let appliedExts = removeDuplicates $ newExt : exts
--         when (name == "head") $ do
--           print newExt
--           print appliedExts
--           print preFun

--         void $ solveExtend cs.extConstraints appliedExts

--         let r = if isFinalSignature
--               then convertedRet
--               else retTy

--         return ((r, expr), newExt)
--     exitLevel

--     finalExt' <- removeExtLink finalExt

--     -- Inserting new extension in the environment and deleting the old one
--     modifyIORef' checkState $ \s ->
--       s {extensions = removeDuplicates $ finalExt : s.extensions}

--     -- Removing the generic types from the environment
--     mapM_
--       ( \case
--           Pre.GVar v -> deleteEnv @"genericsEnv" v
--           _ -> pure ()
--       )
--       generics

--     pure $
--       Post.EExtensionDeclaration
--         name
--         extTy
--         (Annotation extVar extTy)
--         (Post.EClosure convertedArgs retTy body')
-- synthExtMember _ _ _ =
--   throw $ CompilerError "Only extension members are supported"

getConverted :: Converted -> PlumeType
getConverted = \case
  Left ty -> ty
  Right ty -> ty

convertMaybe :: a `ConvertsTo` b => Maybe a -> Checker (Either PlumeType b)
convertMaybe = \case
  Just x -> Right <$> convert x
  Nothing -> Left <$> fresh

(=$=) :: Extension -> Extension -> IO Bool
(MkExtension n1 t1 f1) =$= (MkExtension n2 t2 f2) = do
  b1 <- t1 `doesUnifyWith` t2 
  b2 <- f1 `doesUnifyWith` f2

  return $ n1 == n2 && b1 && b2

nubBy :: [Extension] -> IO [Extension]
nubBy []             =  pure []
nubBy (x:xs)         = do
  nubbed <- nubBy =<< filterM (\y -> do
      b <- x =$= y
      return $ not b
    ) xs
  return $ x : nubbed

-- | Removes duplicate extensions from the set
-- | It just check if two names are equal and if the types are unifiable
removeDuplicates :: [Extension] -> IO [Extension]
removeDuplicates = nubBy
--  where
--   go [] acc = acc
--   go (x : xs') acc
--     | doesExtensionExist x acc = go xs' acc
--     | otherwise = go xs' (x : acc)

--   doesExtensionExist :: Extension -> [Extension] -> Bool
--   doesExtensionExist e@(MkExtension n1 t1 s1) (MkExtension n2 t2 s2 : xs')
--     | n1 == n2 && (t1 `doesUnifyWith` t2 || s1 `doesUnifyWith` s2) = True
--     | otherwise = doesExtensionExist e xs'
--   doesExtensionExist _ [] = False
  