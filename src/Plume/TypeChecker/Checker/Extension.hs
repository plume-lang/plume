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
  