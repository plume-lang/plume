module Language.Plume.Frontend.TypeChecking.Monad where

import Control.Monad.Except qualified as Err
import Control.Monad.Result
import Language.Plume.Syntax.HLIR qualified as HLIR
import Prelude hiding (get, modify, put, local)
import GHC.IO qualified as IO
import Data.Map qualified as Map
import Data.Text qualified as T
import Control.Monad.Position qualified as Pos
import Data.List qualified as List

type MonadChecker m = (MonadIO m, Err.MonadError (PlumeError, HLIR.Position) m)

data CheckerState = MkCheckerState {
  variables :: Map Text HLIR.PlumeScheme,
  generics :: Map Text HLIR.PlumeType,
  returnType :: Maybe HLIR.PlumeType
}

{-# NOINLINE symbolCounter #-}
symbolCounter :: IORef Int
symbolCounter = IO.unsafePerformIO . newIORef $ 0

{-# NOINLINE currentLevel #-}
currentLevel :: IORef Int
currentLevel = IO.unsafePerformIO . newIORef $ 0

{-# NOINLINE checkerState #-}
checkerState :: IORef CheckerState
checkerState = IO.unsafePerformIO . newIORef $
  MkCheckerState Map.empty Map.empty Nothing

get :: MonadChecker m => m CheckerState
get = liftIO $ readIORef checkerState

put :: MonadChecker m => CheckerState -> m ()
put st = liftIO $ writeIORef checkerState st

modify :: MonadChecker m => (CheckerState -> CheckerState) -> m ()
modify f = put . f =<< get

local :: MonadChecker m => (CheckerState -> CheckerState) -> m a -> m a
local f m = do
  st <- get
  put $ f st
  result <- m
  put st
  pure result

gets :: MonadChecker m => (CheckerState -> a) -> m a
gets f = f <$> get

-- Create new fresh types
newTypeVar :: MonadChecker m => m HLIR.PlumeType
newTypeVar = do
  i <- liftIO $ readIORef symbolCounter
  lvl <- liftIO $ readIORef currentLevel
  liftIO $ writeIORef symbolCounter (i + 1)
  t <- newIORef $ HLIR.Unbound (T.pack $ "t" <> show i) lvl
  pure $ HLIR.MkTyVar t

instantiate :: MonadChecker m => HLIR.PlumeScheme -> m HLIR.PlumeType
instantiate (HLIR.MkTyScheme vars ty) = do
  subst <- Map.fromList <$> traverse (\v -> (v,) <$> newTypeVar) vars
  substType subst ty

substType :: MonadChecker m => Map Text HLIR.PlumeType -> HLIR.PlumeType -> m HLIR.PlumeType
substType subst (HLIR.MkTyId idtf) =
  pure $ case Map.lookup idtf subst of
    Just ty -> ty
    Nothing -> HLIR.MkTyId idtf
substType subst (HLIR.MkTyVar ref) = do
  ty <- readIORef ref
  case ty of
    HLIR.Link ty' -> substType subst ty'
    HLIR.Unbound idtf _ -> case Map.lookup idtf subst of
      Just ty' -> pure ty'
      Nothing -> pure $ HLIR.MkTyVar ref
substType subst (HLIR.MkTyApp ty args) =
  HLIR.MkTyApp
    <$> substType subst ty
    <*> traverse (substType subst) args
substType _ ty = pure ty

generalize :: MonadChecker m => HLIR.PlumeType -> m HLIR.PlumeScheme
generalize ty = do
  vars <- List.nub <$> getFreeVars ty
  pure $ HLIR.MkTyScheme vars ty

getFreeVars :: MonadIO m => HLIR.PlumeType -> m [Text]
getFreeVars (HLIR.MkTyId _) = pure []
getFreeVars (HLIR.MkTyVar ref) = do
  ty <- readIORef ref
  case ty of
    HLIR.Link ty' -> getFreeVars ty'
    HLIR.Unbound idtf _ -> pure [idtf]
getFreeVars (HLIR.MkTyApp ty args) = do
  vars <- getFreeVars ty
  vars' <- traverse getFreeVars args
  pure $ vars <> concat vars'
getFreeVars _ = pure []

throw :: MonadChecker m => PlumeError -> m a
throw err = do
  pos <- Pos.fetchPosition
  Err.throwError (err, pos)

defaultVariables :: Map Text HLIR.PlumeScheme
defaultVariables = Map.fromList
  [ ("+", HLIR.MkTyScheme [] binInt)
  , ("-", HLIR.MkTyScheme [] binInt)
  , ("*", HLIR.MkTyScheme [] binInt)
  , ("/", HLIR.MkTyScheme [] binInt)
  , ("==", HLIR.MkTyScheme [] binIntBool)
  , ("<", HLIR.MkTyScheme [] binIntBool)
  , (">", HLIR.MkTyScheme [] binIntBool)
  , ("<=", HLIR.MkTyScheme [] binIntBool)
  , (">=", HLIR.MkTyScheme [] binIntBool)
  , ("and", HLIR.MkTyScheme [] binBool)
  , ("or", HLIR.MkTyScheme [] binBool)
  , ("not", HLIR.MkTyScheme [] ([HLIR.MkTyBool] HLIR.:->: HLIR.MkTyBool))
  ]
  where
    binInt = [HLIR.MkTyInt, HLIR.MkTyInt] HLIR.:->: HLIR.MkTyInt
    binBool = [HLIR.MkTyBool, HLIR.MkTyBool] HLIR.:->: HLIR.MkTyBool
    binIntBool = [HLIR.MkTyInt, HLIR.MkTyInt] HLIR.:->: HLIR.MkTyBool

generateGeneric :: MonadChecker m => Text -> m (Text, HLIR.PlumeType)
generateGeneric name = (name,) <$> newTypeVar
