module Plume.Syntax.Memory where

import Data.List qualified as List
import Data.Set qualified as Set
import GHC.IO qualified as IO
import Plume.Syntax.Abstract qualified as AST
import Plume.Syntax.Common qualified as AST
import Plume.Syntax.Concrete.Expression (TypeConstructor (..))

type AllocatedVariables = Set (Text, Int)
type NativeFunctions = Set Text

{-# NOINLINE allocatedVariables #-}
allocatedVariables :: IORef AllocatedVariables
allocatedVariables = IO.unsafePerformIO $ newIORef mempty

{-# NOINLINE nativeFunctions #-}
nativeFunctions :: IORef NativeFunctions
nativeFunctions = IO.unsafePerformIO $ newIORef mempty

{-# NOINLINE returnCounter #-}
returnCounter :: IORef Int
returnCounter = IO.unsafePerformIO $ newIORef 0

instance Semigroup Int where
  (<>) = (+)

instance Monoid Int where
  mempty = 0

updateAt :: (Eq a, Monoid b, Num b) => a -> (b -> b) -> [(a, b)] -> [(a, b)]
updateAt x f [] = [(x, f mempty <> 1)]
updateAt x f (y@(x', y') : ys) = if x == x' then (x', f y') : ys else y : updateAt x f ys

addRef :: Text -> IO ()
addRef n = do
  natives <- readIORef nativeFunctions
  if n `Set.member` natives
    then pure ()
    else modifyIORef' 
      allocatedVariables 
      (Set.fromList . updateAt n (+ 1) . Set.toList)

assoc :: [(Text, Int)] -> [(Text, Int)] -> [(Text, (Int, Int))]
assoc xs ys = [(n, (c, c')) | (n, c) <- xs, let c' = fromMaybe 0 (List.lookup n ys)]

withScope :: IO a -> IO (a, AllocatedVariables)
withScope f = do
  old <- readIORef allocatedVariables
  a <- f
  new <- readIORef allocatedVariables
  writeIORef allocatedVariables old

  let as = assoc (Set.toList new) (Set.toList old)
      new' = map (\(n, (c, c')) -> (n, c - c')) as

  pure (a, Set.fromList new')

class Memory a where
  transform :: a -> IO a

ref :: Text -> IO (AST.AbstractExpression a)
ref x = do
  natives <- readIORef nativeFunctions

  if x `Set.member` natives
    then pure $ AST.EVariable x
    else pure $ AST.EApplication (AST.EVariable "copy_ref") [AST.EVariable x]

fresh :: IO Text
fresh = do
  i <- readIORef returnCounter
  modifyIORef' returnCounter (+ 1)
  pure $ "ret_value_" <> show i

insertBeforeReturn
  :: [AST.AbstractExpression a]
  -> [AST.AbstractExpression a]
  -> IO [AST.AbstractExpression a]
insertBeforeReturn es [] = pure es
insertBeforeReturn ins es = case List.unsnoc es of
  Just (st, expr) | isReturn expr && not (null ins) -> do
    n <- fresh
    let val = extractReturnValue expr
        decl =
          AST.EDeclaration
            False
            []
            (n AST.:@: Nothing)
            val
            Nothing
        ret = retValueReturn expr n

    pure $ st <> [decl] <> ins <> [ret]
  _ -> pure $ es <> ins

isReturn :: AST.AbstractExpression a -> Bool
isReturn (AST.EReturn _) = True
isReturn (AST.ELocated e _) = isReturn e
isReturn _ = False

retValueReturn :: AST.AbstractExpression a -> Text -> AST.AbstractExpression a
retValueReturn (AST.EReturn _) n = AST.EReturn (AST.EVariable n)
retValueReturn (AST.ELocated e _) n = retValueReturn e n
retValueReturn _ _ = error "No return value"

extractReturnValue :: AST.AbstractExpression a -> AST.AbstractExpression a
extractReturnValue (AST.EReturn e) = e
extractReturnValue (AST.ELocated e _) = extractReturnValue e
extractReturnValue _ = error "No return value"

instance (Memory a) => Memory (AST.AbstractExpression a) where
  transform (AST.EVariable n) = pure $ AST.EVariable n
  transform (AST.ELiteral l) = pure $ AST.ELiteral l
  transform (AST.EApplication e es) = do
    e' <- transform e
    es' <- mapM transformArg es
    pure $ AST.EApplication e' es'
   where
    transformArg (AST.EVariable n) = addRef n >> ref n
    transformArg (AST.ELocated e' p) = do
      e'' <- transformArg e'
      pure $ AST.ELocated e'' p
    transformArg e' = transform e'
  transform (AST.EUnMut e) = AST.EUnMut <$> transform e
  transform (AST.EDeclaration g isMut ann e me) = do
    e' <- transformDecl e
    me' <- mapM transform me
    pure $ AST.EDeclaration g isMut ann e' me'
   where
    transformDecl (AST.EVariable n) = addRef n >> ref n
    transformDecl (AST.ELocated e' p) = do
      e'' <- transformDecl e'
      pure $ AST.ELocated e'' p
    transformDecl e' = transform e'
  transform (AST.EConditionBranch e1 e2 e3) = do
    e1' <- transform e1
    e2' <- transform e2
    e3' <- mapM transform e3
    pure $ AST.EConditionBranch e1' e2' e3'
  transform (AST.EClosure anns t e) = do
    let vars = map (.annotationName) anns
    e' <- transform e

    modifyIORef' allocatedVariables (Set.filter (\(n, _) -> n `notElem` vars))

    pure $ AST.EClosure anns t e'
  transform (AST.EBlock es) = do
    (es', allocas) <- withScope $ transform es
    let deallocs = buildDeallocs allocas
    AST.EBlock <$> insertBeforeReturn deallocs es'
  transform (AST.ELocated e p) = do
    e' <- transform e
    pure $ AST.ELocated e' p
  transform (AST.ESwitch e ps) = do
    e' <- transform e
    ps' <- mapM transform ps
    pure $ AST.ESwitch e' ps'
  transform (AST.EList es) = do
    es' <- mapM transform es
    pure $ AST.EList es'
  transform (AST.EType ann ts) = do
    ts' <- mapM transform ts
    pure $ AST.EType ann ts'
  transform (AST.EReturn e) = do
    e' <- transformRet e
    pure $ AST.EReturn e'
   where
    transformRet (AST.EVariable n) = ref n
    transformRet (AST.ELocated e' p) = do
      e'' <- transformRet e'
      pure $ AST.ELocated e'' p
    transformRet e' = transform e'
  transform (AST.ETypeExtension g ann var ems) = do
    ems' <- mapM transform ems
    pure $ AST.ETypeExtension g ann var ems'
  transform (AST.ENativeFunction fp n gens t st) = do
    modifyIORef' nativeFunctions (Set.insert n)
    pure $ AST.ENativeFunction fp n gens t st
  transform (AST.EInterface ann gs ms) = pure $ AST.EInterface ann gs ms

instance Memory AST.PlumeType where transform = pure

instance (Memory a) => Memory (TypeConstructor a) where
  transform (TConstructor x xs) = do
    xs' <- mapM transform xs
    pure $ TConstructor x xs'
  transform (TVariable x) = pure $ TVariable x

instance (Memory a) => Memory (AST.ExtensionMember a) where
  transform (AST.ExtDeclaration g ann e) = do
    e' <- transform e
    pure $ AST.ExtDeclaration g ann e'

instance Memory AST.Pattern where
  transform (AST.PVariable n) = pure $ AST.PVariable n
  transform AST.PWildcard = pure AST.PWildcard
  transform (AST.PConstructor n ps) = do
    ps' <- mapM transform ps
    pure $ AST.PConstructor n ps'
  transform (AST.PList ps sl) = do
    ps' <- mapM transform ps
    sl' <- mapM transform sl
    pure $ AST.PList ps' sl'
  transform (AST.PSlice n) = pure $ AST.PSlice n
  transform (AST.PLiteral l) = pure $ AST.PLiteral l

instance (Memory a, Memory b) => Memory (a, AST.AbstractExpression b) where
  transform (a, b) = do
    a' <- transform a
    (b', allocs) <- withScope $ transform b

    let block = case b' of
          AST.EBlock b'' -> b''
          e | not (isReturn e) -> [AST.EReturn e]
          _ -> [b']

    let deallocs = buildDeallocs allocs
    block' <- insertBeforeReturn deallocs block

    pure
      ( a'
      , case block' of
          [AST.EReturn e] -> e
          [b''] -> b''
          _ -> AST.EBlock block'
      )

instance (Memory a) => Memory [a] where
  transform = mapM transform

instance {-# OVERLAPPING #-} (Memory a) => Memory [AST.AbstractExpression a] where
  transform xs = do
    (xs', allocs) <- withScope $ mapM transform xs
    let deallocs = buildDeallocs allocs

    insertBeforeReturn deallocs xs'

buildDeallocs :: AllocatedVariables -> [AST.AbstractExpression a]
buildDeallocs =
  concatMap
    ( \(n, c) ->
        [ AST.EApplication
          (AST.EVariable "free_ref")
          [AST.EVariable n, AST.ELiteral (AST.LInt (toInteger c))]
        | c > 0
        ]
    )
