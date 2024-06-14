{-# LANGUAGE LambdaCase #-}

module Plume.Compiler.Desugaring.Modules.Switch where

import Data.Map qualified as M
import Data.Set qualified as S
import Data.List qualified as L
import GHC.IO hiding (liftIO)
import Plume.Compiler.ClosureConversion.Free
import Plume.Compiler.ClosureConversion.Syntax qualified as Pre
import Plume.Compiler.Desugaring.Monad
import Plume.Compiler.Desugaring.Syntax qualified as Post
import Plume.Syntax.Common.Literal
import Control.Monad.Exception (compilerError)
import Plume.Syntax.Translation.Generics

type DesugarStmt =
  Desugar Pre.ClosedStatement [ANFResult (Maybe Post.DesugaredStatement)]

type DesugarExpr = Information -> Pre.ClosedExpr -> MonadDesugar (ANFResult Post.DesugaredExpr)

type DesugarSwitch =
  (DesugarExpr, DesugarStmt) -> Desugar Pre.ClosedExpr (ANFResult Post.DesugaredExpr)

type IsToplevel = Bool
type IsReturned = Bool
type IsExpression = Bool
type Information = (IsToplevel, IsReturned, IsExpression)

{-# NOINLINE switchCounter #-}
switchCounter :: IORef Int
switchCounter = unsafePerformIO $ newIORef 0

desugarSwitch :: (IsToplevel, IsReturned, IsExpression) -> DesugarSwitch
desugarSwitch info@(_, shouldReturn, isExpr) (fExpr, fStmt) (Pre.CESwitch x cases) = do
  let freedPat = foldMap (free . fst) cases
  (decl, declVar) <- case x of
    Pre.CEVar n | n `S.notMember` freedPat -> return ([], x)
    _ -> do
      i <- readIORef switchCounter
      let switchName = "$switch" <> show i
      modifyIORef' switchCounter (+ 1)
      return (createCloseLets (M.singleton switchName x), Pre.CEVar switchName)
  let (conds, maps) = unzip $ map (createCondition declVar . fst) cases

  let bodies = map snd cases
  let cases' = zip3 [0 ..] bodies maps

  (stmts, res) <-
    mapAndUnzipM
      ( \case
          (i, expr, m) -> do
            let pat = maybeAt i conds
            let expr' = substituteMany (M.toList m) expr
            (expr'', stmts'') <- fExpr info expr'
            let lastStmt =
                  if (shouldExprReturn expr' && isExpr) || shouldReturn
                    then Post.DSReturn expr''
                    else Post.DSExpr expr''
            let stmts''' = stmts'' <> [lastStmt]
            case pat of
              Nothing -> return (mempty, Post.DEIf (Post.DELiteral (LBool True)) stmts''' [])
              Just pats -> do
                (pats', stmts2) <- mapAndUnzipM (fExpr info) pats
                let cond = createConditionExpr pats'
                return (stmts2, Post.DEIf cond stmts''' [])
      )
      cases'

  let stmts' = concat $ concat stmts

  t <- concat <$> sequenceMapM fStmt decl
  let finalDecl = L.foldl (\acc (x', stmts'') -> acc <> stmts'' <> maybeToList x') [] t

  if isExpr
    then do
      let ifs'' = createIfsExpr res
      return (ifs'', stmts' <> finalDecl)
    else do
      let ifs' = createIfsStatement res
      return (Post.DEVar "nil", stmts' <> finalDecl <> ifs')
desugarSwitch _ _ _ = compilerError "Received incorrect expression, not a switch."

unboxStatement :: Post.DesugaredStatement -> Post.DesugaredExpr
unboxStatement (Post.DSExpr x) = x
unboxStatement _ = compilerError "Incorrect statement"

createConditionExpr :: [Post.DesugaredExpr] -> Post.DesugaredExpr
createConditionExpr [] = Post.DELiteral (LBool True)
createConditionExpr [x] = x
createConditionExpr (x : xs) = x `and'` createConditionExpr xs

createIfsStatement
  :: [Post.DesugaredExpr]
  -> [Post.DesugaredStatement]
createIfsStatement [] = []
createIfsStatement (Post.DEIf c t _ : xs)
  | c == Post.DELiteral (LBool True) = t
  | otherwise = [Post.DSIf c t (createIfsStatement xs)]
createIfsStatement (x : xs) = Post.DSExpr x : createIfsStatement xs

shouldNotHappen :: Text -> Post.DesugaredStatement
shouldNotHappen x =
  Post.DSExpr
    ( Post.DEApplication
        "println"
        [Post.DELiteral (LString ("Should not happen" <> x))]
    )

and' :: Post.DesugaredExpr -> Post.DesugaredExpr -> Post.DesugaredExpr
and' x y = Post.DEIf x [Post.DSExpr y] [Post.DSExpr (Post.DELiteral (LBool False))]

and'' :: Post.DesugaredExpr -> Post.DesugaredExpr -> Post.DesugaredExpr
and'' = Post.DEAnd

createIfsExpr
  :: [Post.DesugaredExpr]
  -> Post.DesugaredExpr
createIfsExpr [] = Post.DEVar "nil"
createIfsExpr [x] = x
createIfsExpr (Post.DEIf c t _ : xs) = Post.DEIf c t [e]
 where
  e = Post.DSExpr $ createIfsExpr xs
createIfsExpr _ = compilerError "Incorrect list of conditions"

createLets :: Map Text Post.DesugaredExpr -> [Post.DesugaredStatement]
createLets = M.foldrWithKey (\k v acc -> Post.DSDeclaration k v : acc) []

createCloseLets :: Map Text Pre.ClosedExpr -> [Pre.ClosedStatement]
createCloseLets = M.foldrWithKey (\k v acc -> Pre.CSDeclaration k v : acc) []

createCondition
  :: Pre.ClosedExpr
  -> Pre.ClosedPattern
  -> ([Pre.ClosedExpr], Map Text Pre.ClosedExpr)
createCondition _ Pre.CPWildcard = ([], mempty)
createCondition x (Pre.CPVariable y) = ([], M.singleton y x)
createCondition x (Pre.CPConstructor y xs) =
  let spc = Pre.CEEqualsTo (Pre.CEProperty x "0") Pre.CESpecial
      cons = Pre.CEEqualsTo (Pre.CEProperty x "2") (Pre.CELiteral (LString y))
      (conds, maps) = unzip $ zipWith (createCondition . Pre.CEProperty x) [show i | i <- [(3 :: Int) ..]] xs
   in (spc : cons : concat conds, mconcat maps)
createCondition x (Pre.CPLiteral l) =
  ([Pre.CEEqualsTo x (Pre.CELiteral l)], mempty)
createCondition x (Pre.CPSpecialVar n) = do
  let spc = Pre.CEEqualsTo (Pre.CEProperty x "0") Pre.CESpecial
  let cons = Pre.CEEqualsTo (Pre.CEProperty x "2") (Pre.CELiteral (LString n))
  ([spc, cons], mempty)
createCondition x (Pre.CPList pats slice) =
  let (conds, maps) =
        unzip $
          zipWith
            createCondition
            [Pre.CEProperty x (show i) | i <- [0 .. length pats - 1]]
            pats
      (conds', maps') = maybe mempty (createCondition (Pre.CESlice x (length pats))) slice
      patLen = fromIntegral $ length pats
      lenCond = case slice of
        Just _ ->
          Pre.CEApplication (Pre.CEVar ">") [
              Pre.CEApplication (Pre.CEVar "@list_length") [x]
            , Pre.CELiteral (LInt $ patLen - 1)
          ]
        Nothing ->
          Pre.CEEqualsTo
            (Pre.CEApplication (Pre.CEVar "@list_length") [x])
            (Pre.CELiteral (LInt (toInteger patLen)))
   in (lenCond : concat conds <> conds', mconcat maps <> maps')

shouldExprReturn :: Pre.ClosedExpr -> Bool
shouldExprReturn (Pre.CEBlock _) = False
shouldExprReturn _ = True
-- shouldExprReturn (Pre.CEBlock bs) = any shouldStmtReturn bs
-- shouldExprReturn (Pre.CESwitch _ cases) = any (shouldCaseReturn . snd) cases
-- shouldExprReturn (Pre.CEConditionBranch _ t e) = shouldExprReturn t || shouldExprReturn e
-- shouldExprReturn _ = False

-- shouldCaseReturn :: Pre.ClosedExpr -> Bool
-- shouldCaseReturn (Pre.CEBlock bs) = any shouldStmtReturn bs
-- shouldCaseReturn (Pre.CESwitch _ cases) = any (shouldExprReturn . snd) cases
-- shouldCaseReturn (Pre.CEConditionBranch _ t e) = shouldExprReturn t || shouldExprReturn e
-- shouldCaseReturn _ = True

-- shouldStmtReturn :: Pre.ClosedStatement -> Bool
-- shouldStmtReturn (Pre.CSReturn _) = True
-- shouldStmtReturn (Pre.CSConditionBranch _ t e) = shouldStmtReturn t || shouldStmtReturn e
-- shouldStmtReturn _ = False
