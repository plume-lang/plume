module Plume.Syntax.Blocks where

import Plume.Syntax.Abstract as AST

isExhaustiveReturn :: AST.Expression -> Bool
isExhaustiveReturn (AST.ELocated e _) = isExhaustiveReturn e
isExhaustiveReturn (AST.EReturn _) = True
isExhaustiveReturn (AST.EConditionBranch _ t f) = isExhaustiveReturn t && maybe False isExhaustiveReturn f
isExhaustiveReturn (AST.ESwitch _ cases) = all (isExhaustiveReturn . snd) cases
isExhaustiveReturn (AST.EBlock es) = isExhaustiveBlock es
isExhaustiveReturn _ = False

isItBlock :: AST.Expression -> Bool
isItBlock AST.EBlock{} = True
isItBlock (AST.ELocated e _) = isItBlock e
isItBlock _ = False

isExhaustiveBlock :: [AST.Expression] -> Bool
isExhaustiveBlock es = maybe False isExhaustiveReturn (viaNonEmpty last es)

nilReturn :: AST.Expression
nilReturn = AST.EReturn (AST.EVariable "unit" Nothing)

removeUselessBlocks :: (Bool, Bool) -> AST.Expression -> [AST.Expression]
removeUselessBlocks (isInner, isEx') (AST.EBlock es)
  | isInner = do
      let isEx = isEx' || isExhaustiveBlock es

      if isEx
        then [AST.EBlock $ concatMap (removeUselessBlocks (False, isEx)) es]
        else [AST.EBlock $ concatMap (removeUselessBlocks (False, isEx)) es <> [nilReturn]]
  | otherwise = do
      let isEx = isEx' || isExhaustiveBlock es

      if isEx
        then concatMap (removeUselessBlocks (False, isEx)) es
        else concatMap (removeUselessBlocks (False, isEx)) es <> [nilReturn]
removeUselessBlocks (_, isEx) (AST.EApplication f xs) = do
  f' <- removeUselessBlocks (True, isEx) f
  xs' <- mapM (removeUselessBlocks (True, isEx)) xs
  [AST.EApplication f' xs']
removeUselessBlocks isInner@(_, isEx) (AST.EConditionBranch e1 e2 e3) = do
  e1' <- removeUselessBlocks (True, isEx) e1
  let e2' = removeUselessBlocksIf isInner e2
  let e3' = removeUselessBlocksIf isInner <$> e3
  [AST.EConditionBranch e1' e2' e3']
removeUselessBlocks (_, isEx) (AST.EList es) = do
  es' <- mapM (removeUselessBlocks (True, isEx)) es
  [AST.EList es']
removeUselessBlocks isInner@(_, isEx) (AST.ESwitch e cases) = do
  e' <- removeUselessBlocks (True, isEx) e
  let cases' = map (second (removeUselessBlocksIf isInner)) cases
  [AST.ESwitch e' cases']
removeUselessBlocks isInner@(_, isEx) (AST.EDeclaration gens n e1 e2) = do
  e1' <- removeUselessBlocks (True, isEx) e1
  e2' <- mapM (removeUselessBlocks isInner) e2
  [AST.EDeclaration gens n e1' e2']
removeUselessBlocks _ (AST.EClosure args ret body isA) = do
  let isBl = isItBlock body
  let isEx = isExhaustiveReturn body
  let b = removeUselessBlocks (False, isEx) body
  case b of
    [AST.EBlock es] -> [AST.EClosure args ret (AST.EBlock es) isA]
    [x] | isEx || not isBl -> [AST.EClosure args ret x isA]
    _ -> [AST.EClosure args ret (AST.EBlock b) isA]
removeUselessBlocks isInner (AST.EUnMut e) = do
  e' <- removeUselessBlocks isInner e
  [AST.EUnMut e']
removeUselessBlocks _ (AST.EVariable n t) = [AST.EVariable n t]
removeUselessBlocks _ (AST.ELiteral l) = [AST.ELiteral l]
removeUselessBlocks isInner (AST.EReturn e) = do
  e' <- removeUselessBlocks isInner e
  [AST.EReturn e']
removeUselessBlocks isInner (AST.ELocated e p) = do
  e' <- removeUselessBlocks isInner e
  [AST.ELocated e' p]
removeUselessBlocks _ (AST.EType ann ts) = [AST.EType ann ts]
removeUselessBlocks isInner (AST.ETypeExtension gens ann var mems) = do
  let mems' = map (removeUselessBlocksExt isInner) mems
  [AST.ETypeExtension gens ann var mems']
removeUselessBlocks _ e = [e]

removeUselessBlocksExt :: (Bool, Bool) -> AST.ExtensionMember -> AST.ExtensionMember
removeUselessBlocksExt isInner (AST.ExtDeclaration gens n e) = do
  let e' = removeUselessBlocks isInner e
  case e' of
    [AST.EBlock es] -> AST.ExtDeclaration gens n (AST.EBlock es)
    [x] -> AST.ExtDeclaration gens n x
    _ -> AST.ExtDeclaration gens n (AST.EBlock e')

removeUselessBlocksIf :: (Bool, Bool) -> AST.Expression -> AST.Expression
removeUselessBlocksIf b e = do
  let bl = removeUselessBlocks b e

  case bl of
    [x]
      | isBlock x -> AST.EBlock $ getBlock x
      | isReturn x -> AST.EBlock bl
      | otherwise -> x
    _ -> AST.EBlock bl

  where removeLoc :: AST.Expression -> AST.Expression
        removeLoc (AST.ELocated e' _) = removeLoc e'
        removeLoc e' = e'

        isBlock :: AST.Expression -> Bool
        isBlock expr | AST.EBlock _ <- removeLoc expr = True
                     | otherwise = False

        getBlock :: AST.Expression -> [AST.Expression]
        getBlock (AST.EBlock es) = es
        getBlock e' = [e']

        isReturn :: AST.Expression -> Bool
        isReturn expr | AST.EReturn _ <- removeLoc expr = True
                      | otherwise = False

        -- getReturn :: AST.Expression -> AST.Expression
        -- getReturn (AST.EReturn e') = e'
        -- getReturn e' = e'