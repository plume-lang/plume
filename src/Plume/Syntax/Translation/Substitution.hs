module Plume.Syntax.Translation.Substitution where

import Data.Foldable
import Data.Set qualified as S
import Plume.Syntax.Abstract qualified as AST
import Plume.Syntax.Common qualified as AST hiding (Pattern)

-- | Free represents values that are not bound in a given expression.
-- | For instance, in the expression `let x = 1 in x + y`, `y` is a free variable
-- | because it is not bound by the `let` expression, but `x` is not a free 
-- | variable because it is bound by the `let` expression.
class Free a where
  ftv :: a -> Set Text

-- SOME FREE INSTANCES

instance Free (AST.Annotation a) where
  ftv (AST.Annotation t _ _) = S.singleton t.identifier

instance Free AST.Pattern where
  ftv (AST.PVariable t _) = S.singleton t
  ftv AST.PWildcard{} = S.empty
  ftv (AST.PLiteral _) = S.empty
  ftv (AST.PConstructor _ ps) = foldMap ftv ps
  ftv (AST.PList _ ps t) = foldMap ftv ps <> foldMap ftv t
  ftv (AST.PSpecialVar t _) = S.singleton t
  ftv (AST.PSlice p _) = S.singleton p

instance (Free a) => Free [a] where
  ftv = foldr (S.union . ftv) S.empty


-- | Substitute a variable by an expression in an expression
-- | Used to replace variables with expressions for instance in the
-- | macro preprocessor
substitute :: (Text, AST.Expression) -> AST.Expression -> AST.Expression
substitute (name, expr) (AST.EVariable n t)
  | n.identifier == name = expr
  | otherwise = AST.EVariable n t
substitute _ (AST.ELiteral l) = AST.ELiteral l
substitute (name, expr) (AST.EApplication e es) =
  AST.EApplication (substitute (name, expr) e) (map (substitute (name, expr)) es)
substitute (name, expr) (AST.EDeclaration g ann e me) =
  AST.EDeclaration
    g
    ann
    (substitute (name, expr) e)
    (fmap (substitute (name, expr)) me)
substitute (name, expr) (AST.EMutUpdate ann e me) =
  AST.EMutUpdate
    ann
    (substitute (name, expr) e)
    (fmap (substitute (name, expr)) me)
substitute (name, expr) (AST.EConditionBranch e1 e2 e3) =
  AST.EConditionBranch
    (substitute (name, expr) e1)
    (substitute (name, expr) e2)
    (substitute (name, expr) e3)
substitute (name, expr) (AST.EClosure anns t e isA)
  | name `S.notMember` ftv anns = AST.EClosure anns t (substitute (name, expr) e) isA
  | otherwise = AST.EClosure anns t e isA
substitute (name, expr) (AST.EBlock es) = AST.EBlock (map (substitute (name, expr)) es)
substitute (name, expr) (AST.ELocated e p) = AST.ELocated (substitute (name, expr) e) p
substitute (name, expr) (AST.ESwitch e ps) =
  AST.ESwitch
    (substitute (name, expr) e)
    (map proceed ps)
 where
  proceed (p, e')
    | name `S.notMember` ftv p = (p, substitute (name, expr) e')
    | otherwise = (p, e')
substitute (name, expr) (AST.EReturn e) = AST.EReturn (substitute (name, expr) e)
substitute (name, expr) (AST.ETypeExtension g ann (Just var) ems)
  | name /= var =
      AST.ETypeExtension g ann (Just var) (map (substituteExt (name, expr)) ems)
  | otherwise = AST.ETypeExtension g ann (Just var) ems
substitute (name, expr) (AST.ETypeExtension g ann Nothing ems) =
  AST.ETypeExtension g ann Nothing (map (substituteExt (name, expr)) ems)
substitute _ (AST.ENativeFunction fp n gens t st isStd) =
  AST.ENativeFunction fp n gens t st isStd
substitute _ (AST.EInterface ann gs ms) = AST.EInterface ann gs ms
substitute e (AST.EList es) = AST.EList (map (substitute e) es)
substitute _ (AST.EType ann ts) = AST.EType ann ts
substitute _ (AST.EVariableDeclare g n t) = AST.EVariableDeclare g n t
substitute _ (AST.ERequire e) = AST.ERequire e
substitute (name, expr) (AST.EInstanceAccess e i) =
  AST.EInstanceAccess (substitute (name, expr) e) i
substitute (name, expr) (AST.EInstanceDict n t es) =
  AST.EInstanceDict n t (map (substitute (name, expr)) es)
substitute _ (AST.EInstanceVariable n t) = AST.EInstanceVariable n t
substitute _ (AST.ETypeAlias ann t) = AST.ETypeAlias ann t
substitute (name, expr) (AST.EAwait e) = AST.EAwait (substitute (name, expr) e)

substituteExt
  :: (Text, AST.Expression)
  -> AST.ExtensionMember
  -> AST.ExtensionMember
substituteExt (name, expr) (AST.ExtDeclaration g ann e)
  | name `S.notMember` ftv ann =
      AST.ExtDeclaration g ann (substitute (name, expr) e)
  | otherwise = AST.ExtDeclaration g ann e

substituteMany :: [(Text, AST.Expression)] -> AST.Expression -> AST.Expression
substituteMany xs e = foldl (flip substitute) e xs

substituteManyBlock
  :: [(Text, AST.Expression)] -> [AST.Expression] -> [AST.Expression]
substituteManyBlock = map . substituteMany
