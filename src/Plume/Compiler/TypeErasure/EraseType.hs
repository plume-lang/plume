module Plume.Compiler.TypeErasure.EraseType where

import Plume.Compiler.ClosureConversion.Syntax qualified as Pre
import Plume.Compiler.TypeErasure.DynamicDispatch.BundleExtensions
import Plume.Compiler.TypeErasure.DynamicDispatch.Dispatch
import Plume.Compiler.TypeErasure.Syntax qualified as Post

eraseType :: [Pre.ClosedProgram] -> [Post.UntypedProgram]
eraseType (Pre.CPFunction n args body : xs) = [Post.UPFunction n args (eraseStatement body)] <> eraseType xs
eraseType (Pre.CPStatement s : xs) = [Post.UPStatement (eraseStatement s)] <> eraseType xs
eraseType (Pre.CPNativeFunction n a : xs) = [Post.UPNativeFunction n a] <> eraseType xs
eraseType ext@(Pre.CPExtFunction _ name x _ : _) = do
  let (exts, rest) = bundleExtensions name ext
  let prog = dispatch (name, x) exts
  eraseType (prog : rest)
eraseType [] = []

eraseStatement :: Pre.ClosedStatement -> Post.UntypedStatement
eraseStatement (Pre.CSExpr e) = Post.USExpr (eraseExpr e)
eraseStatement (Pre.CSReturn e) = Post.USReturn (eraseExpr e)
eraseStatement (Pre.CSDeclaration n e) = Post.USDeclaration n (eraseExpr e)
eraseStatement (Pre.CSConditionBranch e1 e2 e3) = Post.USConditionBranch (eraseExpr e1) (eraseStatement e2) (eraseStatement e3)

eraseExpr :: Pre.ClosedExpr -> Post.UntypedExpr
eraseExpr (Pre.CEVar x) = Post.UEVar x
eraseExpr (Pre.CEApplication f args) = Post.UEApplication (eraseExpr f) (map eraseExpr args)
eraseExpr (Pre.CELiteral l) = Post.UELiteral l
eraseExpr (Pre.CEList es) = Post.UEList (map eraseExpr es)
eraseExpr (Pre.CEDeclaration n e1 e2) = Post.UEDeclaration n (eraseExpr e1) (eraseExpr e2)
eraseExpr (Pre.CEConditionBranch e1 e2 e3) = Post.UEConditionBranch (eraseExpr e1) (eraseExpr e2) (eraseExpr e3)
eraseExpr (Pre.CESwitch e cases) =
  Post.UESwitch
    (eraseExpr e)
    (map (bimap erasePattern eraseExpr) cases)
eraseExpr (Pre.CEDictionary es) = Post.UEDictionary (fmap eraseExpr es)
eraseExpr (Pre.CEProperty e i) = Post.UEProperty (eraseExpr e) i
eraseExpr (Pre.CETypeOf e) = Post.UETypeOf (eraseExpr e)
eraseExpr (Pre.CEBlock es) = Post.UEBlock (map eraseStatement es)

erasePattern :: Pre.ClosedPattern -> Post.UntypedPattern
erasePattern (Pre.CPVariable x) = Post.UPVariable x
erasePattern (Pre.CPLiteral l) = Post.UPLiteral l
erasePattern (Pre.CPConstructor n ps) = Post.UPConstructor n (map erasePattern ps)
erasePattern Pre.CPWildcard = Post.UPWildcard