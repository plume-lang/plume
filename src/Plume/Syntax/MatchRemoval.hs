module Plume.Syntax.MatchRemoval where

import Plume.Syntax.Abstract as AST

manageLetMatches :: [AST.Expression] -> [AST.Expression]
manageLetMatches (AST.ELetMatch p e : es) = [AST.ESwitch (manageLetMatch e) [(p, AST.EBlock $ manageLetMatches es)]]
manageLetMatches (AST.EBlock xs : es) = AST.EBlock (manageLetMatches xs) : manageLetMatches es
manageLetMatches (AST.ETypeExtension xs x t ys : es) = AST.ETypeExtension xs x t (manageLetMatchesE ys) : manageLetMatches es
manageLetMatches (AST.ELocated e p : es) = AST.ELocated <$> manageLetMatches (e : es) <*> pure p
manageLetMatches (AST.ESwitch x xs : es) = AST.ESwitch (manageLetMatch x) (map (second manageLetMatch) xs) : manageLetMatches es
manageLetMatches (AST.EReturn x : es) = AST.EReturn (manageLetMatch x) : manageLetMatches es
manageLetMatches (AST.EConditionBranch x y z : es) = AST.EConditionBranch (manageLetMatch x) (manageLetMatch y) (fmap manageLetMatch z) : manageLetMatches es
manageLetMatches (AST.EWhile x y : es) = AST.EWhile (manageLetMatch x) (manageLetMatch y) : manageLetMatches es
manageLetMatches (AST.EInterface x xs ys d : es) = AST.EInterface x xs ys d : manageLetMatches es
manageLetMatches (AST.ENativeFunction x y xs z t isStd : es) = AST.ENativeFunction x y xs z t isStd : manageLetMatches es
manageLetMatches (AST.EType x xs : es) = AST.EType x xs : manageLetMatches es
manageLetMatches (AST.EVariableDeclare x y z : es) = AST.EVariableDeclare x y z : manageLetMatches es
manageLetMatches (AST.ERequire x : es) = AST.ERequire x : manageLetMatches es
manageLetMatches (AST.EInstanceAccess x i : es) = AST.EInstanceAccess (manageLetMatch x) i : manageLetMatches es
manageLetMatches (AST.EInstanceDict x t xs : es) = AST.EInstanceDict x t (map manageLetMatch xs) : manageLetMatches es
manageLetMatches (AST.EInstanceVariable x t : es) = AST.EInstanceVariable x t : manageLetMatches es
manageLetMatches (AST.ETypeAlias x t : es) = AST.ETypeAlias x t : manageLetMatches es
manageLetMatches (AST.EAwait x : es) = AST.EAwait (manageLetMatch x) : manageLetMatches es
manageLetMatches (AST.EMutUpdate x y z : es) = AST.EMutUpdate x (manageLetMatch y) (fmap manageLetMatch z) : manageLetMatches es
manageLetMatches (AST.EList xs : es) = AST.EList (map manageLetMatch xs) : manageLetMatches es
manageLetMatches (AST.EApplication x xs : es) = AST.EApplication (manageLetMatch x) (map manageLetMatch xs) : manageLetMatches es
manageLetMatches (AST.EVariable x y : es) = AST.EVariable x y : manageLetMatches es
manageLetMatches (AST.ELiteral x : es) = AST.ELiteral x : manageLetMatches es
manageLetMatches (AST.EClosure x y z a : es) = AST.EClosure x y (manageLetMatch z) a : manageLetMatches es
manageLetMatches (AST.EDeclaration x y z zs : es) = AST.EDeclaration x y (manageLetMatch z) (fmap manageLetMatch zs) : manageLetMatches es
manageLetMatches (e : es) = e : manageLetMatches es
manageLetMatches [] = []

manageLetMatchesE :: [AST.ExtensionMember] -> [AST.ExtensionMember]
manageLetMatchesE (AST.ExtDeclaration xs x y : es) = AST.ExtDeclaration xs x (manageLetMatch y) : manageLetMatchesE es
manageLetMatchesE [] = []

manageLetMatch :: AST.Expression -> AST.Expression
manageLetMatch (AST.EBlock xs) = AST.EBlock $ manageLetMatches xs
manageLetMatch e = case manageLetMatches [e] of
  [x] -> x
  _ -> error "manageLetMatch: impossible"