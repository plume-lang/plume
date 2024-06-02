module Plume.Compiler.Javascript.Translate where

import Plume.Compiler.Javascript.Syntax qualified as Post
import Plume.Compiler.Desugaring.Syntax qualified as Pre
import Plume.Compiler.ClosureConversion.Syntax qualified as Pre
import Plume.Syntax.Common.Literal qualified as Cmm
import Data.IntMap qualified as IntMap

class Assemble a b where
  assemble :: a -> b

instance Assemble Pre.DesugaredProgram Post.Program where
  assemble (Pre.DPDeclaration name expr) = Post.Program [Post.JSVariableDeclaration name (assemble expr)]
  assemble (Pre.DPFunction name args body isAsync) = Post.Program [fun name args (assemble body)]
    where fun = if isAsync then Post.JSAsyncFunction else Post.JSFunction
  assemble (Pre.DPMutDeclaration name expr) = Post.Program [Post.JSVariableDeclaration name (assemble expr)]
  assemble (Pre.DPMutUpdate name expr) = Post.Program [Post.JSUpdate (assemble name) (assemble expr)]
  assemble (Pre.DPStatement st) = Post.Program [assemble st]
  assemble e@(Pre.DPNativeFunction {}) = Post.Program (assembleNative e)
  assemble (Pre.DPDeclare {}) = mempty

createStandardPath :: Text -> Post.Expression
createStandardPath name = Post.JSBinary "+" path (Post.JSLiteral (Cmm.LString ("/" <> name)))
  where 
    path = Post.JSMember (Post.JSMember (Post.JSIdentifier "process") "env") "PLUME_PATH"

assembleNative :: Pre.DesugaredProgram -> [Post.Statement]
assembleNative (Pre.DPNativeFunction lib name _ isStd) = do
  [Post.JSVariableDeclaration name nameCall]
  where
    requireCall = Post.JSCall (Post.JSIdentifier "require") [
        if isStd then 
          createStandardPath lib 
        else Post.JSLiteral (Cmm.LString lib)
      ]
    nameCall = Post.JSMember requireCall name
assembleNative _ = []

instance Assemble Pre.DesugaredStatement Post.Statement where
  assemble (Pre.DSDeclaration name expr) = Post.JSVariableDeclaration name (assemble expr)
  assemble (Pre.DSMutDeclaration name expr) = Post.JSVariableDeclaration name (assemble expr)
  assemble (Pre.DSMutUpdate name expr) = Post.JSUpdate (assemble name) (assemble expr)
  assemble (Pre.DSReturn e) = Post.JSReturn (assemble e)
  assemble (Pre.DSExpr e) = Post.JSExpression (assemble e)
  assemble (Pre.DSIf e1 e2 e3) = Post.JSIfStatement (assemble e1) (assemble e2) (Just $ assemble e3)

doesContainReturn :: [Pre.DesugaredStatement] -> Bool
doesContainReturn = any isReturn
  where
    isReturn (Pre.DSReturn _) = True
    isReturn _ = False

blockToExpr :: [Pre.DesugaredStatement] -> Post.Expression
blockToExpr [Pre.DSExpr e] = assemble e
blockToExpr [Pre.DSReturn e] = assemble e
blockToExpr [] = Post.JSNull
blockToExpr xs = Post.JSCall (Post.JSAnnFunction [] (assemble xs)) []

instance Assemble Pre.DesugaredExpr Post.Expression where
  assemble (Pre.DEAnd e1 e2) = Post.JSBinary "&&" (assemble e1) (assemble e2)
  assemble (Pre.DEApplication "wait" [e]) = Post.JSAwait (assemble e)
  assemble (Pre.DEApplication f args) = Post.JSCall (Post.JSIdentifier f) (assemble args)
  assemble (Pre.DEDictionary fields) = Post.JSObject (assemble fields)
  assemble (Pre.DEEqualsTo e1 e2) = Post.JSBinary "===" (assemble e1) (assemble e2)
  assemble (Pre.DEIf e1 e2 e3) = Post.JSTernary (assemble e1) e2' e3'
    where
      e2' = blockToExpr e2
      e3' = blockToExpr e3
  assemble (Pre.DELiteral l) = Post.JSLiteral l
  assemble (Pre.DEVar x) = Post.JSIdentifier x
  assemble (Pre.DEList es) = Post.JSArray (assemble es)
  assemble (Pre.DEIndex e i) = Post.JSArrayIndex (assemble e) (assemble i)
  assemble (Pre.DEProperty name i) = Post.JSMember (assemble name) (show i)
  assemble Pre.DESpecial = Post.JSNull
  assemble (Pre.DESlice e i) = Post.JSSlice (assemble e) (toInteger i)
  assemble (Pre.DEGreaterThan e1 e2) = Post.JSBinary ">" (assemble e1) (Post.JSLiteral (Cmm.LInt (toInteger e2)))
  assemble (Pre.DEListLength e) = Post.JSMember (assemble e) "length"
  assemble (Pre.DEUnMut x) = Post.JSMember (assemble x) "value"
  assemble _ = error "Not implemented"

instance Assemble a b => Assemble [a] [b] where
  assemble = fmap assemble

instance Assemble Pre.Update Post.Update where
  assemble (Pre.UProperty name field) = Post.JSFieldUpdate field (assemble name)
  assemble (Pre.UVariable name) = Post.JSVariable name

instance Assemble a b => Assemble (IntMap a) [(Post.Field, b)] where
  assemble m = IntMap.toList m >>= \(i, x) -> [(show i, assemble x)]

runTranslateJS :: [Pre.DesugaredProgram] -> Post.Program
runTranslateJS = mconcat . fmap assemble

createMainJSApp :: Post.Program -> Text
createMainJSApp (Post.Program stmts) = show (Post.JSCall (Post.JSAsyncAnnFunction [] stmts) [])