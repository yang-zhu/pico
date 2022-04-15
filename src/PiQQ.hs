module PiQQ (pico) where

import Data.Void (Void)
import Data.Either (fromRight)
import Text.Megaparsec ((<|>), MonadParsec(try), many, sepBy1, Parsec, parse, errorBundlePretty, empty, eof, some)
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space)
import qualified Text.Megaparsec.Char.Lexer as CL
import Language.Haskell.TH (mkName, unsafeCodeCoerce, varP, varE, appE, lamE, unboundVarE, Quote, Exp, ExpQ)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Lift(..))
import Process (exec, inert, par, repl)
import Channel

{-
  Grammar:
  Restriction ::= "new" Variable "." Restriction | Parallel
  Parallel ::= Replication {"|" Replication}
  Choice ::= Replication ["+" Replication]
  Replication ::= "!" InputOutput | InputOutput
  InputOutput ::= Variable "<" Variable ">" "." Replication
                | Variable "(" Variable ")" "." Replication
                | Atom
  Atom ::= "(" Replication ")" 
         | "exec" Variable {Variable} "." InputOutput
         | Variable {Variable}
         | "0"
-}

data ProcessTerm 
  = Restriction Variable ProcessTerm
  | Parallel ProcessTerm ProcessTerm
  | Choice ProcessTerm ProcessTerm
  | Replication ProcessTerm
  | Input Variable Variable ProcessTerm
  | Output Variable Variable ProcessTerm
  | Exec [Variable] ProcessTerm
  | FunApp Variable [Variable]
  | Inert

type Variable = String

type Parser a = Parsec Void String a

lexeme :: Parser a -> Parser a
lexeme = CL.lexeme space

symbol :: String -> Parser String
symbol = CL.symbol space 

keyword :: String -> Parser String
keyword s = try do
  name <- variable
  if name == s then return name else empty

variable :: Parser String
variable = lexeme $ (:) <$> letterChar <*> many identifierChar 
  where
    identifierChar :: Parser Char
    identifierChar = alphaNumChar <|> char '\''

pProcessTerm :: Parser ProcessTerm
pProcessTerm = space *> pRestriction <* eof

pRestriction :: Parser ProcessTerm
pRestriction = Restriction <$> (keyword "new" *> variable <* symbol ".") <*> pParallel
  <|> pParallel

pParallel :: Parser ProcessTerm
pParallel = foldr1 Parallel <$> sepBy1 pReplication (symbol "|")

pChoice :: Parser ProcessTerm
pChoice = do
  term <- pReplication
  Choice term <$> (symbol "+" *> pReplication)
   <|> return term

pReplication :: Parser ProcessTerm
pReplication = Replication <$> (symbol "!" *> pInputOutput)
  <|> pInputOutput

pInputOutput ::Parser ProcessTerm
pInputOutput = pInput <|> pOutput <|> pAtom

pInput :: Parser ProcessTerm
pInput = Input
  <$> try (variable <* symbol "(")
  <*> (variable <* symbol ")" <* symbol ".")
  <*> pReplication

pOutput :: Parser ProcessTerm
pOutput = Output
  <$> try (variable <* symbol "<")
  <*> (variable <* symbol ">" <* symbol ".")
  <*> pReplication

pAtom :: Parser ProcessTerm
pAtom = (symbol "(" *> pReplication <* symbol ")")
  <|> pExec
  <|> pFunApp
  <|> pInert

pExec :: Parser ProcessTerm
pExec = Exec <$> (keyword "exec" *> some variable <* symbol ".") <*> pInputOutput

pFunApp :: Parser ProcessTerm
pFunApp = FunApp <$> variable <*> many variable

pInert :: Parser ProcessTerm
pInert = symbol "0" >> return Inert


pico :: QuasiQuoter
pico = QuasiQuoter { quoteExp = compile
                 , quotePat  = notHandled "patterns"
                 , quoteType = notHandled "types"
                 , quoteDec  = notHandled "declarations"
                 }
  where
    notHandled things = error $ things ++ " not handled by pico quasiquoter."

    compile :: String -> ExpQ
    compile input = case parse pProcessTerm "" input of
      Left e -> error $ errorBundlePretty e
      Right ast -> lift ast 

liftFunApplication :: Quote m => Variable -> [Variable] -> m Exp
liftFunApplication v vs = foldl appE (varE (mkName v)) (map (varE . mkName) vs)

instance Lift ProcessTerm where
  lift (Restriction v t) = [| $(unboundVarE (mkName "new")) $(lamE [varP (mkName v)] (lift t)) |]
  lift (Parallel t1 t2) = [| par $(lift t1) $(lift t2) |]
  lift (Choice (Input chan1 msg1 t1) (Input chan2 msg2 t2)) = [| choose $(unboundVarE (mkName chan1)) $(lamE [varP (mkName msg1)] (lift t1)) $(unboundVarE (mkName chan2)) $(lamE [varP (mkName msg2)] (lift t2)) |]
  lift (Replication t) = [| repl $(lift t) |]
  lift (Input chan msg t) = [| recv $(unboundVarE (mkName chan)) $(lamE [varP (mkName msg)] (lift t)) |]
  lift (Output chan msg t) = [| send $(unboundVarE (mkName chan)) $(unboundVarE (mkName msg)) $(lift t) |]
  lift (Exec (v:vs) t) = [| exec $(liftFunApplication v vs) $(lift t) |]
  lift (FunApp v vs) = liftFunApplication v vs
  lift Inert = [| inert |]
  lift _ = error "invalid input"
  
  liftTyped = unsafeCodeCoerce . lift