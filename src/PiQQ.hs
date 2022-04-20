module PiQQ (pico) where

import Data.Void (Void)
import Data.Either (fromRight)
import Text.Megaparsec ((<|>), MonadParsec(try), many, sepBy1, sepBy, Parsec, parse, errorBundlePretty, empty, eof, some, satisfy)
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space)
import qualified Text.Megaparsec.Char.Lexer as CL
import Language.Haskell.TH (mkName, unsafeCodeCoerce, varP, tupP, varE, appE, lamE, condE, listE, unboundVarE, Quote, Exp, ExpQ, tupE)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Lift(..))
import Language.Haskell.Meta.Parse (parseExp)
import Process (exec, inert, par, repl)
import Channel

{-
  Grammar:
  Parallel ::= Choice {"|" Choice}
  Choice ::= Prefix {"+" Prefix}
  Prefix ::= "exec" HaskellCode "." Prefix
           | "!" Prefix
           | "[" HaskellCode "]" Prefix
           | "new" Variable "." Prefix
           | Variable "<" (epsilon | HaskellCode {"," HaskellCode}) ">" "." Prefix
           | Variable "(" Variable ")" "." Prefix
           | Atom
  Atom ::= "(" Replication ")"
         | Variable {Variable}
         | "0"
-}

data ProcessTerm 
  = Parallel ProcessTerm ProcessTerm
  | Choice [ProcessTerm]
  | Exec String ProcessTerm
  | Replication ProcessTerm
  | Match String ProcessTerm
  | Restriction Variable ProcessTerm
  | Input Variable [Variable] ProcessTerm
  | Output Variable [String] ProcessTerm
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

haskell :: [Char] -> Parser String
haskell symbols = some $ satisfy (not . (`elem` symbols))

pProcessTerm :: Parser ProcessTerm
pProcessTerm = space *> pParallel <* eof

pParallel :: Parser ProcessTerm
pParallel = foldr1 Parallel <$> sepBy1 pChoice (symbol "|")

pChoice :: Parser ProcessTerm
pChoice = do
  terms <- sepBy1 pPrefix (symbol "+")
  case length terms of
    1 -> return $ head terms
    _ -> return $ Choice terms

pPrefix :: Parser ProcessTerm
pPrefix = pExec <|> pReplication <|> pMatch <|> pRestriction <|>  pInput <|> pOutput <|> pAtom

pExec :: Parser ProcessTerm
pExec = Exec <$> (keyword "exec" *> haskell "." <* symbol ".") <*> pPrefix

pReplication :: Parser ProcessTerm
pReplication = Replication <$> (symbol "!" *> pPrefix)

pMatch :: Parser ProcessTerm
pMatch = Match <$> (symbol "[" *> haskell "]" <* symbol "]") <*> pPrefix

pRestriction :: Parser ProcessTerm
pRestriction = Restriction <$> (keyword "new" *> variable <* symbol ".") <*> pPrefix

pInput :: Parser ProcessTerm
pInput = Input
  <$> try (variable <* symbol "(")
  <*> (sepBy variable (symbol ",") <* symbol ")" <* symbol ".")
  <*> pPrefix

pOutput :: Parser ProcessTerm
pOutput = Output
  <$> try (variable <* symbol "<")
  <*> (sepBy (haskell ",>") (symbol ",") <* symbol ">" <* symbol ".")
  <*> pPrefix

pAtom :: Parser ProcessTerm
pAtom = (symbol "(" *> pParallel <* symbol ")")
  <|> pFunApp
  <|> pInert

pFunApp :: Parser ProcessTerm
pFunApp = FunApp <$> variable <*> many variable

pInert :: Parser ProcessTerm
pInert = symbol "0" >> return Inert


pico :: QuasiQuoter
pico = QuasiQuoter
  { quoteExp = compile
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

haskellStrToExp :: Quote m => String -> m Exp
haskellStrToExp s = case parseExp s of
  Left err -> error err
  Right arg' -> pure arg'

liftFunApplication :: Quote m => Variable -> [Variable] -> m Exp
liftFunApplication fun args = foldl appE (varE (mkName fun)) (map haskellStrToExp args)

instance Lift ProcessTerm where
  lift (Parallel t1 t2) = [| par $(lift t1) $(lift t2) |]
  lift (Choice ts) = [| chooseMulti $(chans) $(funs) |]
    where
      chans = listE [unboundVarE (mkName chan) | Input chan _ _ <- ts]
      funs = listE [lamE [tupP $ map (varP . mkName) vars] (lift t) | Input _ vars t <- ts]
  lift (Exec expr t) = [| exec $(haskellStrToExp expr) $(lift t) |]
  lift (Replication t) = [| repl $(lift t) |]
  lift (Match cond t) = [| if $(haskellStrToExp cond) then $(lift t) else inert |]
  lift (Restriction v t) = [| $(unboundVarE (mkName "new")) $(lamE [varP (mkName v)] (lift t)) |]
  lift (Input chan vars t) = [| recv $(unboundVarE (mkName chan)) $(lamE [tupP $ map (varP . mkName) vars] (lift t)) |]
  lift (Output chan args t) = [| send $(unboundVarE (mkName chan)) $(tupE $ map haskellStrToExp args) $(lift t) |]
  lift (FunApp v vs) = liftFunApplication v vs
  lift Inert = [| inert |]
  
  liftTyped = unsafeCodeCoerce . lift