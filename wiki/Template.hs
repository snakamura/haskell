module Template (Template(),
                 loadTemplate,
                 evalTemplate)
    where

import Text.ParserCombinators.Parsec

import HTMLUtil

data Template = Template String

type Variable = (String, String)
type Variables = [Variable]

loadTemplate :: String -> IO Template
loadTemplate path = do s <- readFile path
                       return $ Template s

evalTemplate :: Template -> Variables -> String
evalTemplate (Template s) vars =
    case parse parser "" s of
         Left err  -> error $ show err
         Right str -> str
    where
        parser = many oneParser >>= return . concat
        oneParser =     many1 (noneOf "$")
                    <|> try variable
                    <|> try escapedVariable
                    <|> try urlVariable
                    <|> do char '$'
                           return "$"
        variable = do string "${!:"
                      name <- variableName
                      char '}'
                      return $ lookupVariable name
        escapedVariable = do string "${"
                             name <- variableName
                             char '}'
                             return $ escapeHtml $ lookupVariable name
        urlVariable = do string "${u:"
                         name <- variableName
                         char '}'
                         return $ escapeHtml $ encodeURLComponent $ lookupVariable name
        variableName = many1 alphaNum
        lookupVariable :: String -> String
        lookupVariable = getVariable vars

getVariable :: Variables -> String -> String
getVariable vars name = case lookup name vars of
                             Just v  -> v
                             Nothing -> ""
