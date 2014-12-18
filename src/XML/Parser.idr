module XML.Parser

import Parser
import XML

-- From: http://www.w3.org/TR/2008/REC-xml-20081126/

document : Parser XML
document = do
  p <- prolog
  root <- element
  rest <- many misc
  ?document_rhs

char' : Parser Char
char' = choice
  [ char '\x9'
  , char '\xA'
  , char '\xD'
  , anyOf ['\x20'..'\xD7FF']
  , anyOf ['\xE000'..'\xFFFD']
  , anyOf ['\x10000'..'\x10FFFF']
  ]

s : Parser String
s = many1 $ choice
  [ char '\x20'
  , char '\x9'
  , char '\xD'
  , char '\xA'
  ]

nameStartChar : Parser Char
nameStartChar = choice
  [ char ':'
  , anyOf ['A'..'Z']
  , char '_'
  , anyOf ['a'..'z']
  , anyOf ['\xC0'..'\xD6']
  , anyOf ['\xD8'..'\xF6']
  , anyOf ['\xF8'..'\x2FF']
  , anyOf ['\x370'..'\x37D']
  , anyOf ['\x37F'..'\x1FFF']
  , anyOf ['\x200C'..'\x200D']
  , anyOf ['\x2070'..'\x218F']
  , anyOf ['\x2C00'..'\x2FEF']
  , anyOf ['\x3001'..'\xD7FF']
  , anyOf ['\xF900'..'\xFDCF']
  , anyOf ['\xFDF0'..'\xFFFD']
  , anyOf ['\x10000'..'\xEFFFF']
  ]

nameChar : Parser Char
nameChar = choice
  [ nameStartChar
  , char '-'
  , char '.'
  , anyChar ['0'..'9']
  , char '\xB7'
  , anyChar ['\x0300'..'\x036F']
  , anyChar ['\x203F'..'\x2040']
  ]

name : Parser String
name = pure pack <$> [| nameStartChar :: (many nameChar) |]

names : Parser (List String)
names = [| name :: (many (char '\x20' $> name)) |]

nmtoken : Parser String
nmtoken = pure pack <$> many1 nameChar

nmtokens : Parser (List String)
nmtokens = [| nmtoken :: (many (char '\x20' $> nmtoken)) |]

entityValue : Parser Value
entityValue = choice
  [ char '"' $> many (noneOf ['%', '&', '"'] <|> pEReference <|> reference) <$ char '"'
  , char ''' $> many (noneOf ['%', '&', '''] <|> pEReference <|> reference) <$ char '''
  ]

attValue : Parser Value
attValue = choice
  [ char '"' $> many (noneOf ['<', '&', '"'] <|> reference) <$ char '"'
  , char ''' $> many (noneOf ['<', '&', '''] <|> reference) <$ char '''
  ]

systemLiteral : Parser String
systemLiteral = choice
  [ char '"' $> many (noneOf ['"']) <$ char '"'
  , char ''' $> many (noneOf [''']) <$ char '''
  ]

pubidChar : Parser Char
pubidChar = choice
  [ char '\x20'
  , char '\xD'
  , char '\xA'
  , oneOf ['a'..'z']
  , oneOf ['A'..'Z']
  , oneOf ['0'..'9']
  , oneOf ['-', ''', '(', ')', '+', ',', '.', '/', ':', '=', '?', ';', '!', '*', '#', '@', '$', '_', '%']
  ]

pubidLiteral : Parser String
pubidLiteral = pure pack $ choice
  [ char '"' $> many pubidChar <$ char '"'
  , char ''' $> many (pubidChar `excluding` char ''') <$ char '''
  ]

charData : Parser String
charData = many (noneOf ['<', '&']) `excluding`
  (many (noneOf ['<', '&']) $> string "]]>" $> many (noneOf ['<', '&']))

comment : Parser String
comment =  pure pack $ string "<!--" $> go <$ string "-->"
 where go : Parser (List Char)
       go = do
         c <- anyChar
         if c == '-'
            then [| pure c :: (char' `excluding` char '-') :: go |]
            else [| pure c :: go |]

pI : Parser ProcessingInstruction
pI = do
  string "<?"
  target <- pITarget
  data_ <- optional (s $> (many char' `excluding` (many char' $> "?>" $> many char')))
  string "?>"
  pure $ MkProcessingInstruction
    { target = target
    , data_ = data_
    }

pITarget : Parser String
pITarget = name `excluding` (oneOf ['x', 'X'] $> oneOf ['m', 'M'] $> oneOf ['l', 'L'])

cDStart : Parser String
cDStart = string "<![CDATA["

cData : Parser String
cData = many char' `excluding` (many char' $> "]]>" $> many char')

cDEnd : Parser String
cDEnd = string "]]>"

cDSect : Parser String
cDSect = cDStart $> cData <$ cDEnd

public record Prolog : Type where
  MkProlog
    :  (version : Maybe Version)
    -> (encoding : Maybe Encoding)
    -> (sd : Maybe SD)
    -> (doctype : Maybe Doctype)
    -> (processingInstructions : List ProcessingInstruction)
    -> Prolog

prolog : Parser Prolog
prolog = do
  d1 <- optional xMLDecl
  m1 <- many misc
  d2 <- optional [| (doctypedecl, many misc) |]
  pure $ MkProlog
    { version = d1 >>= (\(v, _, _) => pure v)
    , encoding = d1 >>= (\(_, e, _) => pure e)
    , sd = d1 >>= (\(_, _, s) => pure s)
    , doctype = d2 >>= (\(d, _) => pure d)
    , processingInstructions =
        case d2 of
             Nothing => catMaybes m1
             Just (_, m2) => catMaybes m1 ++ catMaybes m2
    }

xMLDecl : Parser (Version, Encoding, SD)
xMLDecl = do
  string "<?xml"
  v <- versionInfo
  enc <- optional encodingDecl
  sd <- optional sDDecl
  optional s
  string "?>"
  pure (v, enc, sd)

versionInfo : Parser Version
versionInfo = s $> string "version" $> eq $>
              ((char ''' $> versionNum <$ char ''') <|> (char '"' $> versionNum <$ char '"'))

eq : Parser ()
eq = optional s $> string '=' $> optional s $> pure ()

versionNum : Parser Version
versionNum = [| string "1." ++ pure pack (many1 (oneOf ['0'..'9'])) |]

misc : Parser (Maybe ProcessingInstruction)
misc = choice
  [ comment $> pure Nothing
  , Just <$> pI
  , s $> pure Nothing
  ]

-- Validity: Name must match the name of root element
-- Validity: 
doctypedecl : Parser Doctype
doctypedecl = do
  string "<!DOCTYPE"
  s
  n <- name
  eid <- optional (s $> externalID)
  optional s
  isubset <- optional (char '[' $> intSubset <$ char ']' <$ optional s)
  pure $ MkDoctype name eid isubset

declSep : Parser (Maybe PEReference)
declSep = choice
  [ Just <$> pEReference
  , s $> pure Nothing
  ]

intSubset : Parser (List (Either PEReference Markup))
intSubset = choice
  [ do m <- markupdecl
       ((Right m) ::) <$> intSubset
  , do d <- declSep
       case d of
            Just r => ((Left r) ::) <$> intSubset
            Nothing => intSubset
  ]

markupdecl : Parser Markup
markupdecl = elementdecl <|> attlistDecl <|> entityDecl <|> notationDecl <|> pI <|> comment

extSubset : Parser ExternalSubset

extSubsetDecl : Parser ?

sdDecl

public parseXML : String -> Maybe XML
parseXML = parse (document $> eof)
