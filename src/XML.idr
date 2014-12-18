module XML

%access private

public record XMLNode : Type where
  MkXMLNode
    :  (nodeName : String)
    -> (nodeProperties : List (String, String))
    -> (nodeChildren : List XMLNode)
    -> XMLNode

ppProperties : List (String, String) -> String
ppProperties [] = ""
ppProperties ((pname, pval) :: ps) =
  " " ++ pname ++ " = \"" ++ pval ++ "\"" ++ ppProperties ps

unlines : List String -> String
unlines [] = ""
unlines (x :: xs) = x ++ "\n" ++ unlines xs

public showOneNode : XMLNode -> String
showOneNode node =  "<" ++ nodeName node
                 ++ ppProperties (nodeProperties node)
                 ++ ">"

ppXML : String -> XMLNode -> String
ppXML strPrefix node =  strPrefix ++ showOneNode node ++ "\n"
                     ++ unlines (map (ppXML ("   " ++ strPrefix)) (nodeChildren node))
                     ++ strPrefix ++ "</" ++ nodeName node ++ ">"

instance Show XMLNode where
  show = ppXML ""

public record XML : Type where
  MkXML
    :  (version : Maybe Version)
    -> (encoding : Maybe Encoding)
    -> (sd : Maybe SD)
    -> (doctype : Maybe Doctype)
    -> (processingInstructions : List ProcessingInstruction)
    -> (root : XMLNode)
    -> XML

public Value : Type

public record ProcessingInstruction : Type where
  MkProcessingInstruction
    :  (target : String
    -> (data_ : Maybe String)
    -> ProcessingInstruction

public Doctype : Type

public Version : Type
Version = String

public Encoding : Type

public SD : Type
