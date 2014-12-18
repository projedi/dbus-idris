module XML.Printer

import XML

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
