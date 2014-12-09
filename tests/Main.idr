module Main

import System

import XML.Parser

-- TODO: The order of nodes must matter, properties - should not

compareXMLNode : XMLNode -> XMLNode -> Either String ()
compareXMLNode node1 node2 = do
  if nodeName node1 /= nodeName node2
     then Left $  "Name mismatch: "
               ++ showOneNode node1 ++ " vs " ++ showOneNode node2
     else if not (compareProperties (nodeProperties node1) (nodeProperties node2))
             then Left $  "Property mismatch: "
                       ++ showOneNode node1 ++ " vs " ++ showOneNode node2
             else compareChildren (nodeChildren node1) (nodeChildren node2)
 where compareProperties : List (String, String) -> List (String, String) -> Bool
       compareProperties [] [] = True
       compareProperties ((pn1, pv1) :: ps1) ((pn2, pv2) :: ps2) =
         if pn1 == pn2 && pv1 == pv2
            then compareProperties ps1 ps2
            else False
       compareProperties _ _ = False
       compareChildren : XML -> XML -> Either String ()
       compareChildren [] [] = return ()
       compareChildren (c1 :: cs1) (c2 :: cs2) =
         case compareXMLNode c1 c2 of
              Left err =>
                Left $ showOneNode node1 ++ " -> " ++ err
              Right _ => compareChildren cs1 cs2
       compareChildren _ _ = Left "Children count mismatch"

compareXML : XML -> XML -> Either String ()
compareXML [] [] = return ()
compareXML (n1 :: ns1) (n2 :: ns2) = do
  compareXMLNode n1 n2
  compareXML ns1 ns2
compareXML _ _ = Left "Node count mismatch on top level"

xmlParserTest : String -> XML -> IO ()
xmlParserTest fname xml = do
  putStrLn $ "Checking " ++ fname
  case parseXML !(readFile fname) of
       Just pxml => do
         case compareXML xml pxml of
              Left err => do
                putStrLn $ "ERROR: XML do not match: " ++ err
              Right _ => putStrLn "OK"
       Nothing => putStrLn $ "ERROR: Unable to parse XML"

xmlParserTests : IO ()
xmlParserTests = do
  xmlParserTest "xml/test1.xml"
    [ MkXMLNode "node" []
      [ MkXMLNode "interface" [("name", "org.freedesktop.DBus")]
        [ MkXMLNode "method" [("name", "Hello")]
          [ MkXMLNode "arg" [("direction", "out"), ("type", "s")] []
          ]
        , MkXMLNode "method" [("name", "RequestName")]
          [ MkXMLNode "arg" [("direction", "in"), ("type", "s")] []
          , MkXMLNode "arg" [("direction", "in"), ("type", "u")] []
          , MkXMLNode "arg" [("direction", "out"), ("type", "u")] []
          ]
        , MkXMLNode "method" [("name", "ReleaseName")]
          [ MkXMLNode "arg" [("direction", "in"), ("type", "s")] []
          , MkXMLNode "arg" [("direction", "out"), ("type", "u")] []
          ]
        , MkXMLNode "method" [("name", "StartServiceByName")]
          [ MkXMLNode "arg" [("direction", "in"), ("type", "s")] []
          , MkXMLNode "arg" [("direction", "in"), ("type", "u")] []
          , MkXMLNode "arg" [("direction", "out"), ("type", "u")] []
          ]
        , MkXMLNode "method" [("name", "UpdateActivationEnvironment")]
          [ MkXMLNode "arg" [("direction", "in"), ("type", "a{ss}")] []
          ]
        , MkXMLNode "method" [("name", "NameHasOwner")]
          [ MkXMLNode "arg" [("direction", "in"), ("type", "s")] []
          , MkXMLNode "arg" [("direction", "out"), ("type", "b")] []
          ]
        , MkXMLNode "method" [("name", "ListNames")]
          [ MkXMLNode "arg" [("direction", "out"), ("type", "as")] []
          ]
        , MkXMLNode "method" [("name", "ListActivatableNames")]
          [ MkXMLNode "arg" [("direction", "out"), ("type", "as")] []
          ]
        , MkXMLNode "method" [("name", "AddMatch")]
          [ MkXMLNode "arg" [("direction", "in"), ("type", "s")] []
          ]
        , MkXMLNode "method" [("name", "RemoveMatch")]
          [ MkXMLNode "arg" [("direction", "in"), ("type", "s")] []
          ]
        , MkXMLNode "method" [("name", "GetNameOwner")]
          [ MkXMLNode "arg" [("direction", "in"), ("type", "s")] []
          , MkXMLNode "arg" [("direction", "out"), ("type", "s")] []
          ]
        , MkXMLNode "method" [("name", "ListQueuedOwners")]
          [ MkXMLNode "arg" [("direction", "in"), ("type", "s")] []
          , MkXMLNode "arg" [("direction", "out"), ("type", "as")] []
          ]
        , MkXMLNode "method" [("name", "GetConnectionUnixUser")]
          [ MkXMLNode "arg" [("direction", "in"), ("type", "s")] []
          , MkXMLNode "arg" [("direction", "out"), ("type", "u")] []
          ]
        , MkXMLNode "method" [("name", "GetConnectionUnixProcessID")]
          [ MkXMLNode "arg" [("direction", "in"), ("type", "s")] []
          , MkXMLNode "arg" [("direction", "out"), ("type", "u")] []
          ]
        , MkXMLNode "method" [("name", "GetAdtAuditSessionData")]
          [ MkXMLNode "arg" [("direction", "in"), ("type", "s")] []
          , MkXMLNode "arg" [("direction", "out"), ("type", "ay")] []
          ]
        , MkXMLNode "method" [("name", "GetConnectionSELinuxSecurityContext")]
          [ MkXMLNode "arg" [("direction", "in"), ("type", "s")] []
          , MkXMLNode "arg" [("direction", "out"), ("type", "ay")] []
          ]
        , MkXMLNode "method" [("name", "ReloadConfig")] []
        , MkXMLNode "method" [("name", "GetId")]
          [ MkXMLNode "arg" [("direction", "out"), ("type", "s")] []
          ]
        , MkXMLNode "method" [("name", "GetConnectionCredentials")]
          [ MkXMLNode "arg" [("direction", "in"), ("type", "s")] []
          , MkXMLNode "arg" [("direction", "out"), ("type", "a{sv}")] []
          ]
        , MkXMLNode "signal" [("name", "NameOwnerChanged")]
          [ MkXMLNode "arg" [("type", "s")] []
          , MkXMLNode "arg" [("type", "s")] []
          , MkXMLNode "arg" [("type", "s")] []
          ]
        , MkXMLNode "signal" [("name", "NameLost")]
          [ MkXMLNode "arg" [("type", "s")] []
          ]
        , MkXMLNode "signal" [("name", "NameAcquired")]
          [ MkXMLNode "arg" [("type", "s")] []
          ]
        ]
      , MkXMLNode "interface" [("name", "org.freedesktop.DBus.Introspectable")]
        [ MkXMLNode "method" [("name", "Introspect")]
          [ MkXMLNode "arg" [("direction", "out"), ("type", "s")] []
          ]
        ]
      ]
    ]
  xmlParserTest "xml/test2.xml"
    [ MkXMLNode "node" []
      [ MkXMLNode "interface" [("name", "org.freedesktop.DBus.Properties")]
        [ MkXMLNode "method" [("name", "Get")]
          [ MkXMLNode "arg" [("direction", "in"), ("type", "s"), ("name", "interface_name")] []
          , MkXMLNode "arg" [("direction", "in"), ("type", "s"), ("name", "property_name")] []
          , MkXMLNode "arg" [("direction", "out"), ("type", "v"), ("name", "value")] []
          ]
        , MkXMLNode "method" [("name", "GetAll")]
          [ MkXMLNode "arg" [("direction", "in"), ("type", "s"), ("name", "interface_name")] []
          , MkXMLNode "arg" [("direction", "out"), ("type", "a{sv}"), ("name", "properties")] []
          ]
        , MkXMLNode "method" [("name", "Set")]
          [ MkXMLNode "arg" [("direction", "in"), ("type", "s"), ("name", "interface_name")] []
          , MkXMLNode "arg" [("direction", "in"), ("type", "s"), ("name", "property_name")] []
          , MkXMLNode "arg" [("direction", "in"), ("type", "v"), ("name", "value")] []
          ]
        , MkXMLNode "signal" [("name", "PropertiesChanged")]
          [ MkXMLNode "arg" [("type", "s"), ("name", "interface_name")] []
          , MkXMLNode "arg" [("type", "a{sv}"), ("name", "changed_properties")] []
          , MkXMLNode "arg" [("type", "as"), ("name", "invalidated_properties")] []
          ]
        ]
      , MkXMLNode "interface" [("name", "org.freedesktop.DBus.Introspectable")]
        [ MkXMLNode "method" [("name", "Introspect")]
          [ MkXMLNode "arg" [("direction", "out"), ("type", "s"), ("name", "xml_data")] []
          ]
        ]
      , MkXMLNode "interface" [("name", "org.freedesktop.DBus.Peer")]
        [ MkXMLNode "method" [("name", "Ping")] []
        , MkXMLNode "method" [("name", "GetMachineId")]
          [ MkXMLNode "arg" [("direction", "out"), ("type", "s"), ("name", "machine_uuid")] []
          ]
        ]
      , MkXMLNode "interface" [("name", "org.freedesktop.Notifications")]
        [ MkXMLNode "method" [("name", "Notify")]
          [ MkXMLNode "arg" [("direction", "in"), ("type", "s"), ("name", "arg_0")] []
          , MkXMLNode "arg" [("direction", "in"), ("type", "u"), ("name", "arg_1")] []
          , MkXMLNode "arg" [("direction", "in"), ("type", "s"), ("name", "arg_2")] []
          , MkXMLNode "arg" [("direction", "in"), ("type", "s"), ("name", "arg_3")] []
          , MkXMLNode "arg" [("direction", "in"), ("type", "s"), ("name", "arg_4")] []
          , MkXMLNode "arg" [("direction", "in"), ("type", "as"), ("name", "arg_5")] []
          , MkXMLNode "arg" [("direction", "in"), ("type", "a{sv}"), ("name", "arg_6")] []
          , MkXMLNode "arg" [("direction", "in"), ("type", "i"), ("name", "arg_7")] []
          , MkXMLNode "arg" [("direction", "out"), ("type", "u"), ("name", "arg_8")] []
          ]
        , MkXMLNode "method" [("name", "CloseNotification")]
          [ MkXMLNode "arg" [("direction", "in"), ("type", "u"), ("name", "arg_0")] []
          ]
        , MkXMLNode "method" [("name", "GetCapabilities")]
          [ MkXMLNode "arg" [("direction", "out"), ("type", "as"), ("name", "arg_0")] []
          ]
        , MkXMLNode "method" [("name", "GetServerInformation")]
          [ MkXMLNode "arg" [("direction", "out"), ("type", "s"), ("name", "arg_0")] []
          , MkXMLNode "arg" [("direction", "out"), ("type", "s"), ("name", "arg_1")] []
          , MkXMLNode "arg" [("direction", "out"), ("type", "s"), ("name", "arg_2")] []
          , MkXMLNode "arg" [("direction", "out"), ("type", "s"), ("name", "arg_3")] []
          ]
        , MkXMLNode "signal" [("name", "NotificationClosed")]
          [ MkXMLNode "arg" [("type", "u"), ("name", "arg_0")] []
          , MkXMLNode "arg" [("type", "u"), ("name", "arg_1")] []
          ]
        , MkXMLNode "signal" [("name", "ActionInvoked")]
          [ MkXMLNode "arg" [("type", "u"), ("name", "arg_0")] []
          , MkXMLNode "arg" [("type", "u"), ("name", "arg_1")] []
          ]
        ]
      ]
    ]
