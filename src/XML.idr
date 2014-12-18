module XML

%access public

-- TODO: Be more efficient
Map : Type -> Type -> Type
Map k v = List (k, v)

record ProcessingInstruction : Type where
  MkProcessingInstruction
    :  (target : String)
    -> (data_ : Maybe String)
    -> ProcessingInstruction

data Reference
  = ReferenceChar Char
  | ReferenceEntity String

StringWithRefs : Type
StringWithRefs = List (Either String Reference)

mutual
  record XMLElement : Type where
    MkXMLElement
      :  (name : String)
      -> (attributes : Map String StringWithRefs)
      -> (content : List Content)
      -> XMLElement

  data Content
    = ContentString StringWithRefs
    | ContentProcessingInstruction ProcessingInstruction
    | ContentElement XMLElement

data ExternalID
  = ExternalIDSystem String
  | ExternalIDPublic String String

ParameterEntityReference : Type
ParameterEntityReference = String

data RepeatSpec
  = RepeatSpecOne
  | RepeatSpecZeroOrOne
  | RepeatSpecZeroOrMore
  | RepeatSpecOneOrMore

data ChildrenSpec
  = ChildrenSpecName String RepeatSpec
  | ChildrenSpecChoice (List ChildrenSpec) RepeatSpec
  | ChildrenSpecSeq (List ChildrenSpec) RepeatSpec

data ContentSpec
  = ContentSpecEmpty
  | ContentSpecAny
  | ContentSpecMixed (List String)
  | ContentSpecChildren ChildrenSpec

record ElementDecl : Type where
  MkElementDecl
    :  (name : String)
    -> (content : ContentSpec)
    -> ElementDecl

data DefaultDecl
  = DefaultDeclRequired
  | DefaultDeclImplied
  | DefaultDeclFixed StringWithRefs

data TokenizedType
  = TokenizedTypeID
  | TokenizedTypeIDREF
  | TokenizedTypeIDREFS
  | TokenizedTypeENTITY
  | TokenizedTypeENTITIES
  | TokenizedTypeNMTOKEN
  | TokenizedTypeNMTOKENS

data EnumeratedType
  = EnumeratedTypeNotation (List String)
  | EnumeratedTypeEnumeration (List String)

data AttType
  = AttTypeString
  | AttTypeTokenized TokenizedType
  | AttTypeEnumerated EnumeratedType

record AttDef : Type where
  MkAttDef
    :  (name : String)
    -> (type : AttType)
    -> (default : DefaultDecl)
    -> AttDef

record AttlistDecl : Type where
  MkAttlistDecl
    :  (name : String)
    -> (definitions : List AttDef)
    -> AttlistDecl

record GEntityDecl : Type where
  MkGEntityDecl
    :  (name : String)
    -> (definition : Either StringWithRefs (ExternalID, Maybe String))
    -> GEntityDecl

record PEntityDecl : Type where
  MkPEntityDecl
    :  (name : String)
    -> (definition : Either StringWithRefs ExternalID)
    -> PEntityDecl

data EntityDecl
  = EntityDeclG GEntityDecl
  | EntityDeclP PEntityDecl

PublicID : Type
PublicID = String

record NotationDecl : Type where
  MkNotationDecl
    :  (name : String)
    -> (identifier : Either ExternalID PublicID)
    -> NotationDecl

data MarkupDecl
  = MarkupElementDecl ElementDecl
  | MarkupAttlistDecl AttlistDecl
  | MarkupEntityDecl EntityDecl
  | MarkupNotationDecl NotationDecl
  | MarkupProcessingInstruction ProcessingInstruction

record DocumentType : Type where
  MkDocumentType
    :  (name : String)
    -> (externalId : Maybe ExternalID)
    -> (internalSubset : Maybe (List (Either MarkupDecl ParameterEntityReference)))
    -> DocumentType

Version : Type
Version = String

Encoding : Type
Encoding = String

record XML : Type where
  MkXML
    :  (version : Maybe Version)
    -> (encoding : Maybe Encoding)
    -> (isStandalone : Maybe Bool)
    -> (instructions1 : List ProcessingInstruction)
    -> (documentType : Maybe DocumentType)
    -> (instructions2 : List ProcessingInstruction)
    -> (root : XMLElement)
    -> (instructions3 : List ProcessingInstruction)
    -> XML
