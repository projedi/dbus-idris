module DBus.Types

import Data.SortedMap

-- TODO: Turned off totality checker for now
-- %default total

data DBusTyKind = BasicTy | ContainerTy

mutual
  data DBusTy : DBusTyKind -> Type where
    DBusByte : DBusTy BasicTy
    DBusBoolean : DBusTy BasicTy
    DBusInt16 : DBusTy BasicTy
    DBusUInt16 : DBusTy BasicTy
    DBusInt32 : DBusTy BasicTy
    DBusUInt32 : DBusTy BasicTy
    DBusInt64 : DBusTy BasicTy
    DBusUInt64 : DBusTy BasicTy
    DBusDouble : DBusTy BasicTy
    DBusUnixFD : DBusTy BasicTy
    DBusString : DBusTy BasicTy
    DBusObjectPath : DBusTy BasicTy
    DBusSignature : DBusTy BasicTy
    DBusStruct : Signature -> DBusTy ContainerTy
    DBusArray : DBusTyAny -> DBusTy ContainerTy
    DBusVariant : DBusTy ContainerTy
    DBusDictionary : DBusTy BasicTy -> DBusTyAny -> DBusTy ContainerTy

  DBusTyAny : Type
  DBusTyAny = (k : DBusTyKind ** DBusTy k)

  Signature : Type
  Signature = (n : Nat ** Vect n DBusTyAny)

ObjectPath : Type
ObjectPath = List String -- TODO: Each path element must not be empty

UnixFD : Type
UnixFD = Bits32

-- TODO: This is completely wrong:
Int16 : Type
Int16 = Bits16

Int32 : Type
Int32 = Bits32

Int64 : Type
Int64 = Bits64

mutual
  Variant : Type
  Variant = (t : DBusTyAny ** interpDBusTy t)

  interpDBusTy : DBusTyAny -> Type
  interpDBusTy (_ ** DBusByte) = Bits8
  interpDBusTy (_ ** DBusBoolean) = Bool
  interpDBusTy (_ ** DBusInt16) = Int16
  interpDBusTy (_ ** DBusUInt16) = Bits16
  interpDBusTy (_ ** DBusInt32) = Int32
  interpDBusTy (_ ** DBusUInt32) = Bits32
  interpDBusTy (_ ** DBusInt64) = Int64
  interpDBusTy (_ ** DBusUInt64) = Bits64
  interpDBusTy (_ ** DBusDouble) = Float
  interpDBusTy (_ ** DBusUnixFD) = UnixFD
  interpDBusTy (_ ** DBusString) = String
  interpDBusTy (_ ** DBusObjectPath) = ObjectPath
  interpDBusTy (_ ** DBusSignature) = Signature
  interpDBusTy (_ ** DBusStruct (_ ** xs)) = go xs
    where go : Vect n DBusTyAny -> Type
          go [] = ()
          go (x :: xs) = (assert_total (interpDBusTy x), go xs) -- obviously total
  interpDBusTy (_ ** DBusArray x) = List (assert_total (interpDBusTy x)) -- obviously total
  interpDBusTy (_ ** DBusVariant) = assert_total Variant -- obviously total
  interpDBusTy (_ ** DBusDictionary x y) = SortedMap (assert_total (interpDBusTy (_ ** x))) (assert_total (interpDBusTy y)) -- obviously total

printDBusTy' : DBusTyAny -> List Char
printDBusTy' (_ ** DBusByte) = ['y']
printDBusTy' (_ ** DBusBoolean) = ['b']
printDBusTy' (_ ** DBusInt16) = ['n']
printDBusTy' (_ ** DBusUInt16) = ['q']
printDBusTy' (_ ** DBusInt32) = ['i']
printDBusTy' (_ ** DBusUInt32) = ['u']
printDBusTy' (_ ** DBusInt64) = ['x']
printDBusTy' (_ ** DBusUInt64) = ['t']
printDBusTy' (_ ** DBusDouble) = ['d']
printDBusTy' (_ ** DBusUnixFD) = ['h']
printDBusTy' (_ ** DBusString) = ['s']
printDBusTy' (_ ** DBusSignature) = ['g']
printDBusTy' (_ ** DBusVariant) = ['v']
printDBusTy' (_ ** DBusArray t) = 'a' :: assert_total (printDBusTy' t) -- obviously total
printDBusTy' (_ ** DBusStruct (_ ** ts)) = '(' :: assert_total (concat (map printDBusTy' ts)) ++ [')'] -- ts is a Vect => finite.
printDBusTy' (_ ** DBusDictionary x y) = 'a' :: '{' :: assert_total (printDBusTy' (_ ** x)) ++ assert_total (printDBusTy' y) ++ ['}'] -- obviously total

parseDBusTy' : List Char -> (Maybe DBusTyAny, List Char)
parseDBusTy' ('y' :: xs) = (Just (_ ** DBusByte), xs)
parseDBusTy' ('b' :: xs) = (Just (_ ** DBusBoolean), xs)
parseDBusTy' ('n' :: xs) = (Just (_ ** DBusInt16), xs)
parseDBusTy' ('q' :: xs) = (Just (_ ** DBusUInt16), xs)
parseDBusTy' ('i' :: xs) = (Just (_ ** DBusInt32), xs)
parseDBusTy' ('u' :: xs) = (Just (_ ** DBusUInt32), xs)
parseDBusTy' ('x' :: xs) = (Just (_ ** DBusInt64), xs)
parseDBusTy' ('t' :: xs) = (Just (_ ** DBusUInt64), xs)
parseDBusTy' ('d' :: xs) = (Just (_ ** DBusDouble), xs)
parseDBusTy' ('h' :: xs) = (Just (_ ** DBusUnixFD), xs)
parseDBusTy' ('s' :: xs) = (Just (_ ** DBusString), xs)
parseDBusTy' ('g' :: xs) = (Just (_ ** DBusSignature), xs)
parseDBusTy' ('v' :: xs) = (Just (_ ** DBusVariant), xs)
parseDBusTy' ('a' :: '{' :: xs) with (parseDBusTy' xs)
  parseDBusTy' ('a' :: '{' :: xs) | (Just (BasicTy ** t1), xs') with (parseDBusTy' (assert_smaller xs xs')) -- when we return Just the rest of the input has always something consumed
    parseDBusTy' ('a' :: '{' :: _) | (Just (BasicTy ** t1), _) | (Just t2, '}' :: xs'') = (Just (_ ** DBusDictionary t1 t2), xs'')
    parseDBusTy' ('a' :: '{' :: xs) | (Just _, _) | (_, _) = (Nothing, 'a' :: '{' :: xs)
  parseDBusTy' ('a' :: '{' :: xs) | (_, _) = (Nothing, 'a' :: '{' :: xs)
parseDBusTy' ('a' :: xs) with (parseDBusTy' xs)
  parseDBusTy' ('a' :: xs) | (Just t, xs') = (Just (_ ** DBusArray t), xs')
  parseDBusTy' ('a' :: xs) | (Nothing, xs') = (Nothing, 'a' :: xs)
parseDBusTy' ('(' :: xs) =
  case go xs of
       (Just sig, xs') => (Just (_ ** DBusStruct sig), xs')
       (Nothing, _) => (Nothing, '(' :: xs)
 where go : List Char -> (Maybe Signature, List Char)
       go (')' :: xs) = (Just (_ ** []), xs)
       go [] = (Nothing, [])
       go xs with (parseDBusTy' xs)
         go xs | (Just t, xs') with (go (assert_smaller xs xs')) -- when we return Just the rest of the input has always something consumed
           go xs | (Just t, _) | (Just (_ ** ts), xs'') = (Just (_ ** t :: ts), xs'')
           go xs | (Just t, _) | (Nothing, _) = (Nothing, xs)
         go xs | (Nothing, _) = (Nothing, xs)
parseDBusTy' xs = (Nothing, xs)

printDBusTy : DBusTyAny -> String
printDBusTy = pack . printDBusTy'

parseDBusTy : String -> Maybe DBusTyAny
parseDBusTy str with (parseDBusTy' $ unpack str)
  parseDBusTy str | (Just t, []) = Just t
  parseDBusTy str | _ = Nothing
