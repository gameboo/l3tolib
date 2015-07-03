---------------------------------------------------------------------------
-- Exports L3 ISA models to a set of SML FFI declarations
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

Import required haskell packages
================================

> import Text.Parsec
> import Text.Parsec.String (parseFromFile)
> import Data.Char (isAlpha)
> import Data.Char (isDigit)
> import Data.List
> import System.Environment

Types declarations and typeclasses instanciations
=================================================

> -- type synonym for shorter syntax
> type Parser a = Parsec String [Type] a
> 
> -- supported SML types
> data Type = Name String	-- simple type name
>           | Ap [Type]		-- type application
>           | Tuple [Type]	-- tuple of several types
>           | Arrow [Type]	-- arrow of several types
>     deriving Eq	-- TODO find out where
> 
> -- own implementation of the Show typeclass for easier code generation
> instance Show Type where
>     show (Name str) = str
>     show (Ap xs) = intercalate " " (fmap show xs)
>     show (Tuple xs) = intercalate " * " (fmap show xs)
>     show (Arrow xs) = intercalate " -> " (fmap show xs)
> 
> -- "declaration" type. "Val" is of the form "val somename : sometype"
> data Decl = Val String Type
>           | Other
>     deriving Show

Parsing helpers
===============

> kw_list = ["val"] -- list of SML keywords to ignore when parsing an identifier
> 
> -- parses a "token", consumes a string AND the following spaces
> tok :: String -> Parser String
> tok s = string s >> spaces >> return s
> 
> -- parses a parenthesized expressions
> parens = between (tok "(") (tok ")")
> 
> -- parses a word
> name :: Parser String
> name = do {a <- many1 (satisfy p); spaces; return a}
>     where p x = isAlpha x || isDigit x || x == '_' || x == '\'' || x == '.'
> 
> -- parses an identifier i.e. a word that is not a keyword
> ident :: Parser String
> ident = do
>         a <- lookAhead name
>         if any (== a) kw_list then
>             parserZero
>         else
>             name

L3 ".sig" output parser
=======================

The parser is looking for expression of the form *val somename : sometype*.
*somename* and *sometype* are returned in a Decl for the code generator.

Type parser
-----------

Types are parsed following the grammar:

t ::= t1 -> t
   |  t1

t1 ::= t2 * t1
    |  t2

t2 ::= t3 t3
    |  t3

t3 ::= n
    | ( t )

> 
> t :: Parser Type
> t = do
>     a <- t1
>     option a $ do { b <- many1 (tok "->" >> t); return $ Arrow (a:b) }
> 
> t1 :: Parser Type
> t1 = do
>     a <- t2
>     option a $ do { b <- many1 (tok "*" >> t1); return $ Tuple (a:b) }
> 
> t2 :: Parser Type
> t2 = do
>     a <- t3
>     option a $ do { b <- many1 t3; return $ Ap (a:b) }
> 
> t3 :: Parser Type
> t3 = do { a <- ident; return $ Name a } <|> parens t

Declaration parser
------------------

Declarations can be a supported Val declaration, or any other input that will be ignored.

> v :: Parser Decl
> v = do
>     tok "val"
>     a <- ident
>     tok ":"
>     b <- t
>     return $ Val a b
> 
> other :: Parser Decl
> other = (many $ satisfy p) >> (char '\n') >> return Other
>     where p x = x /= '\n'
> 
> decl :: Parser Decl
> decl = v <|> other

SML FFI code generation
=======================

The following functions are a set of helpers to convert an L3 generated SML
type to a valid SML FFI type. Only the standard FFI types (except the pointer
types) and the BitsN type are supported.

> l3_to_ffi_type :: Type -> Type
> l3_to_ffi_type t = case t of
>     Name "BitsN.nbit"   -> Name "Word64.word"
>     Name "int"          -> Name "Int64.int"
>     t                   -> t
> 
> l3_to_ffi_cast :: Type -> String -> String
> l3_to_ffi_cast t s = case t of
>     Name "BitsN.nbit"   -> "Word64.fromInt(BitsN.toInt("++ s ++"))"
>     Name "int"          -> "Int64.fromInt("++ s ++")"
>     _                   -> s
> 
> ffi_to_l3_cast :: Type -> String -> String
> ffi_to_l3_cast t s = case t of
>     Name "Word8.word"   -> "BitsN.fromInt(Word8.toInt("++ s ++"), 8)"
>     Name "Word16.word"  -> "BitsN.fromInt(Word16.toInt("++ s ++"), 16)"
>     Name "Word32.word"  -> "BitsN.fromInt(Word32.toInt("++ s ++"), 32)"
>     Name "Word64.word"  -> "BitsN.fromInt(Word64.toInt("++ s ++"), 64)"
>     Name "word"         -> "BitsN.fromInt(Word32.toInt("++ s ++"), 32)"
>     _                   -> s
> 
> isValidType :: Type -> Bool
> isValidType t = case t of
>     Ap ts               -> all isValidType ts
>     Tuple ts            -> all isValidType ts
>     Arrow ts            -> all isValidType ts
>     Name "unit"         -> True
>     Name "bool"         -> True
>     Name "char"         -> True
>     Name "Int8.int"     -> True
>     Name "Int16.int"    -> True
>     Name "Int32.int"    -> True
>     Name "Int64.int"    -> True
>     Name "int"          -> True
>     Name "Real32.real"  -> True
>     Name "Real64.real"  -> True
>     Name "real"         -> True
>     Name "string"       -> True
>     Name "vector"       -> True
>     Name "Word8.word"   -> True
>     Name "Word16.word"  -> True
>     Name "Word32.word"  -> True
>     Name "Word64.word"  -> True
>     Name "word"         -> True
>     Name "BitsN.nbit"   -> True
>     _                   -> False
> 
> isValidArgType :: Type -> Bool
> isValidArgType = isValidType
> 
> isValidRetType :: Type -> Bool
> isValidRetType t = case t of
>     Tuple ts            -> False
>     _                   -> isValidType t

Simple helper to convert an SML identifier containing a "'" character into a
valid SML FFI symbol name by replacing "'"'s with "_"'s.

> ffi_name :: String -> String
> ffi_name s = [if x == '\'' then '_' else x | x <- s]

Code generation for symbols (currently not working)
---------------------------------------------------

> {-
> ffi_symbol_public_str :: String -> String -> Type -> String
> ffi_symbol_public_str isa n t
>     | isValidType t = intercalate "\n" [
>         "(* "++ isa ++ " - "++ n ++" *)",
>         "_symbol \"" ++ isa ++ "_" ++ ffi_name n ++ "\" public : (unit -> " ++ show t ++ ") * (" ++ show t ++ " -> unit);"
>         ]
>     | otherwise = "(* Unsupported sml ffi symbol for " ++ n ++ " - unsupported type "++ show t ++" *)"
> -}
>
> {-
> ffi_symbol_external_str :: String -> String -> Type -> String
> ffi_symbol_external_str isa n t
>     | isValidType t = intercalate "\n" [
>         "(* "++ isa ++ " - "++ n ++" *)",
>         "_symbol \"" ++ isa ++ "_" ++ ffi_name n ++ "\" external : (unit -> " ++ show t ++ ") * (" ++ show t ++ " -> unit);"
>         ]
>     | otherwise = "(* Unsupported sml ffi symbol for " ++ n ++ " - unsupported type "++ show t ++" *)"
> -}

Code generation for imported symbols
------------------------------------

This code will generate an import FFI declaration. The symbol imported is not
implemented, but simply declared in the L3 ISA model sources. It is expected to
be implemented when linking against a library generated using the produced FFI
declarations.

> ffi_import_str :: String -> String -> [Type] -> String
> ffi_import_str isa fname [arg_list, ret_type]
>     | all isValidArgType arg_types && isValidRetType ret_type && not (null arg_types) = intercalate "\n" [
>         "(* "++ isa ++ " - "++ fname ++" *)",
>         "val "++ fname ++"_ifc_import = _import \""++ isa ++"_ifc_"++ ffi_name fname ++"\" external : "++ (intercalate " * " $ fmap show ffi_arg_types) ++" -> "++ (show . l3_to_ffi_type $ ret_type) ++";",
>         "fun "++ fname ++"_ifc_import_wrapper ("++ (intercalate ", " $ take orig_arg_nb args) ++") = "++ ffi_to_l3_cast (l3_to_ffi_type ret_type) (fname ++"_ifc_import ("++ intercalate ", " [l3_to_ffi_cast a b | (a,b) <- zip arg_types args] ++")"),
>         "val _ = "++ isa ++"."++ fname ++" := "++ fname ++"_ifc_import_wrapper;"
>         ]
>     | otherwise = "(* Unsupported sml ffi import for " ++ fname ++ " - unsupported type(s) ("++ (show $ Tuple arg_types) ++") or "++ show ret_type ++" *)"
>     where   ffi_arg_types = fmap l3_to_ffi_type arg_types
>             args = ["arg"++(show m) | m <- [0..]]
>             orig_arg_nb = case arg_list of
>                 Tuple ts    -> length ts
>                 Name x      -> 1
>                 _           -> 0
>             arg_types = case arg_list of
>                 Tuple ts    -> if (last ts) == (Name "unit") then init ts else ts
>                 Name x      -> [Name x]
>                 _           -> []
> ffi_import_str isa fname ftype = "(* Unsupported sml ffi import for " ++ isa ++ "_" ++ fname ++ " of type " ++ show ftype ++ " *)"

Code generation for exported symbols
------------------------------------

This code will generate an export FFI declaration. The symbol exported is
implemented in the L3 ISA model sources, and will be callable from code linking
against a library generated using the produced FFI declarations.

> ffi_export_str :: String -> String -> [Type] -> String
> ffi_export_str isa fname [arg_list, ret_type]
>     | all isValidArgType arg_types && isValidRetType ret_type && not (null arg_types) = intercalate "\n" [
>         "(* "++ isa ++ " - "++ fname ++" *)",
>         "val "++ fname ++"_export_wrapper = _export \""++ isa ++"_"++ ffi_name fname ++"\" public : (("++ (intercalate " * " $ fmap show ffi_arg_types) ++") -> "++ (show . l3_to_ffi_type $ ret_type) ++") -> unit;",
>         "val _ = "++ fname ++"_export_wrapper ( fn ("++(intercalate ", " $ take orig_arg_nb args)++") => "++ l3_to_ffi_cast ret_type (isa ++"."++ fname ++"("++intercalate ", " [ffi_to_l3_cast a b | (a,b) <- zip ffi_arg_types args]++")")++");"
>         ]
>     | otherwise = "(* Unsupported sml ffi export for " ++ fname ++ " - unsupported type(s) ("++ (show $ Tuple arg_types) ++") or "++ show ret_type ++" *)"
>     where   ffi_arg_types = fmap l3_to_ffi_type arg_types
>             args = ["arg"++(show m) | m <- [0..]]
>             orig_arg_nb = case arg_list of
>                 Tuple ts    -> length ts
>                 Name x      -> 1
>                 _           -> 0
>             arg_types = case arg_list of
>                 Tuple ts    -> if (last ts) == (Name "unit") then init ts else ts
>                 Name x      -> [Name x]
>                 _           -> []
> ffi_export_str isa fname ftype = "(* Unsupported sml ffi export for " ++ isa ++ "_" ++ fname ++ " of type " ++ show ftype ++ " *)"

Identifying supported L3 declarations
-------------------------------------

> ffi_decl_str :: String -> Decl -> String
> ffi_decl_str isa (Val n t) = case t of
>     Ap [Arrow f, Name "ref"]    -> ffi_import_str isa n f
>     Arrow f                     -> ffi_export_str isa n f
>     _                           -> "(* "++ isa ++ " - "++ n ++" , type ("++ show t ++") not supported *)"
> ffi_decl_str _ _ = []

Program's entry point
=====================

> parse_ml_sig :: String -> IO [Decl]
> parse_ml_sig f = do
>         file <- readFile f
>         case runP (many decl) [] f file of
>             Left err -> error ("Parse error: " ++ show err)
>             Right decl_list  -> return decl_list
> 
> main :: IO ()
> main = do
>         [filename, isa] <- getArgs
>         decl_list <- parse_ml_sig filename
>         mapM_ print decl_list
>         let ffi_decl_list = fmap (ffi_decl_str isa) decl_list
>         writeFile (isa++"_ffi.sml") (intercalate "\n" $ filter (not . null) ffi_decl_list)
>         --mapM_ putStrLn $ intersperse "" ffi_decl
