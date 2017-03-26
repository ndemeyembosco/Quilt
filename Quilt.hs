-- CSCI 360, Fall 2016
-- Project 3: the Quilt language

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Quilt where


import Parsing2
import qualified Data.Map as M
import Data.Maybe
import Data.Colour.RGBSpace.HSL



-- | A color is a list of red, green, and blue values between 0.0 - 1.0.
--   For example, [0,0,0] is black, [1,1,1] is white, [0.5, 0, 0.5] is a
--   darkish purple, and so on.

-- GRAMMAR

type Color = [Double]
type Number = Color
type Boolean = Color


data Coord where
  X :: Coord
  Y :: Coord
  deriving Show

data Uop where
  U :: Uop
  E :: Uop
  deriving Show

data Arith where
  Plus  :: Arith
  Minus :: Arith
  Times :: Arith
  Div   :: Arith
  Exp   :: Arith
  -- Mod   :: Arith
  deriving Show

data Comparison where
  Less  :: Comparison
  Great :: Comparison
  Equal :: Comparison
  Loeq  :: Comparison
  Goeq  :: Comparison
  deriving Show

data Bop where
  A :: Arith -> Bop
  C :: Comparison -> Bop
  B :: BlnOp -> Bop
  G :: GeoTrans -> Bop
  deriving Show


data GeoTrans where
   ROT  :: GeoTrans
   TX   :: GeoTrans
   TY   :: GeoTrans
   SCLE :: GeoTrans
   SX   :: GeoTrans
   SY   :: GeoTrans
   deriving Show

data BlnOp where
  AND :: BlnOp
  OR  :: BlnOp
  deriving Show

data Clr where
  RED     :: Clr
  ORNGE   :: Clr
  YELLOW  :: Clr
  GREEN   :: Clr
  BLUE    :: Clr
  INDGO   :: Clr
  PURPLE  :: Clr
  deriving Show

data Nbr where
  I :: Integer -> Nbr
  F :: Double -> Nbr
  deriving Show

data Bln where
  TRUE :: Bln
  FALSE :: Bln
  deriving Show

data Qexp where
   CLR    :: Clr -> Qexp
   NUM    :: Nbr -> Qexp
   CRD    :: Coord -> Qexp
   BLN    :: Bln -> Qexp
   LST    :: Qexp -> Qexp -> Qexp -> Qexp
   IF     :: Qexp -> Qexp -> Qexp -> Qexp
   UOP    :: Uop -> Qexp -> Qexp
   BOP    :: Bop -> Qexp  -> Qexp -> Qexp
   QLT    :: Qexp -> Qexp -> Qexp -> Qexp -> Qexp
   LET    :: String -> Qexp -> Qexp -> Qexp
   SIN    :: Qexp -> Qexp
   COS    :: Qexp -> Qexp
   EXP    :: Qexp -> Qexp
   FLR    :: Qexp -> Qexp
   CEIL   :: Qexp -> Qexp
   RND    :: Qexp -> Qexp
   SQRT   :: Qexp -> Qexp
   ABS    :: Qexp -> Qexp
   HSL    :: Qexp -> Qexp
   INC    :: Qexp -> Qexp
   EN     :: Qexp
   PI     :: Qexp
   VAR    :: String -> Qexp
   deriving Show

-- PARSER

lexer :: TokenParser u
lexer = makeTokenParser emptyDef{
  reservedNames = ["red", "orange", "yellow", "green", "blue", "indigo"
  , "purple", "if", "then", "else", "true", "false", "quilt", "x", "y"
  , "sin", "cos", "exp", "floor", "ceil", "round", "sqrt", "abs", "pi"
  , "e", "let", "in", "rot", "tx", "ty", "scale","sx", "sy", "hsl", "inc_color"]
}

reservedWords :: [String]
reservedWords = ["red", "orange", "yellow", "green", "blue", "indigo"
  , "purple", "if", "then", "else", "true", "false", "quilt", "x", "y"
  , "sin", "cos", "exp", "floor", "ceil", "round", "sqrt", "abs", "pi"
  , "e", "let", "in", "rot", "tx", "ty", "scale","sx", "sy", "hsl", "inc_color"]


-- info :: String -> String
-- info s = if elem s ["if", "else", "then"] then "if statement : its structure is 'if <expr> then <expr> else <expr>' '"
--             else if elem s ["let", "in"] then "handles assignments: its structure is  'let <var> = <expr> in <expr>' "
--               else if elem s ["red", "orange", "yellow", "green", "blue", "indigo"
--                 , "purple"] then s ++ "is a pre-defined color"
--                 else if elem s ["x", "y"
--                 , "sin", "cos", "exp", "floor", "ceil", "round", "sqrt", "abs", "pi"
--                 , "e"] then s ++ "is a pre-defined mathematical expression"
--                  else if elem s ["rot", "tx", "ty", "scale","sx", "sy"] then
--                    s ++ " t : defines a geometric transformations of expression e with t"
--                    else if elem s ["hsl"] then
--                      s ++ "modifies the given color's hue, saturation and light based on the hsl model"
--                      else if s == "quilt" then
--                        "a quilt expression is an expression whose structure is '<quilt> <expr> <expr> <expr> <expr>'" ++ "\n" ++
--                        "The four colors get arranged in a square."
--                        else if elem s ["true", "false"] then
--                          s ++ "is a boolean value"
--                          else if elem s ["x", "y"] then
--                            s ++ "is a reserved variable in this languages that varies from 0.0 to 1.0"
--                            else if s == "inc_color" then "inc_color increases the value of red, green and blue in the color by 0.35"



parens :: Parser a -> Parser a
parens = getParens lexer

identifier :: Parser String
identifier = getIdentifier lexer

reservedOp :: String -> Parser ()
reservedOp = getReservedOp lexer

reserved :: String -> Parser ()
reserved = getReserved lexer

symbol :: String -> Parser String
symbol = getSymbol lexer

whiteSpace :: Parser ()
whiteSpace = getWhiteSpace lexer

integer :: Parser Integer
integer = getInteger lexer

float :: Parser Double
float = getFloat lexer

parseAtomClr :: Parser Clr
parseAtomClr = choice [RED <$ reservedOp "red"
                , ORNGE <$ reservedOp "orange"
                , YELLOW <$ reservedOp "yellow"
                , GREEN <$ reservedOp "green"
                , BLUE <$ reservedOp "blue"
                , INDGO <$ reservedOp "indigo"
                , PURPLE <$ reservedOp "purple"]

parseAtomNbr :: Parser Nbr
parseAtomNbr =  try (F <$> float) <|> I <$> integer

parseAtomCoord :: Parser Coord
parseAtomCoord = try (X <$ reserved "x") <|> (Y <$ reserved "y")

parseAtomBln :: Parser Bln
parseAtomBln = try (TRUE <$ reserved "true") <|> (FALSE <$ reserved "false")


parseAtomUop :: Parser Uop
parseAtomUop = try (U <$ reservedOp "-") <|> E <$ reservedOp "!"

parseAtomBop :: Parser Bop
parseAtomBop = undefined


parseAtomQexp :: Parser Qexp
parseAtomQexp = try (EN <$ reserved "e") <|> try (PI <$ reserved "pi")
    <|>    try (CLR <$> parseAtomClr)
    <|> try (NUM <$> parseAtomNbr)
    <|> try (CRD <$> parseAtomCoord)
    <|> try (BLN <$> parseAtomBln)
    <|> try (VAR <$> identifier)
    <|> try (LST <$> (symbol "[" *> parseQexp) <*> (symbol "," *> parseQexp) <*> (symbol "," *> parseQexp <* symbol "]"))
    <|> try (IF <$> (reserved "if" *> parseQexp) <*> (reserved "then" *> parseQexp) <*> (reserved "else" *> parseQexp))
    <|> try (LET <$> (reserved "let" *> identifier) <*> (reservedOp "=" *> parseQexp) <*> (reserved "in" *> parseQexp))
    <|> try (UOP <$> parseAtomUop <*> parseQexp)
    <|> try (symbol "(" *> parseQexp <* symbol ")")
    <|> try (QLT <$> (reserved "quilt" *> parseQexp) <*> parseQexp <*> parseQexp <*> parseQexp)

parseQexp :: Parser Qexp
parseQexp = buildExpressionParser table parseAtomQexp
 where
   table = [ [Prefix (SIN <$ reserved "sin")
            , Prefix (COS <$ reserved "cos")
            , Prefix (EXP <$ reserved "exp")
            , Prefix (CEIL <$ reserved "ceiling")
            , Prefix (FLR <$ reserved "floor")
            , Prefix (RND <$ reserved "round")
            , Prefix (SQRT <$ reserved "sqrt")
            , Prefix (ABS <$ reserved "abs")
            , Prefix (HSL <$ reserved "hsl")
            , Prefix (INC <$ reserved "inc_color")
            , Infix (BOP (G ROT) <$ reserved "rot") AssocLeft
            , Infix (BOP (G TX) <$ reserved "tx") AssocLeft
            , Infix (BOP (G TY) <$ reserved "ty") AssocLeft
            , Infix (BOP (G SCLE) <$ reserved "scale") AssocLeft
            , Infix (BOP (G SX) <$ reserved "sx") AssocLeft
            , Infix (BOP (G SY) <$ reserved "sy") AssocLeft]
            , [ Infix (BOP (A Exp) <$ reservedOp "^") AssocLeft]
            ,[ Infix (BOP (A Times) <$ reservedOp "*") AssocLeft
            , Infix (BOP (A Div)   <$ reservedOp "/") AssocLeft
            -- , Infix (BOP (A Mod)   <$ reservedOp "%") AssocLeft
             ],[Prefix (UOP U <$ reservedOp "-")
             ,  Prefix (UOP E <$ reservedOp "!")]
           , [ Infix (BOP (A Plus)  <$ reservedOp "+") AssocLeft
             , Infix (BOP (A Minus) <$ reservedOp "-") AssocLeft
             , Infix (BOP (B AND) <$ reservedOp "&&") AssocLeft
             , Infix (BOP (B OR) <$  reservedOp "||") AssocLeft
             ]

           , [ Infix (BOP (C Less) <$ reservedOp "<" ) AssocNone,
               Infix (BOP (C Great) <$ reservedOp ">" ) AssocNone,
              Infix (BOP (C Equal) <$ reservedOp "==" ) AssocNone]
           ]


-- | A quilt function produces a Color for any given location.  The
--   parameters are x and y coordinates in the range [-1,1].
type QuiltFun = Double -> Double -> Color

type Env = M.Map String QuiltFun


-- INTERPRETER

interpClr :: Env -> Clr -> QuiltFun
interpClr _ RED     = \x y -> [1.0, 0.0, 0.0]
interpClr _ ORNGE   = \x y ->  [1.0, 0.64, 0.0]
interpClr _ YELLOW  = \x y ->  [1.0, 1.0, 0.0]
interpClr _ GREEN   = \x y ->  [0.0, 1.0, 0.0]
interpClr _ BLUE    = \x y ->  [0.0, 0.0, 1.0]
interpClr _ INDGO   = \x y ->  [0.3, 0.0, 0.55]
interpClr _ PURPLE  = \x y ->  [0.5, 0, 0.5]

interpNum :: Env -> Nbr -> QuiltFun
interpNum _ (I n) = \x y -> map fromIntegral [n, n, n]
interpNum _ (F d) =  \x y -> [d, d, d]

interpBln :: Env -> Bln -> QuiltFun
interpBln _ TRUE  = \x y -> [1.0, 1.0, 1.0]
interpBln _ FALSE = \x y -> [0.0, 0.0, 0.0]


interpBop :: Env -> Qexp -> QuiltFun
interpBop e (BOP (A Exp) q1 q2) = \x y ->  zipWith (**) ( (interpQexp e q1) x y) ( (interpQexp e q2) x y)
-- interpBop (BOP (A Mod) q1 q2) = \x y ->  zipWith (\x y ->  fromIntegral $ round x `mod` fromIntegral $ round y) ( (interpQexp q1) x y) ( (interpQexp q2) x y)
interpBop e (BOP (A Plus) q1 q2) = \x y ->  zipWith (+) ( (interpQexp e q1) x y) ( (interpQexp e q2) x y)
interpBop e (BOP (A Minus) q1 q2) = \x y ->  zipWith (-) ( (interpQexp e q1) x y) ( (interpQexp e q2) x y)
interpBop e (BOP (A Times) q1 q2) = \x y ->  zipWith (*) ( (interpQexp e q1) x y) ( (interpQexp e q2) x y)
interpBop e (BOP (A Div) q1 q2) = \x y ->  zipWith (/) ( (interpQexp e q1) x y) ( (interpQexp e q2) x y)
interpBop e (BOP (C Less) q1 q2) = \x y -> if (head  $ interpQexp e q1 x y) < (head  $ interpQexp e q2 x y)
  then  [1.0, 1.0, 1.0] else  [0.0, 0.0, 0.0]
interpBop e (BOP (C Great) q1 q2) = \x y  -> if (head  $ interpQexp e q1 x y) > (head  $ interpQexp e q2 x y)
  then  [1.0, 1.0, 1.0] else  [0.0, 0.0, 0.0]
interpBop e (BOP (C Loeq) q1 q2) = \x y  -> if (head  $ interpQexp e q1 x y) <= (head  $ interpQexp e q2 x y)
  then  [1.0, 1.0, 1.0] else  [0.0, 0.0, 0.0]
interpBop e (BOP (C Goeq) q1 q2) = \x y  -> if (head  $ interpQexp e q1 x y) >= (head  $ interpQexp e q2 x y)
  then  [1.0, 1.0, 1.0] else  [0.0, 0.0, 0.0]
interpBop e (BOP (C Equal) q1 q2) = \x y  -> if (head  $ interpQexp e q1 x y) == (head  $ interpQexp e q2 x y)
  then  [1.0, 1.0, 1.0] else  [0.0, 0.0, 0.0]
interpBop e (BOP (B AND) q1 q2) = \x y  -> if (head  $ interpQexp e q1 x y)
  == (head  $ interpQexp e q2 x y) && (head  $ interpQexp e q2 x y) == 1
  then  [1.0, 1.0, 1.0] else  [0.0, 0.0, 0.0]
interpBop e (BOP (B OR) q1 q2) = \x y  -> if (head  $ interpQexp e q1 x y)
  == (head  $ interpQexp e q2 x y) && (head  $ interpQexp e q2 x y) == 0
  then  [0.0, 0.0, 0.0] else  [1.0, 1.0, 1.0]
interpBop e (BOP (G ROT) q1 q2) = \x y -> (interpQexp e q1)
  (x * cos (head( (interpQexp e q2) x y)) - y
  * sin (head ((interpQexp e q2) x y))) (y*cos (head $ (interpQexp e q2) x y)
  + x*sin (head $ (interpQexp e q2) x y))
interpBop e (BOP (G TX) q1 q2)  = \x y -> (interpQexp e q1) (x + head( (interpQexp e q2) x y)) (y)
interpBop e (BOP (G TY) q1 q2)  = \x y -> (interpQexp e q1) (x) (y + head ((interpQexp e q2) x y))
interpBop e (BOP (G SCLE) q1 q2) = \x y -> (interpQexp e q1)
  (x*(head ((interpQexp e q2) x y))) (y*(head ((interpQexp e q2) x y)))
interpBop e (BOP (G SX) q1 q2)   = \x y -> (interpQexp e q1) (x*(head ((interpQexp e q2) x y))) y
interpBop e (BOP (G SY) q1 q2)   = \x y -> (interpQexp e q1) x (y*(head ((interpQexp e q2) x y)))

interpBop _   _              = undefined


interpLST :: Env -> Qexp -> QuiltFun
interpLST e (LST q1 q2 q3) =
  \x y -> map head [(interpQexp e q1) x y, (interpQexp e q2) x y, (interpQexp e q3) x y]
interpLST _    _          = undefined


interpIF :: Env -> Qexp -> QuiltFun
interpIF e (IF bool q1 q2) = \x y -> case (interpQexp e bool) x y of
   [1.0, 1.0, 1.0]  -> (interpQexp e q1) x y
   [0.0, 0.0, 0.0] -> (interpQexp e q2) x y
   _                 -> error "Not True or False"
interpIF  _   _           = undefined

interpQLT :: Env -> Qexp -> QuiltFun
interpQLT e (QLT q1 q2 q3 q4) = \x y -> case (x < 0, y > 0) of
  (True, True)  -> (interpQexp e q1) (2*x + 1) (2*y - 1)
  (False, True) -> (interpQexp e q2) (2*x - 1) (2*y - 1)
  (True, False)  -> (interpQexp e q3) (2*x + 1) (2*y + 1)
  (False, False) -> (interpQexp e q4) (2*x - 1) (2*y + 1)

interpQLT _    _             = undefined



interpQexp :: Env -> Qexp ->  QuiltFun
interpQexp e PI          = \x y -> [pi, pi, pi]
interpQexp e EN          = \x y -> [exp 1, exp 1, exp 1]
interpQexp e (CLR c)     = interpClr e c
interpQexp e (NUM n)     = interpNum e n
interpQexp e (CRD X)     = \x y -> [x, x, x]
interpQexp e (CRD Y)     = \x y -> [y, y, y]
interpQexp e (BLN b)     = interpBln e b
interpQexp e l@LST {}    = interpLST e l
interpQexp e i@IF{}      = interpIF e i
interpQexp e (UOP o q)   = \x y -> case o of
  U ->  map negate $  (interpQexp e q) x y
  E ->  map (\j -> if j == 1.0 then 0.0 else 1.0) $  (interpQexp e q) x y
interpQexp e b@BOP{}     = interpBop e b
interpQexp e q@QLT{}     = interpQLT e q
interpQexp e (COS q)     = \x y -> map cos (interpQexp e q x y)
interpQexp e (SIN q)     = \x y -> map sin (interpQexp e q x y)
interpQexp e (EXP q)     = \x y -> map exp (interpQexp e q x y)
interpQexp e (FLR q)     = \x y -> map (\s -> fromIntegral $ floor s) (interpQexp e q x y)
interpQexp e (CEIL q)     = \x y -> map (\s -> fromIntegral $ ceiling s) (interpQexp e q x y)
interpQexp e (RND q)     = \x y -> map (\s -> fromIntegral $ round s) (interpQexp e q x y)
interpQexp e (ABS q)     = \x y -> map abs (interpQexp e q x y)
interpQexp e (SQRT q)     = \x y -> map sqrt (interpQexp e q x y)
interpQexp e (INC q)     = \x y -> add100 (interpQexp e q x y)
interpQexp e (HSL q)     = \x y -> zipWith (+)
    (zipWith (+)
    (zipWith (+)
    (concat (replicate 3 [(hslToRGB (interpQexp e q x y)) !! 2]))
    (concat (replicate 3 [((hslToRGB (interpQexp e q x y)) !! 1)])))
    (concat (replicate 3 [head (hslToRGB (interpQexp e q x y))])))
    (interpQexp e q x y)
interpQexp e (VAR q)      = fromJust $ M.lookup q e
interpQexp e (LET s q1 q2) = interpQexp (M.insert s (interpQexp e q1) e) q2


-- TYPECHECKER


data Type where
  BO :: Type
  FP :: Type
  CL :: Type
  deriving (Show, Eq)


data TypeError where
  TypeErr :: Type -> Type -> TypeError
  QuiltErr :: String -> TypeError
  CompErr :: String -> TypeError
  BoolErr :: String -> TypeError
  UnBoundVar :: TypeError
  GeoErr   :: String -> TypeError
  deriving Show


type Ctx = M.Map String Type

inferBOP :: Ctx -> Qexp -> Either TypeError Type
inferBOP e (BOP (A _) q1 q2) = case (check e q1 FP, check e q1 CL) of
  (Left _, Right ()) -> Right CL
  (Right (), Left _) -> Right FP
  _                 -> case infer e q1 of
              Left tyerr -> Left tyerr
              Right t    -> case infer e q2 of
                Left tyerr2 -> Left tyerr2
                Right t2    -> Left (TypeErr t t2)
inferBOP e (BOP (C _) q1 q2) = case (check e q1 FP, check e q2 FP) of
  (Right (), Right ())  -> Right BO
  _                     -> Left (CompErr "type error : comparison operators expect num values")
inferBOP e (BOP (B _) q1 q2) = case (check e q1 BO, check e q2 BO) of
  (Right (), Right ())  -> Right BO
  _                     -> Left (BoolErr "type error : boolean operators expect two booleans")
inferBOP e (BOP (G _) q1 q2) = case (check e q2 FP) of
  Right () -> infer e q1
  _        -> Left  (GeoErr "type error : geometric transformations expect a type and a number")

inferBOP _  _               = undefined

inferQLT :: Ctx -> Qexp -> Either TypeError Type
inferQLT e (QLT q1 q2 q3 q4)  = case ((check e q1 FP, check e q1 CL),
      (check e q2 FP, check e q2 CL),
      (check e q3 FP, check e q3 CL),
      (check e q4 FP, check e q4 CL)) of
          ((Left e1, Left e2), _ , _, _)   ->
                Left (QuiltErr $ "type error : a quilt expects values of type color or num 1" ++ show e1 ++ show e2)
          (_, (Left _, Left _) , _, _)   ->
                Left (QuiltErr "type error : a quilt expects values of type color or num 2")
          (_, _ , (Left _, Left _), _)   ->
                Left (QuiltErr "type error : a quilt expects values of type color or num 3")
          (_, _ , _, (Left _, Left _))   ->
                Left (QuiltErr "type error : a quilt expects values of type color or num 4")
          ((Right (), _), (Right (), _), (Right (), _), (Right (), _)) -> Right FP
          _                              -> Right CL
inferQLT  _   _            = undefined



infer :: Ctx -> Qexp -> Either TypeError Type
infer _ PI                  = Right FP
infer _ EN                  = Right FP
infer _ (CLR _)             = Right CL
infer _ (NUM _)             = Right FP
infer _ (CRD _)             = Right FP
infer _ (BLN _)             = Right BO
infer e (LST q1 q2 q3)      = check e q1 FP >> check e q2 FP >> check e q3 FP >> Right FP
infer e (IF q1 q2 q3)       = check e q1 BO >> infer e q2 >>= \t -> check e q3 t >> infer e q1
infer e (UOP E q)           = check e q BO >> Right BO
infer e (UOP U q)           = check e q FP >> Right FP
infer e b@BOP{}             = inferBOP e b
infer e q@QLT{}             = inferQLT e q
infer _ (COS _)             = Right FP
infer _ (SIN _)             = Right FP
infer _ (EXP _)             = Right FP
infer _ (FLR _)             = Right FP
infer _ (CEIL _)            = Right FP
infer _ (ABS _)             = Right FP
infer _ (HSL _)             = Right CL
infer _ (SQRT _)            = Right FP
infer _ (INC _)             = Right CL
infer e (VAR v)             = case M.lookup v e of
  Just t  ->  Right t
  _       ->  Left UnBoundVar
infer c (LET s n m)       = (infer c n) >>= \v -> infer (M.insert s v c) m


check :: Ctx -> Qexp -> Type -> Either TypeError ()
check e q t = infer e q >>= \t1 -> if t == t1 then Right () else Left (TypeErr t1 t)


hslToRGB :: Color -> Color
hslToRGB (r:[g, b]) = case hslView $ hsl r g b of
  (r1, g1, b1)    -> [r1, g1, b1]

add100 :: Color -> Color
add100 c = map (\x -> x + 0.35) c




-- EVAL

evalQuilt :: String -> Either String QuiltFun
evalQuilt s = case parseSome parseQexp s of
  Left err   -> Left (show err)
  Right p -> case infer M.empty $ fst p of
    Left n -> Left (show n)
    Right _ -> Right (interpQexp M.empty $ fst p)
