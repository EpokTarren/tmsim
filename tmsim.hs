{-# LANGUAGE LambdaCase #-}
import Control.Applicative
import Control.Arrow
import Control.Monad(void)
import Data.Char(isAlphaNum)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.List (find)
import System.Exit

data Shift = L | R deriving(Show)
data TuringMachine state symbol = TuringMachine {
    state      :: state,
    blank      :: symbol,
    final      :: [state],
    input      :: [symbol],
    transition :: state -> symbol -> Maybe (state, symbol, Shift)
}

data Tape symbol = Tape [symbol] [symbol]

instance Show symbol => Show (Tape symbol) where
    show (Tape [] []) = "[]"
    show (Tape ps ns) = foldl1 (\a b -> a ++ (' ':b)) $ map show (reverse ps ++ ns)

blankTape :: Tape symbol
blankTape = Tape [] []

symbol :: Tape symbol -> symbol -> symbol
symbol (Tape _ (symbol:_)) _ = symbol
symbol  _              blank = blank

push' :: a -> [a] -> (a, [a])
push' _ (x:xs) = (x, xs)
push' x []     = (x, [])

push :: symbol -> Tape symbol -> symbol -> Shift -> Tape symbol
push _ (Tape ps (_:ns))     s R = Tape (s:ps) ns
push _ (Tape ps ns)         s R = Tape (s:ps) ns
push _ (Tape (p:ps) (_:ns)) s L = Tape ps (p:s:ns)
push p (Tape []     (_:ns)) s L = Tape [] (p:s:ns)
push _ (Tape (p:ps) _)      s L = Tape ps [p, s]
push p (Tape [] [])         s L = Tape [] [p, s]

accepting :: Eq state => TuringMachine state symbol -> Bool
accepting tm = state tm `elem` final tm

advance :: TuringMachine state symbol -> Tape symbol
        -> Maybe (TuringMachine state symbol, Tape symbol)
advance tm tape = case transition tm (state tm) (symbol tape (blank tm)) of
        Just (s, sym, d) -> Just (tm { state = s }, push (blank tm) tape sym d)
        Nothing -> Nothing

run :: Eq state => TuringMachine state symbol -> Tape symbol -> Int
    -> (TuringMachine state symbol, Tape symbol)
run _  _    0 = error "Turing machine exceeded allowed iteration count"
run tm tape n = case advance tm tape of
    Just (tm', tape') -> if accepting tm' then (tm', tape')
                                          else run tm' tape' (n-1)
    Nothing           -> (tm, tape)

verify :: Eq symbol => TuringMachine state symbol -> Tape symbol -> (TuringMachine state symbol, Tape symbol)
verify tm (Tape ps ns) = let xs = input tm in
    if all (`elem` xs) ps && all (`elem` xs) ns
        then (tm, Tape ps ns)
        else error "Invalid token in turing machine"

data Error i
  = EndOfInput
  | Unexpected i
  | Expected i i
  | ExpectedSymbol i
  | EarlyEndOfInput [i]
  | UnknownState State
  | UnknownSymbol Symbol
  | Redefinition String
  | Missing String
  | Empty
  deriving (Eq, Show)

newtype Parser i a = Parser { runParser :: [i] -> Either (Error i) (a, [i]) }

instance Functor (Parser i) where
  fmap f (Parser p) = Parser $ \input -> do
    (output, rest) <- p input
    pure (f output, rest)

instance Applicative (Parser i) where
  pure a = Parser $ Right . (a, )

  Parser f <*> Parser p = Parser $ \input -> do
    (f', rest) <- f input
    (output, rest') <- p rest
    pure (f' output, rest')

instance Monad (Parser i) where
  return = pure

  Parser p >>= k = Parser $ \input -> do
    (output, rest) <- p input
    runParser (k output) rest

instance Alternative (Either (Error i)) where
  empty = Left Empty
  Left Empty <|> e2 = e2
  e1 <|> _ = e1

instance (Eq i) => Alternative (Parser i) where
  empty = Parser $ const empty
  Parser l <|> Parser r = Parser $ \input -> l input <|> r input

satisfy :: (i -> Bool) -> Parser i i
satisfy predicate = Parser $ \case
  [] -> Left EndOfInput
  hd : rest
    | predicate hd -> Right (hd, rest)
    | otherwise -> Left $ Unexpected hd

char :: Eq i => i -> Parser i i
char i = Parser char'
    where
        char' []     = Left EndOfInput
        char' (x:xs) = if x == i then Right (i, xs)
                                 else Left $ Expected i x

oneOfChar :: Eq i => [i] -> Parser i i
oneOfChar cs = satisfy (`elem` cs)

string :: Eq i => [i] -> Parser i [i]
string = traverse char

lenient :: Parser i a -> Parser i a
lenient (Parser f) = Parser $ left (const Empty) . f

charL :: Eq i => i -> Parser i i
charL = lenient . char

oneOfCharL :: Eq i => [i] -> Parser i i
oneOfCharL = lenient . oneOfChar

stringL :: Eq i => [i] -> Parser i [i]
stringL = lenient . string

nonEmptyList :: Eq i => Parser i a -> Parser i b -> Parser i [b]
nonEmptyList sep item = do
     first <- item
     rest  <- Parser (Right . list' (sep *> item))
     return $ first : rest

list' :: Parser i b -> [i] -> ([b], [i])
list' i s = case runParser i s of
    Left  _       -> ([], s)
    Right (x, s') -> f x $ list' i s'
    where f x (xs, s) = (x:xs, s)

list :: Eq i => Parser i a -> Parser i b -> Parser i [b]
list sep item = Parser $ \input ->
    case runParser (nonEmptyList sep item) input of
        Left   _      -> Right ([], input)
        Right (xs, s) -> Right (xs, s)

split :: (a -> Bool) -> [a] -> ([a], [a])
split _    []   = ([], [])
split p (c:cs)  = if p c then ([], c:cs) else (c:l, r')
    where (l, r') = split p cs

predicateString :: (i -> Bool) -> Parser i [i]
predicateString p = Parser $ \input -> case split (not . p) input of
    ([], []) -> Left EndOfInput
    ([], r)  -> Left $ Unexpected $ head r
    (l, r)   -> Right (l , r)

lf :: Parser Char ()
lf = void $ char '\n'

crlf :: Parser Char ()
crlf = void $ string "\r\n"

eof :: Parser Char ()
eof = Parser $ \s->if null s then Right ((), s) else Left Empty

newLine :: Parser Char ()
newLine = lenient eof <|> lenient crlf <|> lf

newLines :: Parser Char ()
newLines = newLine <* Parser (Right . newLines')
    where
        newLines' [] = ((), [])
        newLines' s  = case runParser newLine s of
            Left   _      -> ((), s)
            Right (_, s') -> newLines' s'

ws :: Parser Char ()
ws = Parser ws'
    where
        ws' (' ':is) = ws' is
        ws' ('\t':is) = ws' is
        ws'      is  = Right ((), is)

comma :: Parser Char ()
comma = ws <* char ',' <* ws

alphaNum :: Parser Char String
alphaNum = predicateString isAlphaNum

newtype State = State String deriving(Show, Eq, Ord)
newtype Symbol = Symbol String deriving(Eq, Ord)

instance Show Symbol where
    show (Symbol s) = s

data Statement
    = States     [State]
    | Alphabet   [Symbol]
    | Blank      Symbol
    | Transition (State, Symbol) (State, Symbol, Shift)
    | Input      [Symbol]
    | Initial    State
    | Accepting  [State]
    deriving(Show)

equals :: Parser Char a -> Parser Char b -> Parser Char b
equals name value = ws *> name *> ws *> char '=' *> ws *> value <* ws <* newLines

eqListStrict :: Parser Char a -> Parser Char b -> Parser Char [b]
eqListStrict name value = name `equals` (char '{' *> ws *> nonEmptyList comma value <* ws <* char '}')

eqList :: Parser Char a -> Parser Char b -> Parser Char [b]
eqList name value = equals name
    $ char '{' *> ws *> list comma value <* ws <* char '}'

symbolP :: Parser Char Symbol
symbolP = Symbol <$> alphaNum

stateP :: Parser Char State
stateP = State <$> alphaNum

statesP :: Parser Char Statement
statesP = fmap States $
    (void (oneOfCharL "Q") <|> void (stringL "states")) `eqListStrict` stateP

alphabetP :: Parser Char Statement
alphabetP = fmap Alphabet $
    (void (oneOfCharL "GΓ") <|> void (stringL "alpabet")) `eqListStrict` symbolP

blankP :: Parser Char Statement
blankP = Blank <$> (void (stringL "blank") <|> void (charL 'b')) `equals` symbolP

shift :: Parser Char Shift
shift = left <|>right
    where
        left  = fmap (const L) $ void (stringL "left" <|> stringL "Left") <|> void (oneOfCharL "lL")
        right = fmap (const R) $ void (stringL "right" <|> stringL "Right") <|> void (oneOfChar "rR")

transitionP :: Parser Char Statement
transitionP = do
    ws
    void (stringL "transition") <|>void (oneOfCharL "dδ")
    ws *> char '(' *> ws

    state  <- ws *> stateP
    symbol <- comma *> symbolP <* ws

    ws *> char ')' *> ws *> char '='

    ws *> char '(' *> ws

    state'  <- stateP  <* comma
    symbol' <- symbolP <* comma
    shift   <- shift

    ws *> char ')' *> newLines

    return (Transition (state, symbol) (state', symbol', shift))

inputP :: Parser Char Statement
inputP = fmap Input $
    (void (oneOfCharL "ΣS") <|> void (stringL "input")) `eqList` symbolP

initialP :: Parser Char Statement
initialP = Initial <$> name `equals` stateP
    where name = void (stringL "q0" <|> stringL "initial")
              <|> void (charL 'q')

acceptingP :: Parser Char Statement
acceptingP = Accepting <$> name `eqList` stateP
    where name = void (stringL "final" <|> stringL "accepting")
              <|> void (oneOfCharL "F")

statement :: Parser Char Statement
statement = statesP <|> alphabetP <|> blankP <|> transitionP
         <|> inputP <|> initialP <|> acceptingP

statements :: Parser Char [Statement]
statements = Parser $ \input -> case list' statement input of
    (xs, [])    -> Right (xs, [])
    (_, input') -> Left $ EarlyEndOfInput input'

chain :: Parser a [b] -> Parser b c -> [a] -> Either (Either (Error a) (Error b)) (c, [b])
chain l r s = case runParser l s of
     Left  err     -> Left $ Left err
     Right (s', _) -> left Right $ runParser r s'

tm :: Parser Statement (TuringMachine State Symbol)
tm = do
    states <- tm' "states" $ \case
        States states -> Just states
        _             -> Nothing

    alphabet <- tm' "alphabet" $ \case
        Alphabet alphabet -> Just alphabet
        _                 -> Nothing

    blank <- andThen (typeCheckOpt UnknownSymbol alphabet)
        $ tm'' $ \case
            Blank blank -> Just blank
            _            -> Nothing

    input <- andThen (typeCheck UnknownSymbol alphabet)
        $ tm' "input" $ \case
            Input input -> Just input
            _           -> Nothing

    initial <- andThen (typeCheckOpt UnknownState states)
        $ tm'' $ \case
            Initial initial -> Just initial
            _               -> Nothing

    accepting <- andThen (typeCheck UnknownState states)
        $ tm' "accepting" $ \case
            Accepting accepting -> Just accepting
            _                   -> Nothing

    transitions <- tmTransitions states alphabet

    return TuringMachine {
            state      = fromMaybe (head states) initial,
            blank      = fromMaybe (head alphabet) blank,
            final      = accepting,
            input      = input,
            transition = curry (`Map.lookup` transitions)
        }

andThen :: (b -> Either (Error a) c) -> Parser a b -> Parser a c
andThen f (Parser p) = Parser $ g . p
    where
        g (Right (x, s)) = right (,s) $ f x
        g (Left e)       = Left e

tm' :: String -> (a -> Maybe b) -> Parser a b
tm' s f = Parser $ \input -> case findMap f input of
    (Just x, input') -> Right (x, input')
    (Nothing, _    ) -> Left $ Missing s

tm'' :: (a -> Maybe b) -> Parser a (Maybe b)
tm'' f = Parser $ Right . findMap f

typeCheck :: Eq a => (a -> Error Statement) -> [a] -> [a] -> Either (Error Statement) [a]
typeCheck e xs ys = case find (`notElem` xs) ys of
    Just x  -> Left $ e x
    Nothing -> Right ys

typeCheckOpt :: Eq a => (a -> Error Statement) -> [a] -> Maybe a -> Either (Error Statement) (Maybe a)
typeCheckOpt _ _  Nothing  = Right Nothing
typeCheckOpt e xs (Just y) = if y `elem` xs then Right (Just y) else Left $ e y

tmTransitions :: [State] -> [Symbol] -> Parser Statement (Map.Map (State, Symbol) (State, Symbol, Shift))
tmTransitions states alphabet = Parser $ \input -> case list' (tmTransition states alphabet) input of
        (xs, [])    -> let m = Map.fromList xs in
            if Map.size m == length xs
                then Right (m, [])
                else Left $ Redefinition "the same transition may not be defined more than once"
        (_, input') -> Left $ l (runParser (tmTransition states alphabet) input')
    where
        l (Left a) = a

tmTransition :: [State] -> [Symbol] -> Parser Statement ((State, Symbol), (State, Symbol, Shift))
tmTransition states alphabet = Parser f
    where
        f :: [Statement] -> Either (Error Statement) (((State, Symbol), (State, Symbol, Shift)), [Statement])
        f (Transition (s, sym) (s', sym', lr) : rest) = (((s, sym), (s', sym', lr)), rest)
            <$ typeCheck UnknownState  states [s, s']
            <* typeCheck UnknownSymbol alphabet [sym, sym']
        f (States _    : _) = Left $ Redefinition "states"
        f (Alphabet _  : _) = Left $ Redefinition "alphabet"
        f (Blank _     : _) = Left $ Redefinition "blank"
        f (Input _     : _) = Left $ Redefinition "input"
        f (Initial _   : _) = Left $ Redefinition "initial"
        f (Accepting _ : _) = Left $ Redefinition "accepting"
        f [               ] = Left Empty

findMap :: (a -> Maybe b) -> [a] -> (Maybe b, [a])
findMap _ [] = (Nothing, [])
findMap f (x:xs) = case f x of
    Just x' -> (Just x', xs)
    Nothing -> let (x', xs') = findMap f xs in (x', x:xs')

main = do
    content <- getContents
    case (statements `chain` tm) content of
        Left  (Left err)  -> putStr "Error: " *> print err *> exitFailure
        Left  (Right err) -> putStr "Error: " *> print err *> exitFailure
        Right (tm, _)     -> print $ snd (run tm blankTape 10000000)
