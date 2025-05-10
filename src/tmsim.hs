data Shift = L | R deriving(Show)
data TuringMachine state symbol = TuringMachine {
    state :: state,
    empty :: symbol,
    final :: [state],
    input :: [symbol],
    transition :: state -> symbol -> Maybe (state, symbol, Shift)
}

data Tape symbol = Tape [symbol] [symbol]

instance Show symbol => Show (Tape symbol) where
    show (Tape ps ns) = show ((reverse ps) ++ ns)

emptyTape :: Tape symbol
emptyTape = Tape [] []

symbol :: Tape symbol -> symbol -> symbol
symbol (Tape _ (symbol:_)) _ = symbol
symbol  _              empty = empty

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
advance tm tape = case transition tm (state tm) (symbol tape (empty tm)) of
        Just (s, sym, d) -> Just (tm { state = s }, push (empty tm) tape sym d)
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

data Q = A | B | C | Halt
    deriving (Show, Eq)

data Bin = Zero | One
    deriving (Show, Eq)

bb3' :: Q -> Bin -> Maybe (Q, Bin, Shift)
bb3' A Zero = Just (B, One, R)
bb3' A One  = Just (C, One, L)
bb3' B Zero = Just (A, One, L)
bb3' B One  = Just (B, One, R)
bb3' C Zero = Just (B, One, L)
bb3' C One  = Just (Halt, One, R)

bb3 :: TuringMachine Q Bin
bb3 = TuringMachine {
        state      = A,
        empty      = Zero,
        final      = [Halt],
        input      = [],
        transition = bb3'
    }

main = do
    let (tm, tape) = verify bb3 emptyTape
    print $ snd $ run tm tape 13
