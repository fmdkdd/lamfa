module State (State,
              runState,
              get, gets,
              put, puts,
              withState)
where

-----------
-- State monad

newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where
    return a = State (\s -> (a,s))
    x >>= g = State (\s0 -> let (a, s1) = runState x s0
                            in runState (g a) s1)


test3 :: State Int String
test3 = return "a" >>= \a ->
        return $ a ++ "b"


-- get :: State s a -> s -> s
-- get x s0 = let (a,s1) = runState x s0
--            in s1

get :: State s s
get = State (\s -> (s,s))

put :: s -> State s ()
put s = State (\_ -> ((),s))

incr :: (Num s) => State s a -> State s a
incr x = State (\s0 -> let (a,s1) = runState x s0
                       in (a,s1+1))

testIncr :: State Int String
testIncr = incr $ return "b" >>= \a ->
           incr $ return $ a ++ "0"

incr' :: (Num s) => State s ()
-- incr' = get >>= \s ->
--         put $ s+1
incr' = get >>= put . (+1)

testIncr' :: State Int String
testIncr' = incr' >>
            let b = "b" in
            incr' >>
            (return $ b ++ "0")

testIncr'Do :: State Int String
testIncr'Do = do incr'
                 let x = "b"
                 incr'
                 return $ x ++ "0"


withState :: s -> State s a -> State s a
withState s f = get >>= \oldEnv ->
                put s >>
                f >>= \res ->
                put oldEnv >>
                return res

gets :: (s -> z) -> State s z
gets f = get >>= \s0 ->
         State (\s -> (f s0, s))


puts :: (s -> s) -> State s ()
puts f = get >>= put . f
--puts f = get >>= \s -> put (f s)

data Mud = Mud { a :: Int, b :: String } deriving Show

testGets :: State Mud String
testGets = gets a >>= \n ->
           puts (\s -> s {a=n+1}) >>
           gets b >>= \str ->
           puts (\s -> s {b=str++"z"}) >>
           return "a"
