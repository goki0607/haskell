addOne :: Int -> Int
addOne x = x + 1
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z
factorial :: Integer -> Integer
factorial n = product [1..n]

-- typelcasses:
--   Eq for equality testing
--   Ord for ordering i.e. > < >= and <=
--   function: compare a b or a `compare` b
--   to be a member of Ord you must be a member of Eq
--   Show means can be presented as a string,
--   the function in this case is show a
--   Read is opposite of show,
--   read takes a string and gives type which is member
--   of Read

-- INTERESTING INFERNCE POINT
-- read a will not work, what type do we want it to be?
-- we can instead do read a op b, where b \= string and
-- op is some operation, this way we can infer the type
-- the other option is to use explicit type annotations
-- this looks like: read a :: (type) [Int]

--   Enum, sequentially ordered types
--   Bounded, members have upper and lower bounds,
--   minBound :: (type), maxBound :: (type),
--   minBound :: (typeA, typeB, typeC)
--   Num, numeric typeclass, members act like numbers
--   to be in Num you must be in Show and Eq
--   includes real and integral numbers
--   Integral, only integral numbers (whole) i.e.
--   Int and Integer
--   Floating, only floating point numbers, i.e.
--   Float and Double
--   fromIntegral, takes an Integral gives a Floating,
--   e.g. fromIntegral (length [1,2,3]) + 3.2
--   class costraints