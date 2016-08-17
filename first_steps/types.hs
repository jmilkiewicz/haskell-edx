-- alias
type Strr = [Char] 

type Position = (Int,Int)

origin = (0,0)
left (x,y) = (x-1,y)

originMovedLeft = left origin

type Pair a = (a,a)

multiply (x,y) =  x*y

copy a = (a,a)

copymultipl = multiply . copy

type Trans = Position -> Position

-- structural/nominal system ?
-- scala case classess are similair to algebraic data types 


-- this nominal type, 2 constructors with no arguments, they are values at the same time
data Bool = True| False
-- True and False are "constructors"
-- Bool is a new type, on the right side you have types being subtypes.  
-- False extends Bool , True extends Bool but Bool is abstract so you can not 
-- instantiate it but you can create intstances of True or False but 
-- they both have type Bool


data Answer = Yes | No | Unknown

allAnswers = [Yes,No,Unknown]

flipA Yes = No
flipA No = Yes
flipA _ = Unknown

data Shapes = Circle Float | Rect Float Float | Square Float


square x = Rect x x

-- pattern matching like virtual dispatch
area (Circle r ) = pi * r^2
area (Rect n m) = n *m
area (Square n) = n^2

-- Circle/Square/Rect are function that produces Shape


data Mymaybe a =  Noting | Jus a

safediv _ 0  = Noting
safediv a b = Jus(a `div` b )


safeHead [] = Noting
safeHead xs = Jus(head xs)


safeTail [] = Noting
safeTail (x:[]) = Jus x 
safeTail (x:xs) = safeTail(xs)

--hack
unwrap (Jus x) = x


data Nat = Zero | Succ Nat

one = Succ Zero
two = Succ one


natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

intToNat 0 = Zero
intToNat x = Succ (intToNat (x - 1))


addWithNats n1 n2 = magicAdd (natToInt n1) n2
magicAdd 0 n2 = n2
magicAdd x n2 = Succ (magicAdd (x-1) n2)


add Zero n = n
add (Succ m)  n = Succ (add m n)


