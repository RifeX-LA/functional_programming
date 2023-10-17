import Data.Function (fix)

-- gcd
xGcdHelper fn a b
  | a == b = a
  | a > b = fn (a - b) b
  | otherwise = fn a (b - a)

xGcd a b = fix xGcdHelper a b

-- fib
xInfFibHelper fn a b = a : fn b (a + b)

xInfFib = fix xInfFibHelper 0 1

xFibHelper fn a _ 0 = a
xFibHelper fn a b n = fn b (a + b) (pred n)

xFib n = fix xFibHelper 0 1 (pred n)

-- sum
xSumHelper _ [] s = s
xSumHelper fn (h : t) s = fn t (s + h)

xSum xs = fix xSumHelper xs 0

-- min
xMinHelper _ [] m = m
xMinHelper fn (h : t) m = fn t (min h m)

xMin (h : t) = fix xMinHelper t h

-- reverse
xReverseHelper _ rev [] = rev
xReverseHelper f rev (h : t) = f (h : rev) t

xReverse xs = fix xReverseHelper [] xs