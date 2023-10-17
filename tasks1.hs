-- gcd
xGcd a b
  | a == b = a
  | a > b = xGcd (a - b) b
  | otherwise = xGcd a (b - a)

-- power
xPower _ 0 = 1
xPower x n
  | even n = xPower (x * x) (n `div` 2)
  | otherwise = x * xPower x (n - 1)

-- is perfect
xIsPerfect n = sum (divisors n) == n
  where
    divisors x = [d | d <- [1 .. x - 1], x `mod` d == 0]

-- syracuse length
xSyracuseLenHelper x len
  | x == 1 = len + 1
  | even x = xSyracuseLenHelper (x `div` 2) (len + 1)
  | otherwise = xSyracuseLenHelper (3 * x + 1) (len + 1)

xSyracuseLen x = xSyracuseLenHelper x 0

-- calc polynom
xCalcPolynomHelper [] x res = res
xCalcPolynomHelper (h : t) x res = xCalcPolynomHelper t x (h + x * res)

xCalcPolynom [] _ = 0
xCalcPolynom (h : t) x = xCalcPolynomHelper t x h

-- fib
xInfFib = 0 : 1 : zipWith (+) xInfFib (tail xInfFib)

xFib n = take n xInfFib

-- from digits
xFromDigitsHelper _ [] res = res
xFromDigitsHelper base (h : t) res = xFromDigitsHelper base t (base * res + h)

xFromDigits base digits = xFromDigitsHelper base digits 0

-- to digits
xToDigits base x
  | x < base = [x]
  | otherwise = xToDigits base (x `div` base) ++ [x `mod` base]