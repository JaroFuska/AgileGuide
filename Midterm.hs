-- import Test.QuickCheck

geomR :: [Float] -> Float
geomR = nroot . foldl (\(a, b) x -> (a*x, b+1)) (1, 0)

nroot :: (Float, Integer) -> Float
nroot (a, b) = a ** (1 / fromInteger b)

geomM :: [[Float]] -> Float
geomM = nroot . foldl (\(a, b) row -> let (e,f) = foldl (\(c, d) x -> (c*x, d+1)) (1,0) row in (a*e, b+f)) (1,0)

harmoR :: [Float] -> Float
harmoR = uncurry (/) . foldr (\z (p, s) -> (p+1, s + (1/z))) (0, 0)

harmoM :: [[Float]] -> Float
harmoM = uncurry (/) . foldr (\row (a, b) -> let (e, f) = foldr (\x (c, d) -> (c+1, d+(1/x))) (0, 0) row in (a+e, b+f)) (0,0)

-- qch = quickCheck(\xs -> (not (null xs) && not (isInfinite (harmoR xs))) ==> harmoR xs <= geomR xs)

-- qch1 = quickCheck(\xss f -> True ==> (map f . concat) xss == (concat . map (map f)) xss)


-- inorder :: Fib t -> [t]
-- inorder f = fold (\key l r -> (inorder l) ++ [key] ++ (inorder r)) (:[]) f
