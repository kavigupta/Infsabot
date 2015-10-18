import System.Eval.Haskell

s = "x\n\twhere\n\t\tx :: Int -> Int\n\t\tx t = y\n\t\ty = 2\n\t\tz = 3"

y = do
	f <- x
	putStrLn $ show (unpack f 13)

unpack (Just x) = x
unpack Nothing = error "Type error"

x :: IO (Maybe (Int -> Int))
x = eval s ["Prelude"] where {
	s = "2::Int"
}
