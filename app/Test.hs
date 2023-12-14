
import Test.QuickCheck
import ImageGen 

commProp :: Int -> Int -> Bool 
commProp x y = x + y == y + x

quickCheckN :: Testable prop => Int -> prop -> IO ()
quickCheckN n = quickCheckWith (stdArgs {maxSuccess = n})

main :: IO ()
main = do
    putStr "Testing Partial Image Shape ==> "
    quickCheckN 1000 testPartialDocShape 
    putStr "Testing Partial Image Illegal Inputs ==> "
    quickCheckN 1000 testPartialDocIllegal
    putStr "Test Partial Image <= 0 Score ==> "
    quickCheckN 1000 testDocSmallCur
    putStr "Test Partial Image >= 0 Score ==> "
    quickCheckN 1000 testDocLargeCur
