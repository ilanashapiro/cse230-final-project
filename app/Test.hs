
import Test.QuickCheck
import ImageGen 
import WordAnalysis

commProp :: Int -> Int -> Bool 
commProp x y = x + y == y + x

quickCheckN :: Testable prop => Int -> prop -> IO ()
quickCheckN n = quickCheckWith (stdArgs {maxSuccess = n})

main :: IO ()
main = do
    -- ImageGen Tests
    putStr "Testing Partial Image Shape ==> "
    quickCheckN 1000 testPartialDocShape 
    putStr "Testing Partial Image Illegal Inputs ==> "
    quickCheckN 1000 testPartialDocIllegal
    putStr "Test Partial Image <= 0 Score ==> "
    quickCheckN 1000 testDocSmallCur
    putStr "Test Partial Image >= 0 Score ==> "
    quickCheckN 1000 testDocLargeCur

    -- Word Analysis Tests
    putStr "Testing Missed Words are Sorted ==> "
    quickCheckN 1000 testIsSortedMissedWords
    putStr "Testing Missed Letters are Sorted ==> "
    quickCheckN 1000 testIsSortedMissedLetters

