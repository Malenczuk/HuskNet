import PropertiesTests
import UnitTests
import Test.Tasty

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]


