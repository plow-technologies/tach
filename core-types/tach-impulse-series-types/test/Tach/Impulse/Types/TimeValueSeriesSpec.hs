module Tach.Impulse.Types.TimeValueSeriesSpec (main, spec) where
import Tach.Impulse.Types.TimeValueSeries
import Tach.Impulse.Types.TimeValue
import Control.Applicative
import qualified Data.Sequence as S
import Test.Hspec
import Test.QuickCheck
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "TVSimpleImpulseType" $ do
    it "should Be Ordered By it's Date" $ do
      property testTVSimpleimpulseType

-- |Wrapper so that I have  a prop valued list
testTVSimpleimpulseType :: [Integer] -> Bool
testTVSimpleimpulseType il = rslt
    where 
      tvsList = (\x -> TVSimple x 3 3.3) <$> il
      tvsSequence = S.unstableSort (S.fromList tvsList)
      (x,rslt) = S.foldlWithIndex (\(x,trth) i y -> (x , x `tr` y))  (S.index tvsSequence 0 ,True) tvsSequence
      tr (TVSimple t _ _ ) (TVSimple t' _ _ ) = t <= t'
      
      
