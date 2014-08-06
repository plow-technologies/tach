
module Tach.Transformable.Types.Impulse where

import qualified Data.Vector                           as V
import           Numeric.Tools.Interpolation
import           Tach.Impulse.Types.TimeValue
import           Tach.Transformable.Types.Impulse.Core as Tach.Transformable.Types.Impulse

xs :: [Int]
xs = [0,30,61,92,122,210,241,272,302,333,365,395,426,457,487,518,545,575,606,637,667,698,723,753,784,815,845,876,903,933,964,995,1025,1056,1088,1118,1149,1180,1210,1241,1268,1298,1329,1360,1390,1421,1450,1480,1511,1542,1572,1603,1630,1660,1691,1722,1752,1783,1815,1845,1876,1907,1937,1968,1995,2025,2056,2087,2117,2148,2173,2203,2234,2265,2295,2326,2353,2383,2414,2445,2475,2506,2538,2568,2599,2630,2660,2691,2718,2748,2779,2810,2840,2871,2900,2930,2961,2992,3022,3053,3080,3110,3141,3172,3202,3233,3265,3295,3326,3357,3387,3418,3445,3475,3506,3537,3567,3598,3623,3653,3684,3715,3745,3776,3803,3833,3864,3895,3925,3956,3988,4018,4049,4080,4110,4141,4168,4198,4229,4260,4290,4321,4350,4380,4411,4442,4472,4503,4530,4560,4591,4622,4652,4683,4715,4745,4776,4807,4837,4868,4895,4925,4956,4987,5017,5048,5073,5103,5134,5165,5195,5226,5253,5283,5314,5345,5375,5406,5438,5468,5499,5530,5560,5591,5618,5648,5679,5710,5740,5771,5812,5842,5873,5904,5934,5965,5992,6022,6053,6084,6114,6145,6177,6207,6238,6269,6299,6330,6357,6387,6418,6449,6479,6510,6535,6565,6596,6627,6657,6688,6715,6745,6776,6807,6837,6868,6900,6930,6961,6992,7022,7053,7080,7110,7141,7172,7202,7233,7262,7292,7323,7354,7384,7415,7442,7472,7503,7534,7564,7595,7627,7657,7688,7719,7749,7780,7807,7837,7868,7899,7929,7960,7985,8015,8046,8077,8107,8138,8165,8195,8226,8257,8287,8318,8350,8380,8411,8442,8472,8503,8530,8560,8591,8622,8652,8683,8712,8742,8773,8804,8834,8865,8892,8922,8953,8984,9014,9045,9077,9107,9138,9169,9199,9230,9257,9287,9318,9349,9379,9410,9435,9465,9496,9527,9557,9588,9615,9645,9676,9707,9737,9768,9800,9830,9861,9892,9922,9953,9980,10010,10041,10072,10102,10133,10162,10192,10223,10254,10284,10315,10342,10372]

ys :: [Double]
ys = [1.0,6.0,4.0,2.0,1.0,4,5,1,2,4,4,5,23,3,2,4,5,2,37,2,7,4,8,4,9,3,8,5,6,4,5,7,2,5,8,23,65,4,3,5,7,8,9,0,10,3,10,60,12,15,13,14,16,13,15,18,19,17,14,26,76,52,16,5,3,5,4,5,4,3,2,1,4,6,2,6,1,7,2,8,2,5,2,4,27,6,2,8,25,2,6,4,8,4,5,2,4,3,7,4,4,7,5,6,8,5,3,5,6,6,3,4,6,6,3,4,6,5,2,4,5,56,76,45,24,5,5,6,2,45,7,32,1,4,5,2,3,5,3,23,6,4,5,6,34,4,3,3,4,5,6,54,5,6,2,3,5,6,4,3,4,5,6,3,3,5,6,3,4,56,3,4,3,4,5,3,4,5,3,4,2,1,43,45,4,2,2,3,45,3,4,5,7,8,0.0,9,8,6,4,7,8,9,4,2,6,7,8,4,7,3,5,36,5,3,5,6,3,56,3,55.0,5.0,2.0,3.0,5.0,6.0,4.0,5.0,6.0,1.0,1.0,5.0,2.0,4.0,5.0,6.0,7.0,41.0,1.0,5.0,1.0,2.0,4.0,5.0,15.0,2.0,4.0,1.0,4.0,1.0,2.0,4.0,1.0,2.0,4.0,5.0,6.0,6.0,4.0,3.0,3.0,4.0,5.0,1.0,2.0,4.0,5.0,2.0,3.0,3.0,2.0,1.0,4.0,5.0,6.0,1.0,3.0,4.0,2.0,4.0,2.0,1.0,414.0,3.0,13.0,4.0,1.0,3.0,5.0,53.0,12.0,1.0,2.0,4.0,5.0,1.0,3.0,1.0,2.0,2.0,6.0,4.0,3.0,5.0,3.0,2.0,1.0,5.0,124.0,5.0,4.0,23.0,5.0,5.0,1.0,2.0,5.0,4.0,23.0,4.0,5.0,2.0,3.0,4.0,23.0,4.0,23.0,23.0,23.0,2.0,3.0,33.0,43.0,43.0,34.0,12.0,43.0,21.0,123.0,12.0,32.0,32.0,32.0]

tvnkl :: [TVNoKey]
tvnkl = tvnkFromList $ zip xs ys

tvnkFromList :: [(Int, Double)] -> [TVNoKey]
tvnkFromList = fmap $ uncurry TVNoKey

transformImpulse :: [TVNoKey] -> ImpulseTransformed
transformImpulse tvnklist = ImpulseTransformed rep start end
  where rep = createLinearInterp tvnklist -- Ensure that the list is sorted on time
        vRep = impulseMeshRep . interpolationMesh $ rep
        start = round . V.head $ vRep
        end = round . V.last $ vRep
