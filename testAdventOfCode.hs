import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import AdventOfCode


main = defaultMain tests

tests =
	[ testProperty "day1" prop_day1
	, testProperty "day1_2" prop_day1_2
	, testProperty "day2" prop_day2
	, testProperty "day2_2" prop_day2_2
	, testProperty "day3" prop_day3
	, testProperty "day3_2" prop_day3_2
	, testProperty "day4" prop_day4
	, testProperty "day4_2" prop_day4_2
	, testProperty "day5" prop_day5
	, testProperty "day5_2" prop_day5_2
	, testProperty "day6" prop_day6
	, testProperty "day6_2" prop_day6_2
	, testProperty "day7" prop_day7
	, testProperty "day7_2" prop_day7_2
	, testProperty "day8" prop_day8
--	, testProperty "day8_2" prop_day8_2
	, testProperty "day9" prop_day9
--	, testProperty "day9_2" prop_day9_2
	, testProperty "day10" prop_day10
--	, testProperty "day10_2" prop_day10_2
	, testProperty "day11" prop_day11
--	, testProperty "day11_2" prop_day11_2
	, testProperty "day12" prop_day12
--	, testProperty "day12_2" prop_day12_2
	, testProperty "day13" prop_day13
--	, testProperty "day13_2" prop_day13_2
	, testProperty "day14" prop_day14
--	, testProperty "day14_2" prop_day14_2
	, testProperty "day15" prop_day15
--	, testProperty "day15_2" prop_day15_2
	, testProperty "day16" prop_day16
--	, testProperty "day16_2" prop_day16_2
	, testProperty "day17" prop_day17
--	, testProperty "day17_2" prop_day17_2
	, testProperty "day18" prop_day18
--	, testProperty "day18_2" prop_day18_2
	, testProperty "day19" prop_day19
--	, testProperty "day19_2" prop_day19_2
	, testProperty "day20" prop_day20
--	, testProperty "day20_2" prop_day20_2
	, testProperty "day21" prop_day21
--	, testProperty "day21_2" prop_day21_2
--	, testProperty "day22" prop_day22
--	, testProperty "day22_2" prop_day22_2
	, testProperty "day23" prop_day23
--	, testProperty "day23_2" prop_day23_2
--	, testProperty "day24" prop_day24
--	, testProperty "day24_2" prop_day24_2
	, testProperty "day25" prop_day25 ]
--	, testProperty "day25_2" prop_day25_2 ]

