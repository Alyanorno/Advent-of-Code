import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import AdventOfCode


main = defaultMain tests

tests =
	[ testProperty "day1" test_day1
	, testProperty "day1_2" test_day1_2
	, testProperty "day2" test_day2
	, testProperty "day2_2" test_day2_2
	, testProperty "day3" test_day3
	, testProperty "day3_2" test_day3_2
	, testProperty "day4" test_day4
	, testProperty "day4_2" test_day4_2
	, testProperty "day5" test_day5
	, testProperty "day5_2" test_day5_2
	, testProperty "day6" test_day6
	, testProperty "day6_2" test_day6_2
	, testProperty "day7" test_day7
	, testProperty "day7_2" test_day7_2
	, testProperty "day8" test_day8
--	, testProperty "day8_2" test_day8_2
	, testProperty "day9" test_day9
--	, testProperty "day9_2" test_day9_2
	, testProperty "day10" test_day10
--	, testProperty "day10_2" test_day10_2
	, testProperty "day11" test_day11
--	, testProperty "day11_2" test_day11_2
	, testProperty "day12" test_day12
--	, testProperty "day12_2" test_day12_2
	, testProperty "day13" test_day13
--	, testProperty "day13_2" test_day13_2
	, testProperty "day14" test_day14
--	, testProperty "day14_2" test_day14_2
	, testProperty "day15" test_day15
--	, testProperty "day15_2" test_day15_2
	, testProperty "day16" test_day16
--	, testProperty "day16_2" test_day16_2
	, testProperty "day17" test_day17
--	, testProperty "day17_2" test_day17_2
	, testProperty "day18" test_day18
--	, testProperty "day18_2" test_day18_2
	, testProperty "day19" test_day19
--	, testProperty "day19_2" test_day19_2
	, testProperty "day20" test_day20
--	, testProperty "day20_2" test_day20_2
	, testProperty "day21" test_day21
--	, testProperty "day21_2" test_day21_2
--	, testProperty "day22" test_day22
--	, testProperty "day22_2" test_day22_2
	, testProperty "day23" test_day23
--	, testProperty "day23_2" test_day23_2
--	, testProperty "day24" test_day24
--	, testProperty "day24_2" test_day24_2
	, testProperty "day25" test_day25 ]
--	, testProperty "day25_2" test_day25_2 ]

