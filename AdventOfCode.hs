module AdventOfCode where

-- Tests
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property as T hiding ((.&.))
-- Day 1
-- Day 2
-- Day 3
import Data.List (nub)
-- Day 4
import Data.Hash.MD5 (md5s, Str(..))
-- Day 5
import Data.List (group, intersect, isInfixOf)
-- Day 6
import Data.List (findIndex)
-- Day 7
import Data.List (find, sortBy)
import Data.Bits ((.&.), (.|.), shift, xor)
import Data.Word (Word16)
-- Day 8
-- Day 9
import Data.List (nub, find, delete, permutations)
-- Day 10
import Data.List (group)
-- Day 11
-- Day 12
import Data.Maybe (listToMaybe)
import Data.List (unfoldr, tails)
-- Day 13
import Data.List (nub, permutations, find)
-- Day 14
-- Day 15
import Data.List (transpose)
-- Day 16
-- Day 17
-- Day 18
import Data.List (find)
import Data.Maybe (catMaybes)
-- Day 19
import Data.List (isPrefixOf)
-- Day 20
import Data.Maybe (mapMaybe)
-- Day 21
import Data.List (zipWith4)
import Data.Maybe (catMaybes)
-- Day 22
-- Day 23
-- Day 24
import Data.List (permutations)
-- Day 25

{-# ANN module "HLint: ignore Use fewer imports" #-}


-- Problem description: http://adventofcode.com/day/1
test_day1 = once $ testWithPairs day1
	[ ("(())", 0)
	, ("()()", 0)
	, ("(((", 3)
	, ("))(((((", 3)
	, ("())", negate 1)
	, ("))(", negate 1)
	, (")))", negate 3)
	, (")())())", negate 3)
	, ("", 0)]
day1 = let f '(' = 1; f ')' = -1 in map $ sum.map f

test_day1_2 = once False
day1_2 = let f '(' = 1; f ')' = -1 in map $ length.takeWhile (/=(-1)).scanl (+) 0.map f


-- Problem description: http://adventofcode.com/day/2 
test_day2 = once $ day2 ["2x3x4", "1x1x10"] == [58, 43]
day2 = map $ f.map read.words.map (\c -> if c == 'x' then ' ' else c)
	where f [l,w,h] = let a = [l*w, w*h, h*l] in minimum a + sum (map (*2) a)

test_day2_2 = once False
day2_2 = map $ f.map read.words.map (\c -> if c == 'x' then ' ' else c)
	where f [l,w,h] = l*w*h + l+l+w+w


-- Problem description: http://adventofcode.com/day/3 
test_day3 = once $ day3 [">", "^>v<", "^v^v^v^v^v"] == [2, 4, 2]
day3 = map $ length.nub.scanl (\(a,b) (a',b') -> (a+a', b+b')) (0,0).map f
	where f x = case x of
		'^' -> ( 0,  1)
		'v' -> ( 0, -1)
		'>' -> ( 1,  0)
		'<' -> (-1,  0)

test_day3_2 = once False
day3_2 = map $ length.nub.foldr1 (++).map (f'.map snd).f''.zip [0..].map f
	where
	f'' x = [filter (even.fst) x, filter (odd.fst) x]
	f' = scanl (\(a,b) (a',b') -> (a+a', b+b')) (0,0)
	f x = case x of
		'^' -> ( 0,  1)
		'v' -> ( 0, -1)
		'>' -> ( 1,  0)
		'<' -> (-1,  0)


-- Problem description: http://adventofcode.com/day/4 
test_day4 = once $ day4 ["abcdef", "pqrstuv"] == ["609043", "1048970"]
day4 = map $ f (map show [1 :: Int ..])
	where
	f :: [String] -> String -> String
	f (a:ls) x = if take 5 r == "00000" then a else f ls x
		where r = md5s (Str (x++a))

test_day4_2 = once False
day4_2 = map $ f (map show [1 :: Int ..])
	where
	f :: [String] -> String -> String
	f (a:ls) x = if take 6 r == "000000" then a else f ls x
		where r = md5s (Str (x++a))


-- Problem description: http://adventofcode.com/day/5 
test_day5 = once $ testWithPairs day5
	[ ("ugknbfddgicrmopn", True)
	, ("aaa", True)
	, ("jchzalrnumimnmhp", False)
	, ("haegwjzuvuyypxyu", False)
	, ("dvszwmarrgswjxmb", False)]
day5 = map $ and.pam [notNaughty, nice, nice']
	where
	notNaughty = not.or.flip map sublists.flip isInfixOf
		where sublists = ["ab","cd","pq","xy"]
	nice = (>= 3).length.flip intersect "aeiou"
	nice' = (>= 2).maximum.map length.group

	pam :: [a -> b] -> a -> [b]
	pam f x = map g f
		where g h = h x

test_day5_2 = once False
day5_2 = map $ and.(\x -> [f1 x, f2 x])
	where
	f1 :: String -> Bool
	f1 (a:b:ls) = ([a,b] `isInfixOf` ls) || f1 (b:ls)
	f1 _ = False

	f2 (a:b:c:ls) = (a == c) || f2 (b:c:ls)
	f2 _ = False


-- Problem description: http://adventofcode.com/day/6 
test_day6 = once $ day6 
	[ "turn on 0,0 throgh 999,999"
	, "toggle 0,0 through 999,0"
	, "turn off 499,499 through 500,500"]
	== 998996
day6 = length.filter snd.foldr (\line state -> flip map state.check.parse.words $ line) start.reverse
	where
	check ([(sx, sy), (bx, by)], change) (cord@(x, y), s)
		| sx <= x && x <= bx && sy <= y && y <= by = (cord, change s)
		| otherwise = (cord, s)

	parse ls@(a:b:_)
		| a == "toggle" = (map (f'.(ls!!)) [1,3], not)
		| a == "turn" = (map (f'.(ls!!)) [2,4], \a -> b == "on")
		where f' arg = let (x,y) = span (/=',') arg in (read x, read $ tail y)

	start = [((x, y), False) | x <- [0..999], y <- [0..999]]

test_day6_2 = once False
day6_2 = sum.map snd.foldr (\line state -> flip map state.check.parse.words $ line) start.reverse
	where
	check ([(sx, sy), (bx, by)], change) (cord@(x, y), s)
		| sx <= x && x <= bx && sy <= y && y <= by = (cord, change s)
		| otherwise = (cord, s)

	parse ls@(a:b:_)
		| a == "toggle" = (map (f'.(ls!!)) [1,3], (+2))
		| a == "turn" = (map (f'.(ls!!)) [2,4], \a -> if b == "on" then a+1 else max 0 $ a-1)
		where f' arg = let (x,y) = span (/=',') arg in (read x, read $ tail y)

	start = [((x, y), 0 :: Int) | x <- [0..999], y <- [0..999]]


-- Problem description: http://adventofcode.com/day/7 
test_day7 = once $ day7
	[ "123 -> x"
	, "456 -> y"
	, "x AND y -> d"
	, "x OR y -> e"
	, "x LSHIFT 2 -> f"
	, "y RSHIFT 2 -> g"
	, "NOT x -> h"
	, "NOT y -> i"]
	==
	"d: 72\n" ++
	"e: 507\n" ++
	"f: 492\n" ++
	"g: 114\n" ++
	"h: 65412\n" ++
	"i: 65079\n" ++
	"x: 123\n" ++
	"y: 456\n"
day7 = format.foldr (calc.parse) [].reverse
	where
	format = foldr1 (++).map (\(a,b) -> a ++ ": " ++ show b ++ "\n").sortBy (\(a,_) (b,_) -> compare a b)

	parse :: String -> (String, Statement)
	parse ls = (drop 3 b, case reads a :: [(Int, String)] of
		[(i, " ")] -> NUMBER i
		_ -> let w = words a in if head w == "NOT" then NOT (w!!1) else case w!!1 of
			"AND" -> AND (head w) (w!!2)
			"OR" -> OR (head w) (w!!2)
			"LSHIFT" -> LSHIFT (head w) (read $ w!!2)
			"RSHIFT" -> RSHIFT (head w) (read $ w!!2)
			_ -> error $ "Unknown function " ++ (w!!1))
		where (a,b) = span (/= '-') ls

	calc (v, s) vs = f s
		where
		f (NUMBER i) = (v, i):vs
		f (NOT v1) = rep $ xor (get v1) (fromIntegral (maxBound :: Word16))
		f (AND v1 v2) = rep $ get v1 .&. get v2
		f (OR v1 v2) = rep $ get v1 .|. get v2
		f (LSHIFT v1 v2) = rep $ shift (get v1) v2
		f (RSHIFT v1 v2) = rep $ shiftR (get v1) v2

		rep = replace vs v
		shiftR v1 v2 = shift v1 (v2*(-1))

		get :: String -> Int
		get a = snd $ (\(Just a) -> a) $ find ((==a).fst) vs

	replace ((s',v'):vs) s v
		| s' == s = (s, v) : vs
		| otherwise = (s',v') : replace vs s v
	replace [] s v = [(s, v)]

test_day7_2 = test_day7
day7_2 = day7

type S = String
data Statement = NUMBER Int | NOT S | AND S S | OR S S | LSHIFT S Int | RSHIFT S Int


-- Problem description: http://adventofcode.com/day/8 
test_day8 = once $ day8 ["\"\"", "\"abc\"", "\"aaa\\\"aaa\"", "\"\\x27\""] == [2,2,3,5]
day8 = map $ (\x -> length x - f 0 x).filter (/=' ')
	where
	f i (a:ls) = if a == '"' then f' i ls else f i ls
	f i [] = i

	f' i ls@(a:b:_) = if a == '"' then f i (init ls) else case [a,b] of
		"\\\"" -> step 2
		"\\\\" -> step 2
		"\\x" -> step 4
		_ -> step 1
		where step n = f' (i+1) (drop n ls)
	f' i [a] = case a of '"' -> i; _ -> i+1
	f' i [] = i

test_day8_2 = noTest
day8_2 = empty


-- Problem description: http://adventofcode.com/day/9 
test_day9 = once $ day9
	[ "London to Dublin = 464"
	, "London to Belfast = 518"
	, "Dublin to Belfast = 141"]
	== 605
day9 = minimum.(\ls -> distance (map parse ls) (permutations.nub.concatMap (take 2.delete "to") $ ls)).map words
	where
	parse = (\[from, to, dist] -> ((from, to), read dist)).delete "=".delete "to"

	distance :: [((String, String), Int)] -> [[String]] -> [Int]
	distance ds (l:ls) = if null ls then [] else calc l : distance ds ls
		where
		calc :: [String] -> Int
		calc (a:b:ls) = (\(Just a) -> snd a) (find comp ds) + continue
			where
			continue = if null ls then 0 else calc (b:ls)
			comp ((a',b'),_) = (a == a' || a == b') && (b == a' || b == b')

test_day9_2 = noTest
day9_2 = empty


-- Problem description: http://adventofcode.com/day/10 
test_day10 = once $ day10 ["1"] == [82350]
day10 = map $ length.flip (!!) 40.iterate (foldr1 (++).map (\x -> show (length x) ++ [head x]).group)

test_day10_2 = noTest
day10_2 = empty


-- Problem description: http://adventofcode.com/day/11 
test_day11 = once $ day11 ["abcdefgh", "ghijklmn"] == ["abcdffaa", "ghjaabcc"]
day11 = map $ until check inc.inc
	where
	check = and.pam [ne 'i', ne 'o', ne 'l', test1, test2]
		where ne = notElem

	test1 = flip (>=) 2.length.filter ((>=2).length).group

	test2 ls@(a:b:c:_)
		| succ a == b && succ b == c = True
		| otherwise = test2 $ tail ls
	test2 _ = False

	inc [] = ['a']
	inc ls
		| succ (last ls) > 'z' = inc (init ls) ++ ['a']
		| otherwise = init ls ++ [succ (last ls)]

	pam :: [a -> b] -> a -> [b]
	pam f x = map g f
		where g h = h x

test_day11_2 = noTest
day11_2 = empty


-- Problem description: http://adventofcode.com/day/12 
test_day12 = once $ testWithPairs day12
	[ ("[1,2,3]", 6)
	, ("{\"a\":2,\"b\":4}", 6)
	, ("[[[3]]]", 3)
	, ("{\"a\":{\"b\":4},\"c\":-1}", 3)
	, ("{\"a\":[-1,1]}", 0)
	, ("[-1,{\"a\":1}]", 0)
	, ("[]", 0)
	, ("{}", 0) ]
day12 :: [String] -> [Int]
day12 = map $ sum . unfoldr (listToMaybe.concatMap reads.tails)
--day12 = unfoldr (listToMaybe.concatMap (reads :: String -> [(Int, String)]).tails).concat

test_day12_2 = noTest
day12_2 = empty


-- Problem description: http://adventofcode.com/day/13 
test_day13 = once $ day13
	[ "Alice would gain 54 happiness units by sitting next to Bob."
	, "Alice would lose 79 happiness units by sitting next to Carol."
	, "Alice would lose 2 happiness units by sitting next to David."
	, "Bob would gain 83 happiness units by sitting next to Alice."
	, "Bob would lose 7 happiness units by sitting next to Carol."
	, "Bob would lose 63 happiness units by sitting next to David."
	, "Carol would lose 62 happiness units by sitting next to Alice."
	, "Carol would gain 60 happiness units by sitting next to Bob."
	, "Carol would gain 55 happiness units by sitting next to David."
	, "David would gain 46 happiness units by sitting next to Alice."
	, "David would lose 7 happiness units by sitting next to Bob."
	, "David would gain 41 happiness units by sitting next to Carol." ]
	== 330
day13 = (\h -> maximum.map (happiness h).permutations.nub.map (\(a,_,_) -> a) $ h).map (parse.words)
	where
	parse :: [String] -> (String, String, Int)
	parse ls = (head ls, init (last ls), read (ls!!3) * (if ls!!2 == "gain" then 1 else (-1)))

	happiness :: [(String, String, Int)] -> [String] -> Int
	happiness h a = sum.map (value h).findNeighbors 0 $ a
		where
		findNeighbors i ls
			| i == 0 = (ls!!i, last ls, ls!!(i+1)) : findNeighbors (i+1) ls
			| i == length ls-1 = [(ls!!i, ls!!(i-1), head ls)]
			| otherwise = (ls!!i, ls!!(i-1), ls!!(i+1)) : findNeighbors (i+1) ls
		value h (a,b,c) = relationValue a b + relationValue a c
			where relationValue a b = (\(Just (_,_,v)) -> v).find (\(a',b',_) -> a == a' && b == b') $ h

test_day13_2 = noTest
day13_2 = empty


-- Problem description: http://adventofcode.com/day/14 
test_day14 = once $ day14
	[ "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
	, "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds." ]
	== 1120
day14 = maximum.map (distance.(\ls -> map (read.(ls !!)) [3,6,13]).words)
	where
	distance [speed, time, rest] = a * time * speed + min b time * speed
		where (a,b) = totalTime `divMod` (time+rest)
	totalTime = 1000

test_day14_2 = noTest
day14_2 = empty


-- Problem description: http://adventofcode.com/day/15 
test_day15 = once $ day15
	[ "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8"
	, "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3" ]
	== 62842880
day15 = maximum.f.transpose.map ((\a -> map (read.init.(a!!)) [2,4,6,8]).words)
	where
	f :: [[Int]] -> [Int]
	f ls = map (product.map (max 0.sum)).transpose.zipWith f' ls.repeat $ combinations (length ls) 100
		where f' a = map (zipWith (*) a)

	combinations :: Int -> Int -> [[Int]]
	combinations 1 total = [[total]]
	combinations n total = [ i:ls | i <- [0..total], ls <- combinations (n-1) (total-i)]

test_day15_2 = noTest
day15_2 = empty

-- Problem description: http://adventofcode.com/day/16 
test_day16 = once $ day16
	[ "_ _ _ _ _ _ _ _ _ _"
	, "3 _ _ _ _ _ _ _ _ 1"
	, "_ _ _ 1 1 1 _ _ _ _"
	, "4 7 2 _ _ _ _ _ _ _" ]
	== [0,1]
day16 = map fst.filter (and.zipWith f known.snd).zip [0..].map (map f'.words)
	where
	f a (Just b) = a == b
	f _ _  = True

	f' "_" = Nothing
	f' a = Just (read a :: Int)

	known = [3,7,2,3,0,0,5,3,2,1]

test_day16_2 = noTest
day16_2 = empty


-- Problem description: http://adventofcode.com/day/17 
test_day17 = once $ day17 ["150"] == [297]
day17 = map $ length.f [5,10,15,20].read
	where
	f ls 0 = [[]]
	f ls total = [ i:ls' | i <- filter (<=total) ls, ls' <- f (filter (<=i) ls) (total-i)]

test_day17_2 = noTest
day17_2 = empty


-- Problem description: http://adventofcode.com/day/18 
test_day18 = once $ day18
	[ ".#.#.#"
	, "...##."
	, "#....#"
	, "..#..."
	, "#.#..#"
	, "####.." ]
	== 4
--day18 = format.map (map snd).flip (!!) 4.iterate f.parse
day18 = length.filter (toBool.snd).concat.flip (!!) 4.iterate f.parse
	where
--	format = foldr1 (\a b -> a ++ "\n" ++ b)

	parse :: [String] -> [[((Int,Int), Char)]]
	parse = map (\(i,ls) -> zipWith (\a s -> ((i,a),s)) [zero..] ls).zip [zero..]
		where zero = 0 :: Int

	f :: [[((Int,Int), Char)]] -> [[((Int,Int), Char)]]
	f state = map (map (\s -> f' (neighbors (fst s) state) s)) state
		where
		f' 2 s@(_,'#') = s
		f' 3 s@(_,'#') = s
		f' 3 (a, '.') = (a, '#')
		f' _ (a,_) = (a, '.')

	neighbors :: (Int,Int) -> [[((Int,Int), Char)]] -> Int
	neighbors (a,b) ls = length.filter (toBool.snd).catMaybes $ possibles
		where
		possibles = [find ((==(x,y)).fst) ls' | x <- [a-1..a+1], y <- [b-1..b+1], (x,y) /= (a,b)]
		ls' = concat ls

	toBool '.' = False
	toBool '#' = True

test_day18_2 = noTest
day18_2 = empty


-- Problem description: http://adventofcode.com/day/19 
test_day19 = once $ day19
	[ "H => HO"
	, "H => OH"
	, "O => HH" ]
	== 4
day19 = length.nub.f.map ((\[a,_,b] -> (a,b)).words)
	where
	f = filter isJust.concatMap (flip map (splitStep bottom).match)

	isJust Nothing = False
	isJust _ = True

	match (a,b) (s,ls)
		| a `isPrefixOf` ls = Just $ s ++ b ++ foldr delete ls a
		| otherwise = Nothing

	splitStep ls = map (`splitAt` ls) [0..length ls]

	bottom = "HOH"

test_day19_2 = noTest
day19_2 = empty


-- Problem description: http://adventofcode.com/day/20 
test_day20 = once $ day20 (map show [1..10]) == [1,2,2,3,4,4,4,6,6,6]
day20 = map (\n -> (\(Just (a,_)) -> a).find ((read n <=).snd).zip [1..].map f $ [1..])
	where
	f :: Int -> Int
	f n = sum.mapMaybe (f' n).take n $ [([elf,elf*2..], elf) | elf <- [1..]]
		where
		f' n (a:ls, v)
			| n == a = Just v
			| a > n = Nothing
			| otherwise = f' n (ls, v)

test_day20_2 = noTest
day20_2 = empty


-- Problem description: http://adventofcode.com/day/21 
test_day21 = once $ day21 ["12 7 2"] == [8]
day21 = map $ minimum.catMaybes.flip map comb.testWinn.map read.words
	where
	testWinn [b_hp, b_at, b_def] [p_gold, p_at, p_def]
		| winnTurn < loseTurn = Just p_gold
		| otherwise = Nothing
		where
		winnTurn = snd $ until ((<=0).fst) (\(hp,i) -> (hp - damage p_at b_def, i+1)) (b_hp, 0)
		loseTurn = snd $ until ((<=0).fst) (\(hp,i) -> (hp - damage b_at p_def, i+1)) (100,0)

		damage at def = max 1 (at-def)

	comb = [zipWith4 (\a b c d -> a+b+c+d) w a r1 r2 | w <- weapon, a <- armour, r1 <- ring, r2 <- ring] :: [[Int]]

	weapon = [[8,4,0], [10,5,0], [25,6,0], [40,7,0], [74,8,0]] :: [[Int]]
	armour = [[0,0,0], [13,0,1], [31,0,2], [53,0,3], [75,0,4], [102,0,5]] :: [[Int]]
	ring = [[0,0,0], [25,1,0], [50,2,0], [100,3,0], [20,0,1], [40,0,2], [80,0,3]] :: [[Int]]

test_day21_2 = noTest
day21_2 = empty


-- Problem description: http://adventofcode.com/day/22 
test_day22 = noTest
day22 = empty

test_day22_2 = noTest
day22_2 = empty


-- Problem description: http://adventofcode.com/day/23 
test_day23 = once $ day23
	[ "inc b"
	, "jio b, +2"
	, "tpl b"
	, "inc b" ]
	== 2
day23 = f 0 0 0.map (parse.words.delete ',')
	where
	parse (a:b:ls) = case a of
		"hlf" -> HLF (b == "a")
		"tpl" -> TPL (b == "a")
		"inc" -> INC (b == "a")
		"jmp" -> JMP (read' b)
		"jie" -> JIE (b == "a") (read' (head ls))
		"jio" -> JIO (b == "a") (read' (head ls))
		_ -> error $ "Not supported instruction: " ++ a
		where read' x = let r = read (tail x) in if head x == '+' then r else negate r


	f i regA regB instr = if i >= length instr then regB else case instr!!i of
		HLF a -> if a then change (regA `div` 2) regB else change regA (regB `div` 2)
		TPL a -> if a then change (regA * 3) regB else change regA (regB * 3)
		INC a -> if a then change (regA + 1) regB else change regA (regB + 1)
		JMP j -> jump j
		JIE a j -> let f' x = if even x then jump j else step in if a then f' regA else f' regB
		JIO a j -> let f' x = if odd x then jump j else step in if a then f' regA else f' regB
		where
		step = f (i+1) regA regB instr
		jump j = f (i+j) regA regB instr
		change regA regB = f (i+1) regA regB instr

data InstrType = HLF Bool | TPL Bool | INC Bool | JMP Int | JIE Bool Int | JIO Bool Int

test_day23_2 = noTest
day23_2 = empty


-- Problem description: http://adventofcode.com/day/24 
test_day24 = noTest
day24 = empty

test_day24_2 = noTest
day24_2 = empty


-- Problem description: http://adventofcode.com/day/25 
test_day25 = once $ testWithPairs day25
	[ ("1 1", 20151125)
	, ("1 2", 18749137)
	, ("1 3", 17289845)
	, ("1 4", 30943339)
	, ("1 5", 10071777)
	, ("1 6", 33511524)
	, ("2 1", 31916031)
	, ("2 2", 21629792)
	, ("2 3", 16929656)
	, ("2 4", 7726640)
	, ("2 5", 15514188)
	, ("2 6", 4041754)
	, ("3 1", 16080970)
	, ("3 2", 8057251)
	, ("3 3", 1601130)
	, ("3 4", 7981243)
	, ("3 5", 11661866)
	, ("3 6", 16474243)
	, ("4 1", 24592653)
	, ("4 2", 32451966)
	, ("4 3", 21345942)
	, ("4 4", 9380097)
	, ("4 5", 10600672)
	, ("4 6", 31527494)
	, ("5 1", 77061)
	, ("5 2", 17552253)
	, ("5 3", 28094349)
	, ("5 4", 6899651)
	, ("5 5", 9250759)
	, ("5 6", 31663883)
	, ("6 1", 33071741)
	, ("6 2", 6796745)
	, ("6 3", 25397450)
	, ("6 4", 24659492)
	, ("6 5", 1534922)
	, ("6 6", 27995004) ]
day25 = map $ f.(\[r,c] -> toIndex r c).map read.words
	where
	toIndex 1 1 = 1
	toIndex row 1 = 1 + toIndex 1 (row-1)
	toIndex row column = 1 + toIndex (row+1) (column-1)

	f 1 = 20151125
	f n = f (n-1) * 252533 `rem` 33554393

test_day25_2 = noTest
day25_2 = empty


empty :: [String] -> String
empty a = error "Sorry, not done yet"

noTest :: Property
noTest = undefined
--noTest = counterexample "Not implemented yet" $ property failed
--noTest = discard

testWithPairs :: Eq b => ([a] -> [b]) -> [(a, b)] -> Bool
testWithPairs d = (\(a,b) -> d a == b) . unzip

