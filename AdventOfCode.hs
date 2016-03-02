module AdventOfCode where

-- Tests
import Test.QuickCheck hiding ((.&.))
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


prop_day1 = False
day1 = let f '(' = 1; f ')' = -1 in map $ sum.map f

prop_day1_2 = False
day1_2 = let f '(' = 1; f ')' = -1 in map $ length.takeWhile (/=(-1)).scanl (+) 0.map f


prop_day2 = False
day2 = map $ f.map read.words.map (\c -> if c == 'x' then ' ' else c)
	where f [l,w,h] = let a = [l*w, w*h, h*l] in minimum a + sum (map (*2) a)

prop_day2_2 = False
day2_2 = map $ f.map read.words.map (\c -> if c == 'x' then ' ' else c)
	where f [l,w,h] = l*w*h + l+l+w+w


prop_day3 = False
day3 = map $ length.nub.scanl (\(a,b) (a',b') -> (a+a', b+b')) (0,0).map f
	where f x = case x of
		'^' -> ( 0,  1)
		'v' -> ( 0, -1)
		'>' -> ( 1,  0)
		'<' -> (-1,  0)

prop_day3_2 = False
day3_2 = map $ length.nub.foldr1 (++).map (f'.map snd).f''.zip [0..].map f
	where
	f'' x = [filter (even.fst) x, filter (odd.fst) x]
	f' = scanl (\(a,b) (a',b') -> (a+a', b+b')) (0,0)
	f x = case x of
		'^' -> ( 0,  1)
		'v' -> ( 0, -1)
		'>' -> ( 1,  0)
		'<' -> (-1,  0)


prop_day4 = False
day4 = map $ f (map show [1 :: Int ..])
	where
	f :: [String] -> String -> String
	f (a:ls) x = if take 5 r == "00000" then a else f ls x
		where r = md5s (Str (x++a))

prop_day4_2 = False
day4_2 = map $ f (map show [1 :: Int ..])
	where
	f :: [String] -> String -> String
	f (a:ls) x = if take 6 r == "000000" then a else f ls x
		where r = md5s (Str (x++a))


prop_day5 = False
day5 = map $ and.pam [notNaughty, nice, nice']
	where
	notNaughty = not.or.flip map sublists.flip isInfixOf
		where sublists = ["ab","cd","pq","xy"]
	nice = (>= 3).length.intersect "aeiou"
	nice' = (>= 2).maximum.map length.group

	pam :: [a -> b] -> a -> [b]
	pam f x = map g f
		where g h = h x

prop_day5_2 = False
day5_2 = map $ and.(\x -> [f1 x, f2 x])
	where
	f1 :: String -> Bool
	f1 (a:b:ls) = ([a,b] `isInfixOf` ls) || f1 (b:ls)
	f1 _ = False

	f2 (a:b:c:ls) = (a == c) || f2 (b:c:ls)
	f2 _ = False


prop_day6 = False
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

prop_day6_2 = False
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


prop_day7 = False
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

prop_day7_2 = False
day7_2 = day7

type S = String
data Statement = NUMBER Int | NOT S | AND S S | OR S S | LSHIFT S Int | RSHIFT S Int


prop_day8 = False
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

prop_day8_2 = False
day8_2 = empty


prop_day9 = False
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

prop_day9_2 = False
day9_2 = empty


prop_day10 = False
day10 = map $ length.flip (!!) 40.iterate (foldr1 (++).map (\x -> show (length x) ++ [head x]).group)

prop_day10_2 = False
day10_2 = empty


prop_day11 = False
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

prop_day11_2 = False
day11_2 = empty


prop_day12 = False
day12 :: [String] -> [Int]
day12 = unfoldr (listToMaybe.concatMap reads.tails).concat
--day12 = unfoldr (listToMaybe.concatMap (reads :: String -> [(Int, String)]).tails).concat

prop_day12_2 = False
day12_2 = empty


prop_day13 = False
day13 = (\h -> maximum.map (happiness h).permutations.nub.map (\(a,_,_) -> a) $ h).map (parse.words)
	where
	parse :: [String] -> (String, String, Int)
	parse ls = (head ls, last ls, read (ls!!3) * (if ls!!2 == "gain" then 1 else (-1)))

	happiness :: [(String, String, Int)] -> [String] -> Int
	happiness h a = sum.map (value h).findNeighbors 0 $ a
		where
		findNeighbors i ls
			| i == 0 = (ls!!i, last ls, ls!!(i+1)) : findNeighbors (i+1) ls
			| i == length ls-1 = [(ls!!i, ls!!(i-1), head ls)]
			| otherwise = (ls!!i, ls!!(i-1), ls!!(i+1)) : findNeighbors (i+1) ls
		value h (a,b,c) = relationValue a b + relationValue a c
			where relationValue a b = (\(Just (_,_,v)) -> v).find (\(a',b',_) -> a == a' && b == b') $ h

prop_day13_2 = False
day13_2 = empty


prop_day14 = False
day14 = maximum.map (distance.(\ls -> map (read.(ls !!)) [3,6,13]).words)
	where
	distance [speed, time, rest] = a * time * speed + min b time * speed
		where (a,b) = totalTime `divMod` (time+rest)
	totalTime = 1000

prop_day14_2 = False
day14_2 = empty


prop_day15 = False
day15 = maximum.f.transpose.map ((\a -> map (read.init.(a!!)) [2,4,6,8]).words)
	where
	f :: [[Int]] -> [Int]
	f ls = map (product.map (max 0.sum)).transpose.zipWith f' ls.repeat $ combinations (length ls) 100
		where f' a = map (zipWith (*) a)

	combinations :: Int -> Int -> [[Int]]
	combinations 1 total = [[total]]
	combinations n total = [ i:ls | i <- [0..total], ls <- combinations (n-1) (total-i)]

prop_day15_2 = False
day15_2 = empty

prop_day16 = False
day16 = map fst.filter (and.zipWith f known.snd).zip [0..].map (map f'.words)
	where
	f a (Just b) = a == b
	f _ _  = True

	f' "_" = Nothing
	f' a = Just (read a :: Int)

	known = [3,7,2,3,0,0,5,3,2,1]

prop_day16_2 = False
day16_2 = empty


prop_day17 = False
day17 = map $ length.f [5,10,15,20].read
	where
	f ls 0 = [[]]
	f ls total = [ i:ls' | i <- filter (<=total) ls, ls' <- f (filter (<=i) ls) (total-i)]

prop_day17_2 = False
day17_2 = empty


prop_day18 = False
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

prop_day18_2 = False
day18_2 = empty


prop_day19 = False
day19 = nub.f.map ((\[a,_,b] -> (a,b)).words)
	where
	f = filter isJust.concatMap (flip map (splitStep bottom).match)

	isJust Nothing = False
	isJust _ = True

	match (a,b) (s,ls)
		| a `isPrefixOf` ls = Just $ s ++ b ++ foldr delete ls a
		| otherwise = Nothing

	splitStep ls = map (`splitAt` ls) [0..length ls]

	bottom = "HOH"

prop_day19_2 = False
day19_2 = empty


prop_day20 = False
day20 = map (\n -> (\(Just (a,_)) -> a).find ((read n <=).snd).zip [1..].map f $ [1..])
	where
	f :: Int -> Int
	f n = sum.mapMaybe (f' n).take n $ [([elf,elf*2..], elf) | elf <- [1..]]
		where
		f' n (a:ls, v)
			| n == a = Just v
			| a > n = Nothing
			| otherwise = f' n (ls, v)

prop_day20_2 = False
day20_2 = empty


prop_day21 = False
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

prop_day21_2 = False
day21_2 = empty


prop_day22 = False
day22 = empty

prop_day22_2 = False
day22_2 = empty


prop_day23 = False
day23 = f 0 0 0.map (parse.words.delete ',')
	where
	parse (a:b:ls) = case a of
		"hlf" -> HLF (b == "a")
		"tpl" -> TPL (b == "a")
		"inc" -> INC (b == "a")
		"jmp" -> JMP (read b)
		"jie" -> JIE (b == "a") (read (head ls))
		"jio" -> JIO (b == "a") (read (head ls))
		_ -> error $ "Not supported instruction: " ++ a

	f i regA regB instr = if i >= length instr then regB else case instr!!i of
		HLF a -> if a then change (regA `div` 2) regB else change regA (regB `div` 2)
		TPL a -> if a then change (regA * 3) regB else change regA (regB * 3)
		INC a -> if a then change (regA + 1) regB else change regA (regB + 1)
		JMP j -> jump j
		JIE a j -> let f' x = if even x then jump j else step in if a then f' regA else f' regB
		JIE a j -> let f' x = if odd x then jump j else step in if a then f' regA else f' regB
		where
		step = f (i+1) regA regB instr
		jump j = f (i+j) regA regB instr
		change regA regB = f (i+1) regA regB instr

data InstrType = HLF Bool | TPL Bool | INC Bool | JMP Int | JIE Bool Int | JIO Bool Int

prop_day23_2 = False
day23_2 = empty


prop_day24 = False
day24 = empty

prop_day24_2 = False
day24_2 = empty


prop_day25 = False
day25 = map $ f.(\[r,c] -> toIndex r c).map read.words
	where
	toIndex 1 1 = 1
	toIndex row 1 = 1 + toIndex 1 (row-1)
	toIndex row column = 1 + toIndex (row+1) (column-1)

	f 1 = 20151125
	f n = f (n-1) * 252533 `rem` 33554393

prop_day25_2 = False
day25_2 = empty


empty :: [String] -> String
empty a = error "Sorry, not done yet"


