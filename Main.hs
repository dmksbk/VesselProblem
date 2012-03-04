{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Data.List
import Data.Maybe

-- | Search problem class from
--   http://fprog.ru/lib/martin-erwig-escape-from-zurg/
type Space m s = [([m],s)]
type Strategy m s = Space m s -> Space m s -> Space m s

class SearchProblem s m where

  trans :: s -> [(m,s)]
  isSolution :: ([m],s) -> Bool
  space, solutions :: Strategy m s -> s -> Space m s

  space f s = expand f (step ([],s))
    where expand f [] = []
          expand f (s:ss) = s:expand f (f (step s) ss)
          step (ms,s) = [(ms++[m],t) | (m,t) <- trans s]

  solutions f = filter isSolution . space f

dfs = (++)
bfs = flip dfs

-- | Data for vessel problem
data Vessel = Vessel {
	maxVolume	:: Int,
	curVolume	:: Int } deriving Eq
instance Show Vessel where
	show vs = intercalate "/" $ map ( show . ($ vs) ) [ curVolume, maxVolume]

data GameState = GameState {
	vessels		:: [Vessel],
	target		:: Int } deriving Eq
instance Show GameState where
	show = intercalate ", " . map show . vessels

-- Shortcut for generating initial state
initialState vols target =
	GameState [ Vessel v 0 | v <- vols ] target 

-- gets the index of vessel (if any) with target volume
getSolution	::  GameState -> Maybe Int
getSolution gs = findIndex
	(\ vs -> curVolume vs == target gs) $ vessels gs

-- is there at least one vessel with target volume
hasSolution	:: GameState -> Bool
hasSolution = isJust . getSolution

-- | All possible actions on vessels
data Action =
	Initial			|
	FillVessel	Int |
	EmptyVessel	Int |
	PourFromTo	Int	Int deriving Eq
instance Show Action where
	show Initial		  = cell "Initial"
	show (FillVessel n)   = cell $ "Fill "  ++ (show n)
	show (EmptyVessel n)  = cell $ "Empty " ++ (show n)
	show (PourFromTo i j) = cell $ (show i) ++ " -> " ++ (show j)

-- return a list of all possible actions in current state
getActions	:: GameState -> [Action]
getActions gs =
	-- Fill all non full vessels
	[ FillVessel  i  |
		i <- [0 .. n-1],
		curVolume(ves !! i) < maxVolume(ves !! i) ] ++
	-- empty all nonempty vessels
	[ EmptyVessel i  |
		i <- [0 .. n-1],
		curVolume(ves !! i) > 0 ] ++
	-- pour from all possible nonempty to all possible nonfull vessels
	[ PourFromTo i j |
		i <- [0 .. n-1] ,
		j <- [0 .. n-1],
		i /= j,
		curVolume(ves !! i) > 0,
		curVolume(ves !! j) < maxVolume(ves !! j) ]
	where
		ves = vessels gs
		n = length ves

-- helper function for doAction
setOneTrue :: Int -> GameState -> [Bool]
setOneTrue i state
	| i >= n || i < 0 || n < 0 = error "Wrong parameters in setOneTrue"
	| otherwise = [ i == j | j <- [0 .. n-1 ] ]
	where
		n = length . vessels $ state

-- | Perform action
doAction	:: Action -> GameState -> GameState
doAction Initial state = state
doAction (FillVessel i) state =
	state { vessels = zipWith fill (setOneTrue i state) (vessels state) }
	where
		fill False vs = vs
		fill True vs = vs { curVolume = maxVolume vs }

doAction (EmptyVessel i) state =
	state { vessels = zipWith fill (setOneTrue i state) (vessels state) }
	where
		fill False vs = vs
		fill True vs = vs { curVolume = 0 }

doAction (PourFromTo i j) state
	| i == j = state
	| otherwise =
		state { vessels = zipWith upd [0..] ves}
		where
			ves = vessels state		-- vessels array
			fv = ves !! i			-- vessel for pouring from
			fc = curVolume fv		-- current volume of this vessel
			fm = maxVolume fv		-- maximum volume of this vessel
			tv = ves !! j			-- vessel for pouring to
			tc = curVolume tv		-- current volume of this vessel
			tm = maxVolume tv		-- maximum volume of this vessel
			vl =  min fc (tm - tc)	-- poured volume
			fn = fc - vl			-- new volume of 'from' vessel
			tn = tc + vl			-- new volume of 'to' vessel
			upd n vs				-- update function
				| i == n = vs { curVolume = fn }
				| j == n = vs { curVolume = tn }
				| otherwise = vs

instance SearchProblem GameState Action where
	trans s = map ( \act -> (act, doAction act s) ) (getActions s)
	isSolution = hasSolution . snd

getSolutions	:: GameState -> Space Action GameState
getSolutions = solutions bfs 

-- | Formatting result
cellWidth = 8 :: Int
cell	:: String -> String
cell str
	| length str > cellWidth = take cellWidth $ repeat '='
	| otherwise = (take before fill) ++ str ++ (take after fill)
	where
		fill = repeat ' '
		spaces = cellWidth - length str
		before = (cellWidth - length str) `div` 2
		after = if odd spaces then before + 1 else before

showSolution :: GameState -> ([Action], GameState) -> IO ()
showSolution init (acts, targ) = do
	putStrLn $ "Target is " ++ (show . target $ targ)
	-- Head of the table
	putStrLn hLine
	putStrLn $
		"|" ++ (cell "Action") ++ "|" ++
		(intercalate "|" . map (cell . show) $ [0 .. (length . vessels $ init) - 1 ] ) ++ "|"
	putStrLn hLine

	-- Print each step
	showSolutionSteps init (Initial:acts)

	putStrLn hLine
	-- Say about result
	putStrLn $
		"Target achieved at vessel #" ++
		(show . fromJust . getSolution $ targ)

	where
		hLine = take ((cellWidth + 1) * ((length . vessels $ init) + 1) + 1) $ repeat '-'

		showSolutionSteps	:: GameState -> [Action] -> IO ()
		showSolutionSteps state [] = return ()
		showSolutionSteps state (act:acts) = do
			putStrLn $
				"|" ++ (cell . show $ act) ++ "|" ++
				(intercalate "|" [ cell . show $ v | v <- vessels newState ] )
				++ "|"
			showSolutionSteps newState acts
			where
				newState= doAction act state

main =
	showSolution s ( head . getSolutions $ s )
	where
		s = initialState [5, 8] 2
