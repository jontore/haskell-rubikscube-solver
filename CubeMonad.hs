module CubeMonad where
import Control.Monad.State
import Data.List

data Cube = Cube {
    corners :: [Corner],
    edges :: [Edge]
} deriving (Eq, Ord, Show)

data Corner = Corner {
     cPosition :: (Int, Int, Int),
     cFaces :: [(Color, Int)]
} deriving (Eq, Ord, Show)

data Edge = Edge {
    ePositon :: (Int, Int, Int),
    eFaces :: [(Color, Int)]
} deriving (Eq, Ord, Show)
    
data Color = A | B | C | D | E | F deriving (Eq, Ord, Show)

type CubeState a = State Cube a
    
setCube :: Cube -> CubeState ()
setCube cube = modify $ \st -> st {corners = corners cube, 
                                   edges = edges cube}

getCube :: CubeState Cube 
getCube = get >>= \st -> return $ st

comparePosition :: (Int, Int, Int) -> Char -> Int ->  Bool
comparePosition (_, _, z) 'Z' comp = comp == z
comparePosition (x, _, _) 'X' comp = comp == x

compareOn :: Corner -> Corner -> Char -> Bool
compareOn a b 'X' = do
    let (x1, _, _) = cPosition a
    let (x2, _, _) = cPosition b
    x1 == x2
compareOn a b 'Y' = do
    let (_, y1, _) = cPosition a
    let (_, y2, _) = cPosition b
    y1 == y2

updateCorners :: [Corner] -> [Corner] -> [Corner]
updateCorners (x:xs) (y:ys) = (Corner (cPosition y) (updateFaces (cFaces x) (cFaces y))) : updateCorners xs ys
updateCorners [] [] = []

updateFaces :: [(Color, Int)] -> [(Color, Int)] -> [(Color, Int)]
updateFaces (x:xs) (y:ys) = (fst x, snd y) : updateFaces xs ys
updateFaces [] [] = []

turnMatrix :: [[Corner]] -> [[Corner]]
turnMatrix xs = reverse $ transpose xs

turnCube :: (Int, Int, Int) -> Int -> Char -> CubeState Cube
turnCube (x, y, z) 90 'Z' = do
    corners <- get >>= \st -> return $ corners st
    let co = filter (\elem -> comparePosition (cPosition elem) 'Z' z) corners
    let matrix = groupBy (\a b -> compareOn a b 'Y') co
    let turned = concat $ turnMatrix matrix
    let updatedCorners = updateCorners turned co 
    return $ Cube updatedCorners []
turnCube _ _ _ = return $ Cube [] []
