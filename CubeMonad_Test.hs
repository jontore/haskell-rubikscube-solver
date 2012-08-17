module CubeMonad_Test where 
import CubeMonad
import Test.HUnit
import Control.Monad.State

c1 = Corner (0, 0, 0) [(C, 5), (F, 3), (B, 2)]
c2 = Corner (2, 0, 0) [(E, 5), (C, 4), (F, 2)]
c3 = Corner (0, 2, 0) [(A, 5), (D, 3), (B, 1)]
c4 = Corner (2, 2, 0) [(B, 5), (A, 4), (C, 1)]

e1 = Edge (1, 0, 0) [(A, 5), (E, 2)]
e2 = Edge (0, 1, 0) [(D, 5), (E, 3)]
e3 = Edge (2, 1, 0) [(D, 5), (B, 4)]
e4 = Edge (1, 2, 0) [(D, 5), (F, 1)]

testCube = Cube [c1, c2, c3, c4] [e1, e2, e3, e4]

rc1 = Corner (0, 0, 0) [(E, 5), (F, 3), (C, 2)]
rc2 = Corner (2, 0, 0) [(B, 5), (C, 4), (A, 2)]
rc3 = Corner (0, 2, 0) [(C, 5), (B, 3), (F, 1)]
rc4 = Corner (2, 2, 0) [(A, 5), (B, 4), (D, 1)]

re1 = Edge (1, 0, 0) [(D, 5), (B, 2)]
re2 = Edge (0, 1, 0) [(A, 5), (E, 3)]
re3 = Edge (2, 1, 0) [(D, 5), (F, 4)]
re4 = Edge (1, 2, 0) [(D, 5), (E, 1)]

resultCube = Cube [rc1, rc2, rc3, rc4] [re1, re2, re3, re4]
initCube = Cube [] []

addNewCubeTest = do 
    TestCase $ assertEqual
        "Adds new cube to the monad"
        (testCube)
        (execState (setCube testCube >> getCube) initCube)

testTurnCube = do 
    TestCase $ assertEqual
        "Turns a surface on the cube"
         (Cube [rc1, rc2, rc3, rc4] [])
         (evalState (turnCube (0,0,0) 90 'Z') testCube)

main = runTestTT $ TestList [addNewCubeTest, testTurnCube]
