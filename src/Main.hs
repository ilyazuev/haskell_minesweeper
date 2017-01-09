module Main where

import Lib
import qualified Data.Map as M
import qualified Data.Set as S
import System.Random
import System.Random.Shuffle (shuffle')
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

main :: IO () -- main = someFunc
main=start
start=do
    -- let field = createField
    -- startGame field
    getStdGen>>=startGame

cellSize=24
fieldSize@(fieldWidth, fieldHeight)=(15, 15)::(Int, Int)
mineCount=40::Int
both f (a,b)=(f a,f b)

type Field=M.Map Cell CellState
type Cell=(Int,Int)
-- data CellState=Closed{hasMine,hasFlag::Bool}
--               |Opened{Maybe Int} -- Открыта; параметр — сколько у неё опасных соседей (и Nothing, если мина в ней самой)
data CellState=Opened Int
              |Mine
              |Flag

type Mines=S.Set Cell

data GameState=GS{field::Field
                 ,mines::Either StdGen Mines
                 ,gameOver::Bool -- сделать enum-ADT с состояниями "в процессе", "проиграли", "выиграли"
                 ,nextGen::StdGen
                 }

createField::Field
createField=M.empty

createMines::RandomGen g=>g->Cell->Mines
createMines g fst=S.fromList$ take mineCount$ shuffle g$ 
    [(i,j)|i<-[0..fieldWidth-1],j<-[0..fieldHeight-1],(i,j)/=fst]

shuffle::RandomGen g=>g->[a]->[a] -- shuffle'::RandomGen g=>[a]->Int->g->[a]
shuffle g l=shuffle' l (fieldWidth*fieldHeight-1) g

-- main=do print$ fieldWidth*fieldHeight; g<-getStdGen; print$ createMines g (2,2)

-- stack install random-shuffle                 
-- stack install gloss

-- play::
-- Display-> -- (1) параметры окна или полноэкранного режима
-- Color-> -- (2) цвет фона по умолчанию
-- Int-> -- (3) Некоторое число раз в секунду (задаваемое третьим параметром) мир может изменяться
-- world-> -- (4) Есть сцена, или некий внутренний «мир», состояние которого хранится в значении типа world
-- (world->Picture)-> -- (5) renderer: превращает «состояние мира» в картинку (назовём её отрисовщиком, или renderer)
-- (Event->world->world)-> -- (6) handler: принимающий событие, старое состояние и возвращающий новое состояние (её будем звать обработчиком, или handler).
-- (Float->world->world)-> -- (7) updater: принимающий текущее время, старое состояние и возвращающий новое (назовём её обновителем, или updater)
-- IO()

-- main=display(InWindow "Test title"(400,150)(10,10)) white picture
-- picture=Translate(-170)(-20) -- shift the text to the middle of the window
--         $ Scale 0.5 0.5 -- display it half the original size
--         $ Text "Test text"

startGame::StdGen->IO()
startGame gen=play(InWindow "Hsweeper" windowSize (240,160)) (greyN 0.25) 30 (initState gen) renderer handler updater

windowSize=both (*(round cellSize)) fieldSize

-- initState gen=GS createField (Left gen) False gen
initState gen=GS createField (Right$ createMines gen (-1,-1)) False$ snd$ split gen

updater _=id

-- renderer _=pictures[color white$ rectangleSolid cellSize cellSize]
-- renderer _=pictures[uncurry translate (cellToScreen (x,y))$ color white$ rectangleSolid cellSize cellSize|x<-[0..fieldWidth-1],y<-[0..fieldHeight-1]]
renderer GS{field=field}= 
    applyViewPortToPicture viewPort$ pictures$ cells ++ grid -- pictures$ [uncurry translate(cellToScreen(x,y))$ drawCell x y|x<-[0..fieldWidth-1],y<-[0..fieldHeight-1]]
    where grid=[uncurry translate(cellToScreen(x,y))$ color black$ rectangleWire cellSize cellSize|x<-[0..fieldWidth-1],y<-[0..fieldHeight-1]]
          cells=[uncurry translate(cellToScreen(x,y))$ drawCell x y|x<-[0..fieldWidth-1],y<-[0..fieldHeight-1]]
          drawCell x y=case M.lookup(x,y)field of
            Nothing->color white$ rectangleSolid cellSize cellSize -- клетка пустая
            Just Mine->pictures[color red$ rectangleSolid cellSize cellSize
                               ,label "@"] -- ,scale 0.15 0.15$ color black$ text "@"]
            Just(Opened n)->pictures[color green$ rectangleSolid cellSize cellSize
                                    ,label$ show n] -- ,scale 0.15 0.15$ color black$ text$ show n]
            Just Flag->pictures[color yellow$ rectangleSolid cellSize cellSize
                               ,label "?"] -- ,scale 0.15 0.15$ color black$ text "?"]

viewPort=ViewPort(both(negate.(/2).(subtract cellSize))$ cellToScreen fieldSize) 0 1 -- последние два параметра – это поворот и коэффициент масштабирования
label=translate(-5)(-5).scale 0.15 0.15.color black.text
cellToScreen=both((*cellSize).fromIntegral)
screenToCell=both(round.(/cellSize)).invertViewPort viewPort

handler(EventKey(MouseButton LeftButton) Down _ mouse) gs@GS{mines=Left gen}=
    gs{mines=Right$ createMines gen cell,nextGen=snd$ split gen} 
    where cell=screenToCell mouse
-- handler(EventKey(MouseButton LeftButton) Down _ mouse) gs@GS{field=field,mines=Right mines,gameOver=False}=
--     gs{field=M.insert cell opened field
--       ,gameOver=exploded} 
--     where cell@(cx,cy)=screenToCell mouse
--           (opened,exploded)=if cell`S.member`mines then (Mine,True) else (Opened neighbours,False)
--           neighbours=length[()|i<-[-1..1],j<-[-1..1],(i,j)/=(0,0),(cx+i,cy+j)`S.member`mines]
-- handler(EventKey(MouseButton LeftButton) Down _ mouse) gs@GS{gameOver=True}=

handler(EventKey(MouseButton LeftButton) Down _ mouse) gs@GS{gameOver=True,nextGen=gen}=
    initState gen
handler(EventKey(MouseButton LeftButton) Down _ mouse) gs@GS{field=field,mines=Right mines,gameOver=False}=
    gs{field=newField,gameOver=exploded}
    where
        newField=click cell field
        exploded=case M.lookup cell newField of -- Проигрыш, если последняя вскрытая клетка - мина
            Just Mine->True
            _->False
        cell@(cx,cy)=screenToCell mouse
        click::Cell->Field->Field
        click c@(cx,cy) f
            |c`M.member`f=f
            |c`S.member`mines=put Mine
            |otherwise=let nf=put(Opened neighbours)in
                if neighbours==0 then Prelude.foldr click nf neighbourCells
                                 else nf
            where put state=M.insert c state f
                  -- neighbourCells=[(cx+1,cy+1)|i<-[-1..1],y<-[-1..1]]
                  neighbourCells=[(i,j)|i<-[cx-1..cx+1],j<-[cy-1..cy+1]
                                 ,0<=i&&i<fieldWidth
                                 ,0<=j&&j<fieldHeight]
                  neighbours=length$ Prelude.filter(`S.member`mines)neighbourCells
handler(EventKey(MouseButton RightButton)Down _ mouse) gs@GS{field=field}=
    case M.lookup cell field of
        Nothing->gs{field=M.insert cell Flag field}
        Just Flag->gs{field=M.delete cell field}
        _->gs
        where cell=screenToCell mouse
handler _ gs=gs








