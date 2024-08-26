import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random (randomRIO)
import System.Exit (exitSuccess)

-- Definindo o estado do jogo
data Player = Player
  { position :: (Float, Float)  -- Posição do jogador
  , lives :: Int                -- Vidas do jogador
  , isActive :: Bool            -- Se o jogador está ativo (é a vez de atirar)
  , angle :: Float              -- Ângulo do disparo
  , force :: Float              -- Força do disparo
  }

data GameState = GameState
  { player1 :: Player           -- Estado do jogador 1
  , player2 :: Player           -- Estado do jogador 2
  , projectilePos :: (Float, Float) -- Posição atual do projétil
  , projectileActive :: Bool    -- Se o projétil está em movimento
  , timeElapsed :: Float        -- Tempo decorrido desde o lançamento
  }

-- Configurações da janela
windowWidth, windowHeight :: Int
windowWidth = 1200
windowHeight = 800

initialLives :: Int
initialLives = 3

-- Estado inicial do jogo
initialState :: IO GameState
initialState = do
  p1Pos <- randomRIO (-600, -200) -- Restrição para o lado esquerdo
  p2Pos <- randomRIO (200, 600)   -- Restrição para o lado direito
  return GameState
    { player1 = Player (p1Pos, -250) initialLives True 45 100 -- Diminuindo a força inicial
    , player2 = Player (p2Pos, -250) initialLives False 45 100 -- Diminuindo a força inicial para o jogador 2
    , projectilePos = (p1Pos, -250)
    , projectileActive = False
    , timeElapsed = 0
    }

-- Função principal
main :: IO ()
main = do
  state <- initialState
  playIO window background 60 state render handleInput update

-- Configuração da janela e fundo
window :: Display
window = InWindow "Combate de Alvos" (windowWidth, windowHeight) (100, 100)

background :: Color
background = white

-- Renderizando o jogo
render :: GameState -> IO Picture
render game = return $ pictures
  [ drawGround
  , drawPlayer (player1 game) red
  , drawPlayer (player2 game) blue
  , if projectileActive game then drawProjectile (projectilePos game) else Blank
  , drawLives (player1 game) (-350, 250)
  , drawLives (player2 game) (350, 250)
  ]

-- Desenha o chão cinza
drawGround :: Picture
drawGround = translate 0 (-350) $ color (greyN 0.5) $ rectangleSolid (fromIntegral windowWidth) 200

-- Desenha o jogador com uma base fixa (trapézio) e um canhão rotacionável (retângulo)
drawPlayer :: Player -> Color -> Picture
drawPlayer player playerColor =
  let (x, y) = position player
      cannonAngle = if x < 0 then -angle player else angle player - 180
      baseVertices = [(-30, -15), (30, -15), (20, 15), (-20, 15)] -- Vértices para desenhar um trapézio
      base = color playerColor $ polygon baseVertices -- Base do trapézio
      cannon = rotate cannonAngle $ translate 30 0 $ color playerColor $ rectangleSolid 40 10 -- Cano do canhão que rotaciona
  in translate x y $ pictures [base, cannon]

drawProjectile :: (Float, Float) -> Picture
drawProjectile (x, y) = translate x y $ color black $ circleSolid 10

drawLives :: Player -> (Float, Float) -> Picture
drawLives player (x, y) =
  translate x y $ scale 0.3 0.3 $ color black $ text ("Lives: " ++ show (lives player))

-- Atualizando o estado do jogo
update :: Float -> GameState -> IO GameState
update _ game
  | projectileActive game = do
      let newPos = calculateProjectilePosition game
      if checkCollision newPos (position (targetPlayer game)) || outOfBounds newPos
        then do
          updatedGame <- if checkCollision newPos (position (targetPlayer game))
                            then hitTarget game
                            else return $ switchTurn game
          if lives (targetPlayer updatedGame) <= 0
            then exitSuccess >> return updatedGame -- Finaliza o jogo se o jogador perder todas as vidas
            else return updatedGame { projectileActive = False, timeElapsed = 0 }
        else return game { projectilePos = newPos, timeElapsed = timeElapsed game + 0.1 }
  | otherwise = return game

-- Calcula a nova posição do projétil
calculateProjectilePosition :: GameState -> (Float, Float)
calculateProjectilePosition game =
  let t = timeElapsed game
      player = currentPlayer game
      v = force player -- Usando a força do jogador atual
      g = 10.0 -- Gravidade ajustada para um movimento mais suave
      a = if position player == position (player2 game)
            then pi - angle player * pi / 180 -- Jogador 2 dispara para a esquerda
            else angle player * pi / 180 -- Jogador 1 dispara para a direita
      (x0, y0) = position player
      newX = x0 + v * t * cos a
      newY = y0 + v * t * sin a - 0.5 * g * t * t -- Gravidade ajustada
  in (newX, newY)

-- Verifica se o projétil saiu dos limites da tela
outOfBounds :: (Float, Float) -> Bool
outOfBounds (x, y) = x < -fromIntegral windowWidth / 2 || x > fromIntegral windowWidth / 2
                  || y < -fromIntegral windowHeight / 2 || y > fromIntegral windowHeight / 2

-- Verifica se houve colisão do projétil com o alvo
checkCollision :: (Float, Float) -> (Float, Float) -> Bool
checkCollision (projX, projY) (targetX, targetY) =
  let targetSize = 40
      projectileRadius = 10
      targetHitbox = (targetX - targetSize / 2, targetY - targetSize / 2, targetX + targetSize / 2, targetY + targetSize / 2)
  in rectCircleCollision (projX, projY) projectileRadius targetHitbox

-- Função para verificar colisão entre um círculo (projétil) e um retângulo (alvo)
rectCircleCollision :: (Float, Float) -> Float -> (Float, Float, Float, Float) -> Bool
rectCircleCollision (cx, cy) radius (left, bottom, right, top) =
  let closestX = clamp cx left right
      closestY = clamp cy bottom top
      distanceX = cx - closestX
      distanceY = cy - closestY
  in (distanceX * distanceX + distanceY * distanceY) < (radius * radius)

-- Função auxiliar para limitar valores
clamp :: Float -> Float -> Float -> Float
clamp x minVal maxVal = max minVal (min x maxVal)

-- Atualiza o estado do jogo quando um jogador é atingido
hitTarget :: GameState -> IO GameState
hitTarget game = do
  let target = targetPlayer game
  newTargetPos <- if isActive (player1 game)
                    then randomRIO (100, 300)  -- Restrição para jogador 2 na metade direita
                    else randomRIO (-300, -100) -- Restrição para jogador 1 na metade esquerda
  let updatedTarget = target { lives = lives target - 1, position = (newTargetPos, -250) }
      -- Mantendo o ângulo correto para o jogador 2 após perder uma vida
      finalTarget = if isActive (player1 game)
                    then updatedTarget
                    else updatedTarget { angle = 135 } -- Ângulo fixo para o jogador 2
  if isActive (player1 game)
    then return game { player2 = finalTarget, projectileActive = False }
    else return game { player1 = finalTarget, projectileActive = False }

-- Alterna a vez de cada jogador
switchTurn :: GameState -> GameState
switchTurn game =
  if isActive (player1 game)
    then game { player1 = (player1 game) { isActive = False }, player2 = (player2 game) { isActive = True }, projectilePos = position (player2 game) }
    else game { player1 = (player1 game) { isActive = True }, player2 = (player2 game) { isActive = False }, projectilePos = position (player1 game) }

-- Retorna o jogador atual (que está atirando)
currentPlayer :: GameState -> Player
currentPlayer game =
  if isActive (player1 game)
    then player1 game
    else player2 game

-- Retorna o jogador alvo (que está sendo atacado)
targetPlayer :: GameState -> Player
targetPlayer game =
  if isActive (player1 game)
    then player2 game
    else player1 game

-- Tratando a entrada do usuário
handleInput :: Event -> GameState -> IO GameState
handleInput (EventKey (SpecialKey KeySpace) Down _ _) game
  | not (projectileActive game) = do
      let shooter = currentPlayer game
      return game { projectileActive = True, timeElapsed = 0, projectilePos = position shooter }
handleInput (EventKey (SpecialKey KeyUp) Down _ _) game
  | isActive (currentPlayer game) && not (projectileActive game) = return $ updatePlayerAngle (currentPlayer game) game (-5)
handleInput (EventKey (SpecialKey KeyDown) Down _ _) game
  | isActive (currentPlayer game) && not (projectileActive game) = return $ updatePlayerAngle (currentPlayer game) game 5
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) game
  | isActive (currentPlayer game) && not (projectileActive game) = return $ updatePlayerForce (currentPlayer game) game (-10)
handleInput (EventKey (SpecialKey KeyRight) Down _ _) game
  | isActive (currentPlayer game) && not (projectileActive game) = return $ updatePlayerForce (currentPlayer game) game 10
handleInput (EventKey (Char 'q') Down _ _) game = 
  exitSuccess >> return game -- Encerra o jogo quando 'q' é pressionado
handleInput _ game = return game

-- Atualiza o ângulo do jogador ativo
updatePlayerAngle :: Player -> GameState -> Float -> GameState
updatePlayerAngle player game delta =
  let newAngle = if position player == position (player2 game)
                   then clamp (angle player - delta) 0 90 -- Inverte o controle para o jogador 2
                   else clamp (angle player + delta) 0 90
  in if isActive (player1 game)
       then game { player1 = player { angle = newAngle } }
       else game { player2 = player { angle = newAngle } }

-- Atualiza a força do jogador ativo
updatePlayerForce :: Player -> GameState -> Float -> GameState
updatePlayerForce player game delta =
  let newForce = clamp (force player + delta) 50 200 -- Reduzindo a força máxima
  in if isActive (player1 game)
       then game { player1 = player { force = newForce } }
       else game { player2 = player { force = newForce } }

