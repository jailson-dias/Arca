module Main where

-- main :: IO ()
-- main = putStrLn "Hello, Haskell!"

{--
    import SFML.Window
    import 


    main = do
        desktopMode <- getDesktopMode
        fsModes <- getFullscreenModes
        
        putStrLn $ "Current desktop mode:\n\n" ++ show desktopMode
        putStrLn ""
        putStrLn $ "Fullscreen modes:"
        putStrLn ""
        mapM_ (\m -> putStrLn (show m) >> putStrLn "") fsModes
        
        let ctxSettings = Just $ ContextSettings 24 8 0 1 2 [ContextDefault]
        wnd <- createWindow (VideoMode 640 480 32) "SFML Haskell Demo" [SFDefaultStyle] ctxSettings
        loop wnd
        destroy wnd


    loop :: Window -> IO ()
    loop wnd = do
        evt <- waitEvent wnd
        case evt of
            Just SFEvtClosed -> return ()
            _ -> loop wnd
import SFML.Audio
import SFML.Graphics
import SFML.Graphics.CircleShape
import SFML.Window
import SFML.Utils
import SFML.Graphics.Sprite
import SFML.Graphics.Color

-- import SFML.System.Vector2

import Foreign.Ptr (nullPtr)


txtSize = 24


data DemoState = DemoState
    { xmouse :: Int
    , ymouse :: Int
    , key    :: String
    }


main = do
    let ctxSettings = Just $ ContextSettings 30 8 0 1 2 [ContextDefault]
    wnd <- createRenderWindow (VideoMode 640 480 32) "SFML Haskell Demo" [SFDefaultStyle] ctxSettings
    -- fontPath <- getDataFileName "Vera.ttf"
    -- fnt <- err $ fontFromFile fontPath
    txt <- err $ createText
    -- setTextFont txt fnt
    setTextCharacterSize txt txtSize
    setTextColor txt blue
    let ds = DemoState 0 0 ""
    loop wnd txt ds
    circle <- createCircleShape
    SFML.Graphics.setPosition circle 50 50
    -- circle 50 250
    -- Shape 50
    destroy txt
    -- destroy fnt
    destroy wnd

loop :: RenderWindow -> Text -> DemoState -> IO ()
loop wnd txt ds = do
    ret <- processEvt wnd ds
    case ret of
        Nothing -> return ()
        Just ds' -> do
            clearRenderWindow wnd $ Color 240 240 240 255
            setTextString txt $ "Mouse: " ++ show (xmouse ds') ++ ", " ++ show (ymouse ds')
            drawText wnd txt Nothing
            setTextString txt $ "Keyboard: " ++ key ds'
            let rs = renderStates { transform = (translation 0 $ 2 * fromIntegral txtSize) }
            drawText wnd txt $ Just rs
            -- sf::CircleShape shape(50);
            -- shape.setFillColor(sf::Color(100, 250, 50));
            -- window.draw(shape);
            display wnd
            loop wnd txt ds'


processEvt :: RenderWindow -> DemoState -> IO (Maybe DemoState)
processEvt wnd ds = do
    evt <- pollEvent wnd
    case evt of
        Just SFEvtClosed -> return Nothing
        Just (SFEvtMouseMoved x y) -> processEvt wnd $ ds { xmouse = x, ymouse = y }
        Just e@SFEvtKeyPressed{} -> processEvt wnd $ ds { key = show . code $ e }
        Nothing -> return . Just $ ds
        _ -> processEvt wnd ds

        --}

import Control.Exception
import System.IO
import System.IO.Error
import System.Process


-- definição dos tipos dos dados
type Jogadores = [Jogador]
type Nome = String
type Pontuacao = Int
type Vez = Int
type Tabela = [Char]
data Jogador = Jogador Nome Pontuacao
					deriving (Show, Read)


-- função que recebe uma String e retorna uma IO String
getString :: String -> IO String
getString str = do
			putStr str
			res <- getLine
			return res


main :: IO ()
main = inicio

-- função que inicia o programa
inicio :: IO ()
inicio = do
		{catch (ler_arquivo) tratar_erro;}
		where
			-- tenta ler o arquivo
			ler_arquivo = do
			{
				arq <- openFile "dados.txt" ReadMode; -- abre o arquivo para leitura
				dados <- hGetLine arq; -- ler o conteúdo do arquivo
				hClose arq; -- fecha o arquivo
				menu (read dados); -- passa os dados para a função menu
				return ()
			}
			tratar_erro erro = if isDoesNotExistError erro then do
			{
				-- se o arquivo NÃO existir, então cria o arquivo
				arq <- openFile "dados.txt" WriteMode; -- abre o arquivo para escrita
				hPutStrLn arq "[]"; -- escreve uma lista vazia no arquivo
				hClose arq; -- fecha o arquivo
				menu []; -- passa uma lista vazia para o menu
				return ()
			}
			else
				ioError erro


-- função que exibe o Menu
menu :: Jogadores -> IO Jogadores
menu dados = do
		system "cls" -- limpa a tela (windows somente)
		putStrLn "-------------------------------- Jogo da Velha --------------------------------"
		putStrLn "\nDigite 1 para cadastrar jogador"
		putStrLn "Digite 2 para jogar"
		putStrLn "Digite 3 para visualizar o ranking"
		putStrLn "Digite 0 para sair"
		putStr "Opção: "
		op <- getChar
		getChar -- descarta o Enter
		executarOpcao dados op


-- função para manipular a opção escolhida pelo usuário
executarOpcao :: Jogadores -> Char -> IO Jogadores
executarOpcao dados '1' = cadastrarJogador dados
executarOpcao dados '2' = prepararJogo dados
executarOpcao dados '0' = do
				putStrLn ("\nBye! Visite: www.GeeksBR.com ;-)\n")
				return dados
executarOpcao dados _ = do
				putStrLn ("\nOpção inválida! Tente novamente...")
				putStr "\nPressione <Enter> para voltar ao menu..."
				getChar
				menu dados


-- função responsável pelo cadastro de jogadores
cadastrarJogador :: Jogadores -> IO Jogadores
cadastrarJogador dados = do
				nome <- getString "\nDigite um nome de usuário: "
				if (existeJogador dados nome) then do
					putStrLn "\nEsse nome já existe, escolha outro."
					putStr "\nPressione <Enter> para continuar..."
					getChar
					menu dados
				else do
					arq <- openFile "dados.txt" WriteMode -- abre o arquivo para escrita
					hPutStrLn arq (show ((Jogador nome 0):dados))
					hClose arq -- fecha o arquivo
					putStrLn ("\nUsuário " ++ nome ++ " cadastrado com sucesso.")
					putStr "\nPressione <Enter> para continuar..."
					getChar
					menu ((Jogador nome 0):dados) -- retorna a nova lista para o menu


-- função que verifica se um jogador existe (o nome do jogador é único)
existeJogador :: Jogadores -> Nome -> Bool
existeJogador [] _ = False
existeJogador ((Jogador n p):xs) nome
			| (n == nome) = True
			| otherwise = existeJogador xs nome


-- função que prepara o início do jogo
prepararJogo :: Jogadores -> IO Jogadores
prepararJogo dados = do
			jogador1 <- getString "\nDigite o nome do primeiro jogador: "
			-- testa se o jogador1 existe
			if not (existeJogador dados jogador1) then do
				putStrLn "\nEsse jogador não existe!"
				putStr "\nPressione <Enter> para continuar..."
				getChar -- descarta o Enter
				menu dados
			else do
				jogador2 <- getString "\nDigite o nome do segundo jogador: "
				if not (existeJogador dados jogador2) then do
					putStrLn "\nEsse jogador não existe!"
					putStr "\nPressione <Enter> para continuar..."
					getChar -- descarta o Enter
					menu dados
				else do
					-- se chegou aqui, é porque os dois jogadores existem
					novoJogo dados jogador1 jogador2


-- função que inicia um novo jogo
novoJogo :: Jogadores -> Nome -> Nome -> IO Jogadores
novoJogo dados jogador1 jogador2 = do
					putStrLn ("\nIniciando o jogo \"" ++
							jogador1 ++ " vs " ++ jogador2 ++ "\" ... ")
					putStrLn ("\nOs quadrados que possuem números NÃO estão marcados.")
					putStrLn ("\n" ++ jogador1 ++ " será o \'X\' e " ++ jogador2 ++ " será o \'O\'. Vamos lá!!")
					getChar
					{-
						A configuração inicial do tabuleiro é
						['1', '2', '3', '4', '5', '6', '7', '8', '9']
						Numeração da esquerda para direita e de cima para baixo
						Exemplo:
								1 | 2 | 3
							   -----------
							   	4 | 5 | 6
							   -----------
							   	7 | 8 | 9
					-}
					-- passa os dados, a configuração inicial, os jogadores e uma flag que indica de quem é
					-- a vez: 0 quer dizer que a vez é do jogador1 e 1 quer dizer que a vez é do jogador2
					rodarJogo dados ['1', '2', '3', '4', '5', '6', '7', '8', '9'] jogador1 jogador2 0


-- função que exibe o tabuleiro do jogo da velha
-- recebe a lista de jogadores, a tabela, o nome do jogador1, do jogador2 e um inteiro indicando de quem é a vez
rodarJogo :: Jogadores -> Tabela -> Nome -> Nome -> Vez -> IO Jogadores
rodarJogo dados tabela jogador1 jogador2 vez = menu dados