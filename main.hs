module Main where

type Posicao = (Int, Int)
type Tabuleiro = [[Char]]

expandirLinha :: String -> String
expandirLinha linha = concatMap (\c -> if c == '8' then replicate 8 ' ' else [c]) linha

encontraReiBranco :: Tabuleiro -> Maybe Posicao
encontraReiBranco tabuleiro = buscaReiBranco (zip [0..] $ map expandirLinha tabuleiro)
  where
    buscaReiBranco [] = Nothing
    buscaReiBranco ((y, linha):linhas) =
      case buscaNaLinha linha 0 y of
        Just pos -> Just pos
        Nothing -> buscaReiBranco linhas
    buscaNaLinha [] _ _ = Nothing
    buscaNaLinha (c:cs) x y
      | c == 'R' = Just (x, y)
      | otherwise = buscaNaLinha cs (x + 1) y

podeCapturarRei :: Tabuleiro -> Posicao -> Bool
podeCapturarRei tabuleiro (rx, ry) =
  let tabuleiroExpandido = map expandirLinha tabuleiro
      verificaPeca x y = let peca = tabuleiroExpandido !! y !! x in peca `elem` ['t', 'b', 'c']
      posicoesPecas = [(x, y) | x <- [0..7], y <- [0..7], verificaPeca x y]
  in any (\pos -> podeMoverPara tabuleiroExpandido pos (rx, ry)) posicoesPecas

podeMoverPara :: Tabuleiro -> Posicao -> Posicao -> Bool
podeMoverPara t (px, py) (rx, ry) =
  let peca = t !! py !! px in
    case peca of
      't' -> px == rx || py == ry -- Torre
      'b' -> abs (px - rx) == abs (py - ry) -- Bispo
      'c' -> (abs (px - rx) == 2 && abs (py - ry) == 1) || (abs (px - rx) == 1 && abs (py - ry) == 2) -- Cavalo
      _ -> False

xeque :: Tabuleiro -> Bool
xeque tabuleiro = case encontraReiBranco (map expandirLinha tabuleiro) of
  Just reiPos -> podeCapturarRei tabuleiro reiPos
  Nothing -> False

main :: IO ()
main = do
  let tabuleiro = ["tcbdrbct","pppppppp","8","8","8","8","PPPPPPPP","TCBDRBCT"]
  putStrLn $ if xeque tabuleiro then "O rei está em xeque." else "O rei não está em xeque."
