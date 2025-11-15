{-# LANGUAGE BangPatterns #-}

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time
import System.IO
import Control.Exception (catch, SomeException)
import Data.List (intercalate)
import System.IO.Unsafe (unsafePerformIO)

-- ============================================================================
-- TIPOS DE DADOS (Aluno 1: Arquiteto de Dados)
-- ============================================================================

-- | Representa um item no inventário com identificador, nome, quantidade e categoria
data Item = Item
  { itemID :: String      -- ^ Identificador único do item
  , nome :: String        -- ^ Nome descritivo do item
  , quantidade :: Int     -- ^ Quantidade em estoque
  , categoria :: String   -- ^ Categoria do item
  } deriving (Show, Read, Eq)

-- | Inventário como mapa de itemID para Item
type Inventario = Map String Item

-- | Tipo de ação realizada no sistema (ADT)
data AcaoLog = Add        -- ^ Adição de item
             | Remove     -- ^ Remoção de item
             | Update     -- ^ Atualização de quantidade
             | QueryFail  -- ^ Falha em consulta
             | ListItems  -- ^ Listagem de itens
             | Report     -- ^ Geração de relatório
  deriving (Show, Read, Eq)

-- | Status do resultado da operação (ADT)
data StatusLog = Sucesso           -- ^ Operação bem-sucedida
               | Falha String      -- ^ Operação falhou com mensagem
  deriving (Show, Read, Eq)

-- | Entrada no log de auditoria
data LogEntry = LogEntry
  { timestamp :: UTCTime    -- ^ Momento da operação
  , acao :: AcaoLog         -- ^ Tipo de ação executada
  , detalhes :: String      -- ^ Descrição detalhada
  , status :: StatusLog     -- ^ Resultado da operação
  } deriving (Show, Read, Eq)

  -- ============================================================================
-- LÓGICA DE NEGÓCIO PURA (Aluno 2: Lógica de Negócio)
-- ============================================================================

-- | Tipo para resultado de operação: novo inventário e entrada de log
type ResultadoOperacao = (Inventario, LogEntry)

-- | Adiciona um item ao inventário
-- Retorna Either com erro ou o novo estado e log
addItem :: UTCTime -> String -> String -> Int -> String -> Inventario 
        -> Either String ResultadoOperacao
addItem time iid nom qtd cat inv
  | qtd <= 0 = Left "Quantidade deve ser positiva"
  | Map.member iid inv = Left mensagemDuplicado
  | otherwise = Right (novoInv, logEntry)
  where
    -- Cria novo item com os dados fornecidos
    novoItem = Item iid nom qtd cat
    
    -- Insere item no inventário
    novoInv = Map.insert iid novoItem inv
    
    -- Mensagem para item duplicado (sem acentos)
    mensagemDuplicado = "Item com ID " ++ iid ++ " ja existe"
    
    -- Detalhes para o log
    detalhesMsg = "Adicionado: " ++ iid ++ " - " ++ nom ++ " (" ++ show qtd ++ ")"
    
    -- Entrada de log de sucesso
    logEntry = LogEntry time Add detalhesMsg Sucesso

-- | Remove uma quantidade de um item do inventário
-- Valida estoque suficiente antes de remover
removeItem :: UTCTime -> String -> Int -> Inventario 
           -> Either String ResultadoOperacao
removeItem time iid qtdRemover inv =
  case Map.lookup iid inv of
    Nothing -> Left $ "Item " ++ iid ++ " nao encontrado"
    Just item ->
      let
        qtdAtual = quantidade item
        validaQuantidade
          | qtdRemover <= 0 = Left "Quantidade a remover deve ser positiva"
          | qtdAtual < qtdRemover = Left mensagemEstoqueInsuficiente
          | otherwise = Right ()
          where
            mensagemEstoqueInsuficiente = 
              "Estoque insuficiente. Disponivel: " ++ show qtdAtual
      in
        case validaQuantidade of
          Left erro -> Left erro
          Right () ->
            let
              novaQtd = qtdAtual - qtdRemover
              itemAtualizado = item { quantidade = novaQtd }
              -- Remove item se quantidade chegar a zero
              novoInv = if novaQtd == 0
                       then Map.delete iid inv
                       else Map.insert iid itemAtualizado inv
              detalhesMsg = "Removido: " ++ iid ++ " - " ++ 
                           show qtdRemover ++ " unidades"
              logEntry = LogEntry time Remove detalhesMsg Sucesso
            in
              Right (novoInv, logEntry)

-- | Atualiza a quantidade de um item existente
-- Remove o item se a nova quantidade for zero
updateQty :: UTCTime -> String -> Int -> Inventario 
          -> Either String ResultadoOperacao
updateQty time iid novaQtd inv =
  case Map.lookup iid inv of
    Nothing -> Left ("Item " ++ iid ++ " nao encontrado")
    Just item ->
      if novaQtd < 0
        then Left "Quantidade nao pode ser negativa"
        else Right (novoInv, logEntry)
      where
        itemAtualizado = item { quantidade = novaQtd }
        -- Remove do inventário se quantidade for zero
        novoInv = if novaQtd == 0
                 then Map.delete iid inv
                 else Map.insert iid itemAtualizado inv
        detalhesMsg = "Atualizado: " ++ iid ++ 
                     " - nova quantidade: " ++ show novaQtd
        logEntry = LogEntry time Update detalhesMsg Sucesso

-- | Cria entrada de log para operação que falhou
-- Usa mensagens sem acentos para evitar problemas de encoding
criarLogFalha :: UTCTime -> AcaoLog -> String -> LogEntry
criarLogFalha time ac msg = LogEntry time ac msgSemAcentos (Falha msgSemAcentos)
  where
    msgSemAcentos = removerAcentos msg
    
    -- Remove acentos e caracteres especiais para evitar problemas de encoding
    removerAcentos :: String -> String
    removerAcentos = map substituir
      where
        substituir 'á' = 'a'
        substituir 'é' = 'e'
        substituir 'í' = 'i'
        substituir 'ó' = 'o'
        substituir 'ú' = 'u'
        substituir 'ã' = 'a'
        substituir 'õ' = 'o'
        substituir 'â' = 'a'
        substituir 'ê' = 'e'
        substituir 'ô' = 'o'
        substituir 'ç' = 'c'
        substituir 'Á' = 'A'
        substituir 'É' = 'E'
        substituir 'Í' = 'I'
        substituir 'Ó' = 'O'
        substituir 'Ú' = 'U'
        substituir 'Ã' = 'A'
        substituir 'Õ' = 'O'
        substituir 'Â' = 'A'
        substituir 'Ê' = 'E'
        substituir 'Ô' = 'O'
        substituir 'Ç' = 'C'
        substituir c = c
        
-- ============================================================================
-- FUNÇÕES DE ANÁLISE DE LOGS (Aluno 4: Validação e Documentação)
-- ============================================================================

-- | Filtra apenas os logs que representam erros
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter isErro
  where
    isErro (LogEntry _ _ _ (Falha _)) = True
    isErro _ = False

-- | Retorna histórico de operações que mencionam um item específico
historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem itemId = filter contemItem
  where
    contemItem (LogEntry _ _ det _) = itemId `elem` words det

-- | Identifica o item mais movimentado (mais menções nos logs)
itemMaisMovimentado :: [LogEntry] -> Maybe (String, Int)
itemMaisMovimentado logs =
  let
    -- Extrai IDs de itens dos logs
    items = concatMap extrairItems logs
    -- Conta ocorrências de cada item
    contagens = Map.toList $ Map.fromListWith (+) [(i, 1) | i <- items]
  in
    if null contagens
      then Nothing
      else Just $ foldr1 maiorContagem contagens
  where
    -- Extrai primeiro ID mencionado nos detalhes
    extrairItems (LogEntry _ _ det _) = 
      take 1 $ filter (not . null) $ words det
    -- Compara e retorna item com maior contagem
    maiorContagem x y = if snd x > snd y then x else y
-- ============================================================================
-- MÓDULO DE I/O E PERSISTÊNCIA (Aluno 3: I/O e Persistência)
-- ============================================================================

-- | Nome do arquivo de persistência do inventário
arquivoInventario :: FilePath
arquivoInventario = "Inventario.dat"

-- | Nome do arquivo de log de auditoria
arquivoLog :: FilePath
arquivoLog = "Auditoria.log"

-- | Cria inventário inicial com 10 itens para teste
criarInventarioInicial :: IO Inventario
criarInventarioInicial = do
  time <- getCurrentTime
  let itensIniciais = 
        [ Item "T001" "Teclado_Mecanico" 15 "Perifericos"
        , Item "M001" "Mouse_Optico" 25 "Perifericos"
        , Item "MON01" "Monitor_24pol" 8 "Perifericos"
        , Item "CPU01" "Processador_i7" 12 "Hardware"
        , Item "RAM01" "Memoria_16GB" 30 "Hardware"
        , Item "SSD01" "SSD_500GB" 20 "Armazenamento"
        , Item "HDD01" "HDD_2TB" 10 "Armazenamento"
        , Item "CAD01" "Cadeira_Gamer" 5 "Mobiliario"
        , Item "MES01" "Mesa_Escritorio" 3 "Mobiliario"
        , Item "CAB01" "Cabo_HDMI_2m" 50 "Acessorios"
        ]
  
  let inventario = Map.fromList [(itemID item, item) | item <- itensIniciais]
  
  -- Salva o inventário inicial
  salvarInventario inventario
  
  -- Cria logs para cada item adicionado
  mapM_ (\item -> 
    let logEntry = LogEntry time Add 
                   ("Adicionado: " ++ itemID item ++ " - " ++ nome item ++ 
                    " (" ++ show (quantidade item) ++ ")") 
                   Sucesso
    in adicionarLog logEntry) itensIniciais
  
  putStrLn "10 itens iniciais criados automaticamente!"
  return inventario

-- | Carrega o inventário do disco usando catch para tratar exceções
-- Retorna inventário vazio se arquivo não existir
carregarInventario :: IO Inventario
carregarInventario = do
  catch tentarCarregar tratarErro
  where
    tentarCarregar = do
      conteudo <- readFile arquivoInventario
      -- Força avaliação COMPLETA e ESTRITA do conteúdo
      let !conteudoAvaliado = conteudo
      length conteudoAvaliado `seq` return ()
      let conteudoLimpo = trim conteudoAvaliado
      if null conteudoLimpo
        then return Map.empty
        else do
          let !resultado = read conteudoLimpo :: Inventario
          return resultado
    
    tratarErro :: SomeException -> IO Inventario
    tratarErro _ = return Map.empty
    
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- | Carrega os logs de auditoria usando catch
-- Lê formato append-only (uma entrada por linha)
carregarLogs :: IO [LogEntry]
carregarLogs = do
  catch tentarCarregar tratarErro
  where
    tentarCarregar = do
      conteudo <- readFile arquivoLog
      -- Força leitura COMPLETA e ESTRITA do arquivo
      let !conteudoAvaliado = conteudo
      length conteudoAvaliado `seq` return ()
      let linhas = lines conteudoAvaliado
          -- Filtra linhas vazias e converte para LogEntry
          logsValidos = [read linha :: LogEntry | 
                        linha <- linhas, not (null linha)]
      -- Força avaliação de toda a lista
      length logsValidos `seq` return logsValidos
    
    tratarErro :: SomeException -> IO [LogEntry]
    tratarErro _ = return []

-- | Salva o inventário no disco usando writeFile
-- Sobrescreve arquivo anterior
salvarInventario :: Inventario -> IO ()
salvarInventario inv = do
  catch operacao tratarErro
  where
    operacao = writeFile arquivoInventario (show inv)
    tratarErro e = 
      putStrLn $ "Erro ao salvar inventário: " ++ show (e :: SomeException)

-- | Adiciona entrada ao log usando appendFile (modo append-only)
-- Cada entrada em uma nova linha
adicionarLog :: LogEntry -> IO ()
adicionarLog entry = do
  catch operacao tratarErro
  where
    operacao = appendFile arquivoLog (show entry ++ "\n")
    tratarErro e = 
      putStrLn $ "Erro ao salvar log: " ++ show (e :: SomeException)

-- | Exibe o inventário atual formatado
listarInventario :: Inventario -> IO ()
listarInventario inv = do
  putStrLn "\n=== INVENTARIO ATUAL ==="
  if Map.null inv
    then putStrLn "Inventario vazio."
    else mapM_ exibirItem (Map.elems inv)
  putStrLn "========================\n"
  where
    exibirItem item = 
      let
        idStr = "ID: " ++ itemID item
        nomeStr = " | Nome: " ++ nome item
        qtdStr = " | Qtd: " ++ show (quantidade item)
        catStr = " | Cat: " ++ categoria item
      in
        putStrLn $ idStr ++ nomeStr ++ qtdStr ++ catStr

-- | Processa comando do usuário e atualiza o inventário
processarComando :: String -> Inventario -> IO Inventario
processarComando cmd inv = do
  time <- getCurrentTime
  let tokens = words cmd
  case tokens of
    -- Comando: add <id> <nome> <qtd> <categoria>
    ["add", iid, nom, qtdStr, cat] ->
      case reads qtdStr :: [(Int, String)] of
        [(qtd, "")] ->
          case addItem time iid nom qtd cat inv of
            Right (novoInv, logEntry) -> do
              salvarInventario novoInv
              adicionarLog logEntry
              putStrLn "✓ Item adicionado com sucesso!"
              return novoInv
            Left erro -> do
              adicionarLog (criarLogFalha time Add erro)
              putStrLn $ "✗ Erro: " ++ erro
              return inv
        _ -> do
          putStrLn "✗ Quantidade invalida"
          return inv
    
    -- Comando: remove <id> <qtd>
    ["remove", iid, qtdStr] ->
      case reads qtdStr :: [(Int, String)] of
        [(qtd, "")] ->
          case removeItem time iid qtd inv of
            Right (novoInv, logEntry) -> do
              salvarInventario novoInv
              adicionarLog logEntry
              putStrLn "✓ Item removido com sucesso!"
              return novoInv
            Left erro -> do
              adicionarLog (criarLogFalha time Remove erro)
              putStrLn $ "✗ Erro: " ++ erro
              return inv
        _ -> do
          putStrLn "✗ Quantidade invalida"
          return inv
    
    -- Comando: update <id> <nova_qtd>
    ["update", iid, qtdStr] ->
      case reads qtdStr :: [(Int, String)] of
        [(qtd, "")] ->
          case updateQty time iid qtd inv of
            Right (novoInv, logEntry) -> do
              salvarInventario novoInv
              adicionarLog logEntry
              putStrLn "✓ Quantidade atualizada com sucesso!"
              return novoInv
            Left erro -> do
              adicionarLog (criarLogFalha time Update erro)
              putStrLn $ "✗ Erro: " ++ erro
              return inv
        _ -> do
          putStrLn "✗ Quantidade invalida"
          return inv
    
    -- Comando: list
    ["list"] -> do
      listarInventario inv
      adicionarLog (LogEntry time ListItems "Listagem de inventário" Sucesso)
      return inv
    
    -- Comando: report
    ["report"] -> do
      logs <- carregarLogs
      gerarRelatorio logs
      adicionarLog (LogEntry time Report "Relatório gerado" Sucesso)
      return inv
      where
        gerarRelatorio logs = do
          putStrLn "\n=== RELATORIO DE ANALISE ==="
          putStrLn $ "Total de operacoes: " ++ show (length logs)
          
          let erros = logsDeErro logs
          putStrLn $ "Total de erros: " ++ show (length erros)
          
          putStrLn "\n--- Logs de Erro ---"
          if null erros
            then putStrLn "Nenhum erro registrado."
            else mapM_ (putStrLn . formatarLog) erros
          
          case itemMaisMovimentado logs of
            Nothing -> putStrLn "\nNenhum item movimentado."
            Just (item, count) -> 
              putStrLn $ "\nItem mais movimentado: " ++ item ++ 
                        " (" ++ show count ++ " operacoes)"
          
          putStrLn "============================\n"
        
        formatarLog (LogEntry t ac det st) =
          show t ++ " | " ++ show ac ++ " | " ++ det ++ " | " ++ show st
    
    -- Comando: help
    ["help"] -> do
      exibirAjuda
      return inv
    
    -- Comando: exit
    ["exit"] -> return inv
    
    -- Comando inválido
    _ -> do
      putStrLn "✗ Comando invalido. Digite 'help' para ver os comandos disponiveis."
      return inv

-- | Exibe menu de ajuda com todos os comandos disponíveis
exibirAjuda :: IO ()
exibirAjuda = do
  putStrLn "\n=== COMANDOS DISPONIVEIS ==="
  putStrLn "add <id> <nome> <quantidade> <categoria>  - Adiciona um item"
  putStrLn "remove <id> <quantidade>                   - Remove quantidade de um item"
  putStrLn "update <id> <nova_quantidade>              - Atualiza quantidade"
  putStrLn "list                                       - Lista todos os itens"
  putStrLn "report                                     - Gera relatorio de analise"
  putStrLn "help                                       - Exibe esta ajuda"
  putStrLn "exit                                       - Sai do programa"
  putStrLn "============================\n"

-- | Loop principal de interação com o usuário
-- Lê comandos e atualiza o inventário recursivamente
loop :: Inventario -> IO ()
loop inv = do
  putStr "inventario> "
  cmd <- getLine
  case cmd of
    "exit" -> putStrLn "Encerrando sistema..."
    _ -> do
      novoInv <- processarComando cmd inv
      loop novoInv

-- ============================================================================
-- PONTO DE ENTRADA PRINCIPAL
-- ============================================================================

-- | Função principal que inicializa o sistema
-- Carrega estado anterior e inicia loop de interação
main :: IO ()
main = do
  -- Configura buffering para melhor interação no terminal
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  
  -- Exibe cabeçalho
  putStrLn "==================================="
  putStrLn "  SISTEMA DE INVENTARIO - HASKELL"
  putStrLn "==================================="
  
  -- Carrega estado anterior (usa catch internamente)
  putStrLn "Carregando dados..."
  inv <- carregarInventario
  
  -- Se inventário estiver vazio, cria itens iniciais
  invFinal <- if Map.null inv
              then criarInventarioInicial
              else return inv
  
  putStrLn $ "Inventario carregado: " ++ show (Map.size invFinal) ++ " itens"
  
  -- Instrução inicial
  putStrLn "Digite 'help' para ver os comandos disponiveis.\n"
  
  -- Inicia loop principal
  loop invFinal