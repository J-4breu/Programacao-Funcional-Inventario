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
