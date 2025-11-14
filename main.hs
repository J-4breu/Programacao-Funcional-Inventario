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
