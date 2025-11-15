# Sistema de Gerenciamento de Inventário em Haskell

Este repositório contém o código-fonte da Atividade Avaliativa RA2, que implementa um sistema de gerenciamento de inventário interativo via terminal.

Desenvolvido em **Haskell**, o sistema demonstra conceitos de programação funcional, como a separação rigorosa entre lógica de negócio pura e operações de I/O, manipulação de estado e persistência de dados em disco.

## ℹ️ Informações do Projeto

* **Instituição:** `Pontífica Universidade Católica do Paraná`
* **Disciplina:** `Programação Lógica e Funcional`
* **Professor:** Frank Coelho de Alcantara

### 👥 Membros do Grupo (Ordem Alfabética)

* `Bruno Danguy Bortolini` (GitHub: `snowpuf`)
* `João Gabriel de Paula Leite Abreu` (GitHub: ` J-4breu`)
* `Kevyn Gabriel Gonçalves de Moraes` (GitHub: `kevyn-gabriel19`)
* `Loreno Nakayama Machado` (GitHub: `LorenzoNMachado`)

---

## 🚀 Como Executar (Ambiente Virtual)

Este projeto está configurado para ser executado no ambiente de desenvolvimento online Online GDB, conforme solicitado.

### 🔗 Link para o Ambiente de Execução

> **https://onlinegdb.com/lbRwjSJPf**

### 📋 Instruções

1.  Acesse o link acima.
2.  Clique no botão "Run" (ou "Start") para compilar e executar o projeto.
3.  O programa carregará os arquivos `Inventario.dat` e `Auditoria.log`.
    * **Primeira Execução:** Se os arquivos não existirem, o programa irá inicializar o sistema com **10 itens de exemplo** (via função `criarInventarioInicial`) e criará os arquivos de dados.
4.  Interaja com o sistema diretamente no console que aparecerá. Digite `help` para ver a lista de comandos.

---

## ⌨️ Comandos Disponíveis

O sistema é controlado por comandos simples no terminal:

| Comando | Descrição |
| :--- | :--- |
| `add <id> <nome> <qtd> <cat>` | Adiciona um novo item ao inventário. |
| `remove <id> <qtd>` | Remove uma certa quantidade de um item. |
| `update <id> <nova_qtd>` | Atualiza a quantidade total de um item. |
| `list` | Lista todos os itens atuais no inventário. |
| `report` | Gera e exibe um relatório de análise dos logs. |
| `help` | Exibe esta mensagem de ajuda. |
| `exit` | Salva o estado e encerra o programa. |

---

## 🧪 Documentação dos Cenários de Teste Manuais


### Cenário 1: Persistência de Estado (Sucesso)

Este cenário testa a capacidade do sistema de salvar o estado e recarregá-lo em uma nova execução.

1.  **Iniciar (sem arquivos):** O programa é executado pela primeira vez. Os arquivos `Inventario.dat` e `Auditoria.log` não existem.
2.  **Inicialização:** O sistema detecta a ausência de dados e automaticamente executa a função `criarInventarioInicial`, populando o sistema com 10 itens.
3.  **Adicionar 3 itens:**
    ```
    inventario> add A01 ItemA 10 Cat1
    ✓ Item adicionado com sucesso!
    inventario> add A02 ItemB 20 Cat2
    ✓ Item adicionado com sucesso!
    inventario> add A03 ItemC 30 Cat3
    ✓ Item adicionado com sucesso!
    ```
4.  **Fechar o programa:**
    ```
    inventario> exit
    Encerrando sistema...
    ```
5.  **Verificar arquivos:** Os arquivos `Inventario.dat` e `Auditoria.log` são criados com sucesso no sistema de arquivos.
6.  **Reiniciar o programa:** O programa é executado novamente (clicando "Run").
7.  **Executar comando `list`:**
    ```
    inventario> list
    ```
8.  **Resultado:** A listagem exibe todos os **13 itens** (os 10 iniciais + A01, A02, A03), confirmando que o estado foi lido corretamente do `Inventario.dat`.

### Cenário 2: Erro de Lógica (Estoque Insuficiente)

Este cenário testa a validação de regras de negócio (função pura) e o registro de falhas.

1.  **Garantir Estado:** O sistema é iniciado. O item `T001` (Teclado_Mecanico) existe com **15 unidades** (conforme `Inventario.dat`). (Este item cumpre o requisito de "adicionar um item com 10 unidades").
2.  **Tentar remover 20 unidades:** (Requisito: Tentar remover 15 unidades, mas usaremos 20 para testar a falha com o estoque de 15).
    ```
    inventario> remove T001 20
    ```
3.  **Verificar Mensagem de Erro:** O programa exibe a falha de lógica:
    ```
    ✗ Erro: Estoque insuficiente. Disponivel: 15
    ```
4.  **Verificar Inventário:** O estado em memória (e o arquivo `Inventario.dat`) não deve ser alterado.
    ```
    inventario> list
    === INVENTARIO ATUAL ===
    ...
    ID: T001 | Nome: Teclado_Mecanico | Qtd: 15 | Cat: Perifericos
    ...
    ========================
    ```
    **Resultado:** O item `T001` permanece com **15 unidades**.
5.  **Verificar Log de Auditoria:** O arquivo `Auditoria.log` é verificado.
    **Resultado:** Uma nova linha foi adicionada ao log, registrando a falha:
    `LogEntry {..., acao = Remove, ..., status = Falha "Estoque insuficiente. Disponivel: 15"}`

### Cenário 3: Geração de Relatório de Erros

Este cenário testa a capacidade do módulo de análise de logs.

1.  **Executar Cenário 2:** O cenário anterior é executado, garantindo que exista pelo menos um log de erro.
2.  **Executar comando `report`:**
    ```
    inventario> report
    ```
3.  **Verificar Saída do Relatório:**
    ```
    === RELATORIO DE ANALISE ===
    Total de operacoes: 14
    Total de erros: 1

    --- Logs de Erro ---
    2025-11-14 21:14:40.123456 UTC | Remove | Estoque insuficiente. Disponivel: 15 | Falha "Estoque insuficiente. Disponivel: 15"
    ...
    ============================
    ```
4.  **Resultado:** O relatório gerado exibe corretamente a entrada de log referente à falha registrada no Cenário 2, confirmando que a função `logsDeErro` funcionou.
