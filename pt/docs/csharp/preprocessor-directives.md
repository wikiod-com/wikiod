---
title: "Diretivas do pré-processador"
slug: "diretivas-do-pre-processador"
draft: false
images: []
weight: 9933
type: docs
toc: true
---

## Sintaxe
- #define *[symbol]* // Define um símbolo do compilador.
- #undef *[symbol]* // Indefine um símbolo do compilador.
- #warning *[warning message]* // Gera um aviso do compilador. Útil com #if.
- #error *[mensagem de erro]* // Gera um erro do compilador. Útil com #if.
- #line *[line number] (file name)* // Substitui o número da linha do compilador (e opcionalmente o nome do arquivo de origem). Usado com [modelos de texto T4](https://msdn.microsoft.com/en-us/library/bb126445.aspx).
- #pragma warning [disable|restore] *[warning numbers]* // Desabilita/restaura os avisos do compilador.
- #pragma checksum "*[filename]*" "*[guid]*" "*[checksum]*" // Valida o conteúdo de um arquivo fonte.
- #region *[region name]* // Define uma região de código recolhível.
- #endregion // Finaliza um bloco de região de código.
- #if *[condition]* // Executa o código abaixo se a condição for verdadeira.
- #else // Usado após um #if.
- #elif *[condition]* // Usado após um #if.
- #endif // Finaliza um bloco condicional iniciado com #if.

As diretivas de pré-processador são normalmente usadas para tornar os programas de origem fáceis de alterar e compilar em diferentes ambientes de execução. As diretivas no arquivo de origem instruem o pré-processador a executar ações específicas. Por exemplo, o pré-processador pode substituir tokens no texto, inserir o conteúdo de outros arquivos no arquivo de origem ou suprimir a compilação de parte do arquivo removendo seções de texto. As linhas de pré-processador são reconhecidas e executadas antes da expansão da macro. Portanto, se uma macro se expandir para algo que se pareça com um comando de pré-processador, esse comando não será reconhecido pelo pré-processador.
 
As instruções do pré-processador usam o mesmo conjunto de caracteres que as instruções do arquivo de origem, com a exceção de que as sequências de escape não são suportadas. O conjunto de caracteres usado nas instruções do pré-processador é o mesmo que o conjunto de caracteres de execução. O pré-processador também reconhece valores de caracteres negativos.

## Expressões condicionais

Expressões condicionais (`#if`, `#elif`, etc) suportam um subconjunto limitado de operadores booleanos. Eles são:

- `==` e `!=`. Estes só podem ser usados ​​para testar se o símbolo é verdadeiro (definido) ou falso (não definido)
- `&&`, `||`, `!`
- `()`

Por exemplo:

    #if !DEBUG && (SOME_SYMBOL || SOME_OTHER_SYMBOL) && RELEASE == true
    Console.WriteLine("OK!");
    #endif

compilaria o código que imprime "OK!" para o console se `DEBUG` não for definido, `SOME_SYMBOL` ou `SOME_OTHER_SYMBOL` for definido, e `RELEASE` for definido.

Nota: Essas substituições são feitas _em tempo de compilação_ e, portanto, não estão disponíveis para inspeção em tempo de execução. Código eliminado pelo uso de `#if` não faz parte da saída do compilador.

Consulte também: [Diretivas de pré-processador C#](https://msdn.microsoft.com/en-us/library/ed8yd1ha.aspx) no MSDN.


## Expressões condicionais
Quando o seguinte for compilado, ele retornará um valor diferente dependendo de quais diretivas são definidas.

    // Compile with /d:A or /d:B to see the difference
    string SomeFunction() 
    {
    #if A
        return "A";
    #elif B
        return "B";
    #else
        return "C";
    #endif
    }

As expressões condicionais são normalmente usadas para registrar informações adicionais para compilações de depuração.

    void SomeFunc()
    {
        try
        {
            SomeRiskyMethod();
        }
        catch (ArgumentException ex)
        {
            #if DEBUG
            log.Error("SomeFunc", ex);
            #endif

            HandleException(ex);
        }
    }



## Outras instruções do compilador
# Linha

`#line` controla o número da linha e o nome do arquivo relatado pelo compilador ao emitir avisos e erros.

    void Test()
    {
        #line 42 "Answer"
        #line filename "SomeFile.cs"
        int life; // compiler warning CS0168 in "SomeFile.cs" at Line 42
        #line default
        // compiler warnings reset to default
    }

# Pragma Checksum

`#pragma checksum` permite a especificação de um checksum específico para um banco de dados de programa gerado (PDB) para depuração.

    #pragma checksum "MyCode.cs" "{00000000-0000-0000-0000-000000000000}" "{0123456789A}"

## Definindo e Indefinindo Símbolos
Um símbolo do compilador é uma palavra-chave definida em tempo de compilação que pode ser verificada para executar condicionalmente seções específicas de código.
 
Existem três maneiras de definir um símbolo do compilador. Eles podem ser definidos via código:
 
    #define MYSYMBOL
 
Eles podem ser definidos no Visual Studio, em Propriedades do projeto > Compilar > Símbolos de compilação condicional:
 
![Símbolos do compilador VS](http://i.imgur.com/PHG04dI.png)
 
*(Observe que `DEBUG` e `TRACE` têm suas próprias caixas de seleção e não precisam ser especificadas explicitamente.)*
 
Ou eles podem ser definidos em tempo de compilação usando a opção `/define:[name]` no compilador C#, `csc.exe`.

Você também pode indefinir símbolos usando a diretiva `#undefine`.
 
O exemplo mais comum disso é o símbolo `DEBUG`, que é definido pelo Visual Studio quando um aplicativo é compilado no modo Debug (versus modo Release).
 
    public void DoBusinessLogic()
    {
        try
        {
            AuthenticateUser();
            LoadAccount();
            ProcessAccount();
            FinalizeTransaction();
        }
        catch (Exception ex)
        {
    #if DEBUG
            System.Diagnostics.Trace.WriteLine("Unhandled exception!");
            System.Diagnostics.Trace.WriteLine(ex);
            throw;
    #else
            LoggingFramework.LogError(ex);
            DisplayFriendlyErrorMessage();
    #endif
        }
    }
 
No exemplo acima, quando ocorre um erro na lógica de negócios do aplicativo, se o aplicativo estiver compilado no modo Debug (e o símbolo `DEBUG` estiver definido), o erro será gravado no log de rastreamento e a exceção será ser relançado para depuração. No entanto, se o aplicativo for compilado no modo Release (e nenhum símbolo `DEBUG` for definido), uma estrutura de log será usada para registrar o erro silenciosamente e uma mensagem de erro amigável será exibida para o usuário final.

## Blocos de região
Use `#region` e `#endregion` para definir uma região de código recolhível.
 
    #region Event Handlers
 
    public void Button_Click(object s, EventArgs e)
    {
        // ...
    }
 
    public void DropDown_SelectedIndexChanged(object s, EventArgs e)
    {
        // ...
    }
 
    #endregion
 
Essas diretivas são benéficas apenas quando um IDE que oferece suporte a regiões recolhíveis (como [Visual Studio](https://www.visualstudio.com/en-us/visual-studio-homepage-vs.aspx)) é usado para editar o código.

## Desabilitando e restaurando avisos do compilador
Você pode desabilitar os avisos do compilador usando `#pragma warning disable` e restaurá-los usando `#pragma warning restore`:
 
    #pragma warning disable CS0168
 
    // Will not generate the "unused variable" compiler warning since it was disabled
    var x = 5;
 
    #pragma warning restore CS0168
 
    // Will generate a compiler warning since the warning was just restored
    var y = 8;
 
Números de aviso separados por vírgulas são permitidos:
 
    #pragma warning disable CS0168, CS0219
 
O prefixo `CS` é opcional e pode até ser misturado (embora esta não seja uma prática recomendada):
 
    #pragma warning disable 0168, 0219, CS0414

## Gerando avisos e erros do compilador
Os avisos do compilador podem ser gerados usando a diretiva `#warning`, e os erros também podem ser gerados usando a diretiva `#error`.

<!-- idioma: lang-none -->

    #if SOME_SYMBOL
    #error This is a compiler Error.
    #elif SOME_OTHER_SYMBOL
    #warning This is a compiler Warning.
    #endif

## Usando o atributo condicional
Adicionar um atributo `Conditional` do namespace `System.Diagnostics` a um método é uma maneira limpa de controlar quais métodos são chamados em suas compilações e quais não são.

    #define EXAMPLE_A

    using System.Diagnostics;
    class Program
    {
        static void Main()
        {
            ExampleA(); // This method will be called
            ExampleB(); // This method will not be called
        }

        [Conditional("EXAMPLE_A")]
        static void ExampleA() {...}

        [Conditional("EXAMPLE_B")]
        static void ExampleB() {...}
    }

## Pré-processadores personalizados no nível do projeto
É conveniente definir o pré-processamento condicional personalizado no nível do projeto quando algumas ações precisam ser ignoradas, digamos, para testes.

Vá para `Solution Explorer` -> Clique <kbd>Right Mouse</kbd> no projeto que você deseja definir a variável para -> `Properties` -> `Build` -> Em geral, encontre o campo `Símbolos de compilação condicional` e digite seu variável condicional aqui

[![digite a descrição da imagem aqui][1]][1]


Exemplo de código que irá pular algum código:

    public void Init()
    {
        #if !IGNOREREFRESHDB
        // will skip code here
         db.Initialize();
        #endif
    }

[1]: http://i.stack.imgur.com/B2pi1.png


