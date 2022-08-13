---
title: "Criando um aplicativo de console usando um editor de texto simples e o compilador C# (csc.exe)"
slug: "criando-um-aplicativo-de-console-usando-um-editor-de-texto-simples-e-o-compilador-c-cscexe"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Criando um aplicativo de console usando um editor de texto simples e o compilador C#
Para usar um editor de texto simples para criar um aplicativo de console escrito em C#, você precisará do compilador C#. O compilador C# (csc.exe), pode ser encontrado no seguinte local:
`%WINDIR%\Microsoft.NET\Framework64\v4.0.30319\csc.exe`

**N.B.** Dependendo da versão do .NET Framework instalada em seu sistema, pode ser necessário alterar o caminho acima de acordo.


----------

<h1>Salvando o código</h1>
O objetivo deste tópico não é ensiná-lo <i>como</i> escrever um aplicativo de console, mas ensiná-lo a <i>compilar</i> um [para produzir um único arquivo executável], sem nada diferente do compilador C# e qualquer editor de texto simples (como o bloco de notas).
<br/><br/>

1. Abra a caixa de diálogo Executar, usando o atalho de teclado <kbd>Tecla Windows</kbd> + <kbd>R</kbd>
2. Digite `notepad` e pressione <kbd>Enter</kbd>
3. Cole o código de exemplo abaixo no Bloco de Notas
4. Salve o arquivo como `ConsoleApp.cs`, indo para **Arquivo** → **Salvar como...**, digitando `ConsoleApp.cs` no campo de texto 'Nome do arquivo' e selecionando ` All Files` como o tipo de arquivo.
5. Clique em "Salvar"

<h1>Compilando o código-fonte</h1>
1. Abra a caixa de diálogo Executar, usando <kbd>Tecla Windows</kbd> + <kbd>R</kbd><br/>
2. Digite:

    %WINDIR%\Microsoft.NET\Framework64\v4.0.30319\csc.exe /t:exe /out:"C:\Users\yourUserName\Documents\ConsoleApp.exe" "C:\Users\yourUserName\Documents\ConsoleApp.cs"

Agora, volte para onde você salvou originalmente seu arquivo `ConsoleApp.cs`. Agora você deve ver um arquivo executável (`ConsoleApp.exe`). Clique duas vezes em `ConsoleApp.exe` para abri-lo.

É isso! Seu aplicativo de console foi compilado. Um arquivo executável foi criado e agora você tem um aplicativo de console em funcionamento.


    using System;
    
    namespace ConsoleApp
    {
        class Program
        {
            private static string input = String.Empty;
    
            static void Main(string[] args)
            {
                goto DisplayGreeting;
    
                DisplayGreeting:
                {
                    Console.WriteLine("Hello! What is your name?");
    
                    input = Console.ReadLine();
    
                    if (input.Length >= 1)
                    {
                        Console.WriteLine(
                            "Hello, " + 
                            input + 
                            ", enter 'Exit' at any time to exit this app.");
    
                        goto AwaitFurtherInstruction;
                    }
                    else
                    {
                        goto DisplayGreeting;
                    }
                }
    
                AwaitFurtherInstruction:
                {
                    input = Console.ReadLine();
    
                    if(input.ToLower() == "exit")
                    {
                        input = String.Empty;
    
                        Environment.Exit(0);
                    }
                    else
                    {
                        goto AwaitFurtherInstruction;
                    }
                }
            }
        }
    }

