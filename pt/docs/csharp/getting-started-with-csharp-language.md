---
title: "Introdução à linguagem C#"
slug: "introducao-a-linguagem-c"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Criando um novo aplicativo de console (Visual Studio)
1. Abra o Visual Studio
2. Na barra de ferramentas, vá para **Arquivo** → **Novo projeto**
3. Selecione o tipo de projeto **Aplicativo de console**
4. Abra o arquivo `Program.cs` no Solution Explorer
5. Adicione o seguinte código a `Main()`:


    public class Program
    {
        public static void Main()
        {
            // Prints a message to the console.
            System.Console.WriteLine("Hello, World!");

            System.Console.ReadKey();
        }
    }

6. Na barra de ferramentas, clique em **Debug** -> **Start Debugging** ou pressione **F5** ou **ctrl + F5** (executando sem depurador) para executar o programa.


[Demonstração ao vivo no ideone][1]

-------------------------------------------------- ----------------------------------

# Explicação

- `class Program` é uma declaração de classe. A classe `Program` contém as definições de dados e métodos que seu programa usa. As classes geralmente contêm vários métodos. Os métodos definem o comportamento da classe. No entanto, a classe `Program` tem apenas um método: `Main`.

- `static void Main()` define o método `Main`, que é o ponto de entrada para todos os programas C#. O método `Main` indica o que a classe faz quando executada. Apenas um método `Main` é permitido por classe.

- O método `System.Console.WriteLine("Hello, world!");` imprime um dado dado (neste exemplo, `Hello, world!`) como uma saída na janela do console.

- `System.Console.ReadKey()`, garante que o programa não será fechado imediatamente após a exibição da mensagem. Ele faz isso esperando que o usuário pressione uma tecla no teclado. Qualquer tecla pressionada pelo usuário encerrará o programa. O programa termina quando termina a última linha de código no método `main()`.

-------------------------------------------------- ----------------------------------

# Usando a linha de comando

Para compilar via linha de comando, use `MSBuild` ou `csc.exe` _(o compilador C#)_, ambos parte do [Microsoft Build Tools](https://www.visualstudio.com/downloads/download-visual- studio-vs#d-build-tools).

Para compilar este exemplo, execute o seguinte comando no mesmo diretório em que `HelloWorld.cs` está localizado:

<!-- idioma: lang-none -->
    %WINDIR%\Microsoft.NET\Framework64\v4.0.30319\csc.exe HelloWorld.cs

Também pode ser possível que você tenha dois métodos principais dentro de um aplicativo. Neste caso, você deve informar ao compilador qual método principal executar digitando o seguinte comando no **console**. (suponha que a classe `ClassA` também tenha um método principal no mesmo arquivo `HelloWorld.cs` em HelloWorld namespace)

<!-- idioma: lang-none -->
    %WINDIR%\Microsoft.NET\Framework64\v4.0.30319\csc.exe HelloWorld.cs /main:HelloWorld.ClassA 

onde HelloWorld é namespace


***Observação**: Este é o caminho onde o **.NET framework v4.0** está localizado em geral. Altere o caminho de acordo com sua versão .NET. Além disso, o diretório pode ser **framework** em vez de **framework64** se você estiver usando o .NET Framework de 32 bits. No prompt de comando do Windows, você pode listar todos os caminhos da estrutura csc.exe executando os seguintes comandos (o primeiro para estruturas de 32 bits):*

    dir %WINDIR%\Microsoft.NET\Framework\csc.exe /s/b
    dir %WINDIR%\Microsoft.NET\Framework64\csc.exe /s/b

![Compilando o arquivo .cs][2]

Agora deve haver um arquivo executável chamado `HelloWorld.exe` no mesmo diretório. Para executar o programa a partir do prompt de comando, basta digitar o nome do executável e pressionar <kbd>Enter</kbd> da seguinte forma:

<!-- idioma: lang-none -->
    HelloWorld.exe

Isso produzirá:

>Olá, mundo!

![Executando o arquivo exe no console][3]

Você também pode clicar duas vezes no executável e abrir uma nova janela de console com a mensagem "**Hello, world!"

![Executando o executável e clicando duas vezes][4]

[1]: https://ideone.com/3OhmnG
[2]: http://i.stack.imgur.com/xT8kk.png
[3]: http://i.stack.imgur.com/x0Fek.png
[4]: http://i.stack.imgur.com/qstu1.png

## Criando um novo projeto no Visual Studio (aplicativo de console) e executando-o no modo de depuração
1. **Baixe e instale o [Visual Studio][1]**. O Visual Studio pode ser baixado em [VisualStudio.com][2]. A edição Community é sugerida, primeiro porque é gratuita, e segundo porque envolve todos os recursos gerais e pode ser estendida ainda mais.

2. **Abra o Visual Studio.**
3. **Bem-vindo.** Vá para **Arquivo → **Novo** → Projeto**.
    [![Microsoft Visual Studio - File Menu][3]][3]

4. Clique em **Modelos** → **Visual C#** → **Aplicativo de console**

    [![Microsoft Visual Studio - New Project window][4]][4]

5. **Depois de selecionar Console Application,** digite um nome para seu projeto e um local para salvar e pressione <kbd>OK</kbd>. Não se preocupe com o nome da solução.

6. **Projeto criado**. O projeto recém-criado será semelhante a:

    [![Microsoft Visual Studio - c# Default Project][5]][5]

    _(Always use descriptive names for projects so that they can easily be distinguished from other projects.  It is recommended not to use spaces in project or class name.)_

7. **Escreva o código.** Agora você pode atualizar seu `Program.cs` para apresentar "Hello world!" ao usuário.

        using System;
        
        namespace ConsoleApplication1
        {
            public class Program
            {
                public static void Main(string[] args)
                {
                }
            }
        }

    Add the following two lines to the `public static void Main(string[] args)` object in `Program.cs`: (make sure it's inside the braces)

        Console.WriteLine("Hello world!");
        Console.Read();

    **Why** `Console.Read()`__?__ The first line prints out the text "Hello world!" to the console, and the second line waits for a single character to be entered; in effect, this causes the program to pause execution so that you're able to see the output while debugging.  Without `Console.Read();`, when you start debugging the application it will just print "Hello world!" to the console and then immediately close.  Your code window should now look like the following:

        using System;
        
        namespace ConsoleApplication1
        {
            public class Program
            {
                public static void Main(string[] args)
                {
                    Console.WriteLine("Hello world!");
                    Console.Read();
                }
            }
        }

8. **Depure seu programa.** Pressione o botão Iniciar na barra de ferramentas próximo ao topo da janela [![Botão Iniciar Depuração][6]][6] ou pressione <kbd>F5</kbd> em seu teclado para executar seu aplicativo. Se o botão não estiver presente, você pode executar o programa a partir do menu superior: **Debug → Start Debugging**. O programa irá compilar e abrir uma janela de console. Deve ser semelhante à captura de tela a seguir:

[![Console executando o aplicativo Hello World][7]][7]

9. **Pare o programa.** Para fechar o programa, basta pressionar qualquer tecla do teclado. O `Console.Read()` que adicionamos foi para este mesmo propósito. Outra forma de fechar o programa é ir ao menu onde estava o botão <kbd>Iniciar</kbd>, e clicar no botão <kbd>Parar</kbd>.

     


[1]: https://www.visualstudio.com/products/vs-2015-product-editions
[2]: http://www.visualstudio.com
[3]: http://i.stack.imgur.com/fpvTX.png
[4]: http://i.stack.imgur.com/kKGls.png
[5]: http://i.stack.imgur.com/WVkeF.png
[6]: https://i.stack.imgur.com/odDu6.png
[7]: http://i.stack.imgur.com/ZD5MF.png

## Criando um novo programa usando Mono
Primeiro instale [Mono][1] seguindo as instruções de instalação para a plataforma de sua escolha, conforme descrito na [seção de instalação][2].

Mono está disponível para Mac OS X, Windows e Linux.

Após a instalação, crie um arquivo de texto, nomeie-o como `HelloWorld.cs` e copie o seguinte conteúdo para ele:

    public class Program
    {
        public static void Main()
        {
            System.Console.WriteLine("Hello, world!");
            System.Console.WriteLine("Press any key to exit..");
            System.Console.Read();
        }
    }


Se você estiver usando o Windows, execute o Prompt de Comando Mono que está incluído na instalação do Mono e garante que as variáveis ​​de ambiente necessárias sejam definidas. Se estiver no Mac ou Linux, abra um novo terminal.

Para compilar o arquivo recém-criado, execute o seguinte comando no diretório que contém `HelloWorld.cs`:

<!-- idioma: lang-none -->
    mcs -out:HelloWorld.exe HelloWorld.cs
 

O `HelloWorld.exe` resultante pode ser executado com:
 
<!-- idioma: lang-none -->
    mono HelloWorld.exe
 
que produzirá a saída:
 
 
<!-- idioma: lang-none -->
    Hello, world!   
    Press any key to exit..

 
[1]: http://www.mono-project.com/
[2]: http://www.mono-project.com/docs/getting-started/install/

## Criando um novo programa usando .NET Core
Primeiro instale o [**.NET Core SDK**][1] seguindo as instruções de instalação para a plataforma de sua escolha:

- [Janelas][2]
- [OSX][3]
- [Linux][4]
- [Docker][5]

Após a conclusão da instalação, abra um prompt de comando ou janela de terminal.

1. Crie um novo diretório com `mkdir hello_world` e mude para o diretório recém-criado com `cd hello_world`.

2. Crie um novo aplicativo de console com `dotnet new console`.
Isso produzirá dois arquivos:

    - **hello_world.csproj**

          <Project Sdk="Microsoft.NET.Sdk">

            <PropertyGroup>
              <OutputType>Exe</OutputType>
              <TargetFramework>netcoreapp1.1</TargetFramework>
            </PropertyGroup>

          </Project>
          
    - **Program.cs**

          using System;
        
          namespace hello_world
          {
              class Program
              {
                  static void Main(string[] args)
                  {
                      Console.WriteLine("Hello World!");
                  }
              }
          }

3. Restaure os pacotes necessários com `dotnet restore`.

4. *Opcional* Compile o aplicativo com `dotnet build` para Debug ou `dotnet build -c Release` para Release. `dotnet run` também executará o compilador e lançará erros de compilação, se algum for encontrado.

5. Execute o aplicativo com `dotnet run` para Debug ou `dotnet run .\bin\Release\netcoreapp1.1\hello_world.dll` para Release.

-------------------------------------------------- ----------------------------------

Saída do prompt de comando
----------
[![digite a descrição da imagem aqui][6]][6]


[1]: https://docs.microsoft.com/en-us/dotnet/articles/core/
[2]: https://www.microsoft.com/net/core#windows
[3]: https://www.microsoft.com/net/core#macos
[4]: https://www.microsoft.com/net/core#linuxubuntu
[5]: https://www.microsoft.com/net/core#dockercmd
[6]: https://i.stack.imgur.com/arqCl.png


## Criando uma nova consulta usando LinqPad
LinqPad é uma ótima ferramenta que permite aprender e testar recursos de linguagens .Net (C#, F# e VB.Net.)

1. Instale o [LinqPad][1]
2. Crie uma nova Consulta (<kbd>Ctrl</kbd> + <kbd>N</kbd>)
[![digite a descrição da imagem aqui][2]][2]
3. Em idioma, selecione "instruções C#"
[![digite a descrição da imagem aqui][3]][3]
4. Digite o código a seguir e clique em executar (<kbd>F5</kbd>)

        string hw = "Hello World";

        hw.Dump(); //or Console.WriteLine(hw);
[![digite a descrição da imagem aqui][4]][4]

5. Você deverá ver "Hello World" impresso na tela de resultados.
[![digite a descrição da imagem aqui][5]][5]
6. Agora que você criou seu primeiro programa .Net, vá e confira os exemplos incluídos no LinqPad através do navegador "Samples". Existem muitos exemplos excelentes que mostrarão muitos recursos diferentes das linguagens .Net.
[![digite a descrição da imagem aqui][6]][6]

**Notas:**
1. Se você clicar em "IL", poderá inspecionar o código IL gerado pelo seu código .net. Esta é uma ótima ferramenta de aprendizado.
[![digite a descrição da imagem aqui][7]][7]
2. Ao usar `LINQ to SQL` ou `Linq to Entities` você pode inspecionar o SQL que está sendo gerado, que é outra ótima maneira de aprender sobre LINQ.


[1]: http://www.linqpad.net/
[2]: http://i.stack.imgur.com/D0tSi.png
[3]: http://i.stack.imgur.com/kC5Ur.jpg
[4]: http://i.stack.imgur.com/LO4kD.jpg
[5]: http://i.stack.imgur.com/GzsrS.jpg
[6]: http://i.stack.imgur.com/yucuf.jpg
[7]: http://i.stack.imgur.com/XPumO.jpg

## Criando um novo projeto usando o Xamarin Studio
1. Baixe e instale [Xamarin Studio Community][1].
2. Abra o Xamarin Studio.
3. Clique em **Arquivo** → **Novo** → **Solução**.

[![Criando um novo projeto no Xamarin Studio][2]][2]

4. Clique em **.NET** → **Console Project** e escolha **C#**.
5. Clique em <kbd>Avançar</kbd> para continuar.

[![Escolhendo modelo para novo projeto][3]][3]
 
6. Digite o **Nome do projeto** e <kbd>Procurar...</kbd> para um **Local** para Salvar e clique em <kbd>Criar</kbd>.

[![Nome e localização do projeto][4]][4]

7. O projeto recém-criado será semelhante a:

[![digite a descrição da imagem aqui][5]][5]

8. Este é o código no Editor de Texto:


    using System;
    
    namespace FirstCsharp
    {
        public class MainClass
        {
            public static void Main(string[] args)
            {
                Console.WriteLine("Hello World!");
                Console.ReadLine();
            }
        }
    }

9. Para executar o código, pressione <kbd>F5</kbd> ou clique no **Botão Reproduzir** conforme mostrado abaixo:

[![Execute o código][6]][6]

10. Segue a saída:

[![saída][7]][7]


[1]: https://store.xamarin.com/
[2]: http://i.stack.imgur.com/hHjMM.png
[3]: http://i.stack.imgur.com/s58Ju.png
[4]: http://i.stack.imgur.com/lrK8L.png
[5]: http://i.stack.imgur.com/vva82.png
[6]: http://i.stack.imgur.com/6q4ZN.png
[7]: http://i.stack.imgur.com/cqBsK.png

