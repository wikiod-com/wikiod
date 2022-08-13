---
title: "Gerenciamento de memória"
slug: "gerenciamento-de-memoria"
draft: false
images: []
weight: 9925
type: docs
toc: true
---

Os aplicativos de desempenho crítico em aplicativos .NET gerenciados podem ser severamente afetados pelo GC. Quando o GC é executado, todos os outros encadeamentos são suspensos até que ele seja concluído. Por esse motivo, é recomendável avaliar cuidadosamente os processos do GC e determinar como minimizar quando ele é executado.

## Use SafeHandle ao agrupar recursos não gerenciados
Ao escrever wrappers para recursos não gerenciados, você deve subclassificar `SafeHandle` em vez de tentar implementar `IDisposable` e um finalizador você mesmo. Sua subclasse `SafeHandle` deve ser tão pequena e simples quanto possível para minimizar a chance de um vazamento de handle. Isso provavelmente significa que sua implementação do SafeHandle seria um detalhe de implementação interno de uma classe que o envolve para fornecer uma API utilizável. Essa classe garante que, mesmo que um programa vaze sua instância `SafeHandle`, seu identificador não gerenciado seja liberado.

    using System.Runtime.InteropServices;
    
    class MyHandle : SafeHandle
    {
        public override bool IsInvalid => handle == IntPtr.Zero;
        public MyHandle() : base(IntPtr.Zero, true)
        { }
    
        public MyHandle(int length) : this()
        {
            SetHandle(Marshal.AllocHGlobal(length));
        }

        protected override bool ReleaseHandle()
        {
            Marshal.FreeHGlobal(handle);
            return true;
        }
    }

Isenção de responsabilidade: Este exemplo é uma tentativa de mostrar como proteger um recurso gerenciado com `SafeHandle` que implementa `IDisposable` para você e configura os finalizadores adequadamente. É muito artificial e provavelmente inútil alocar um pedaço de memória dessa maneira.

## Recursos não gerenciados
Quando falamos sobre o GC e o "heap", estamos realmente falando sobre o que é chamado de *heap gerenciado*. Objetos no *heap gerenciado* podem acessar recursos que não estão no heap gerenciado, por exemplo, ao gravar ou ler um arquivo. Um comportamento inesperado pode ocorrer quando um arquivo é aberto para leitura e, em seguida, ocorre uma exceção, impedindo que o identificador de arquivo seja fechado como faria normalmente. Por esta razão, .NET requer que recursos não gerenciados implementem a interface `IDisposable`. Esta interface tem um único método chamado `Dispose` sem parâmetros:

    public interface IDisposable
    {
        Dispose();
    } 

Ao manipular recursos não gerenciados, você deve certificar-se de que eles sejam descartados corretamente. Você pode fazer isso chamando explicitamente `Dispose()` em um bloco `finally`, ou com uma instrução `using`.

    StreamReader sr; 
    string textFromFile;
    string filename = "SomeFile.txt";
    try 
    {
        sr = new StreamReader(filename);
        textFromFile = sr.ReadToEnd();
    }
    finally
    {
        if (sr != null) sr.Dispose();
    }

ou

    string textFromFile;
    string filename = "SomeFile.txt";
    
    using (StreamReader sr = new Streamreader(filename))
    {
        textFromFile = sr.ReadToEnd();
    }

O último é o método preferido e é automaticamente expandido para o primeiro durante a compilação.

