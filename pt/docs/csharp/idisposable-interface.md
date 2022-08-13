---
title: "Interface descartável"
slug: "interface-descartavel"
draft: false
images: []
weight: 9867
type: docs
toc: true
---

- Cabe aos clientes da classe que implementa `IDisposable` garantir que eles chamem o método `Dispose` quando terminarem de usar o objeto. Não há nada no CLR que procure objetos diretamente por um método `Dispose` para invocar.

- Não é necessário implementar um finalizador se seu objeto contiver apenas recursos gerenciados. Certifique-se de chamar `Dispose` em todos os objetos que sua classe usa quando você implementa seu próprio método `Dispose`.

- Recomenda-se tornar a classe segura contra várias chamadas para `Dispose`, embora deva ser chamada apenas uma vez. Isso pode ser feito adicionando uma variável `private bool` à sua classe e definindo o valor como `true` quando o método `Dispose` for executado.

## Em uma classe que contém apenas recursos gerenciados
Os recursos gerenciados são recursos que o coletor de lixo do tempo de execução está ciente e sob controle. Existem muitas classes disponíveis na BCL, por exemplo, como uma `SqlConnection` que é uma classe wrapper para um recurso não gerenciado. Essas classes já implementam a interface `IDisposable` -- cabe ao seu código limpá-las quando terminar.

Não é necessário implementar um finalizador se sua classe contiver apenas recursos gerenciados.

    public class ObjectWithManagedResourcesOnly : IDisposable
    {
        private SqlConnection sqlConnection = new SqlConnection();

        public void Dispose()
        {
            sqlConnection.Dispose();
        }
    }

## Em uma classe com recursos gerenciados e não gerenciados
É importante permitir que a finalização ignore os recursos gerenciados. O finalizador é executado em outro thread -- é possível que os objetos gerenciados não existam mais no momento em que o finalizador for executado. A implementação de um método `Dispose(bool)` protegido é uma prática comum para garantir que os recursos gerenciados não tenham seu método `Dispose` chamado de um finalizador.

    public class ManagedAndUnmanagedObject : IDisposable
    {
        private SqlConnection sqlConnection = new SqlConnection();
        private UnmanagedHandle unmanagedHandle = Win32.SomeUnmanagedResource();
        private bool disposed;

        public void Dispose()
        {
            Dispose(true); // client called dispose
            GC.SuppressFinalize(this); // tell the GC to not execute the Finalizer
        }

        protected virtual void Dispose(bool disposeManaged)
        {
            if (!disposed)
            {
                if (disposeManaged)
                {
                    if (sqlConnection != null)
                    {
                        sqlConnection.Dispose();
                    }
                }
    
                unmanagedHandle.Release();

                disposed = true;
            }
        }

        ~ManagedAndUnmanagedObject()
        {
            Dispose(false);
        }
    }

## IDescartável, Descarte


## usando palavra-chave
Quando um objeto implementa a interface `IDisposable`, ele pode ser criado dentro da sintaxe `using`:

    using (var foo = new Foo())
    {
        // do foo stuff
    } // when it reaches here foo.Dispose() will get called

    public class Foo : IDisposable
    {
        public void Dispose()
        {
            Console.WriteLine("dispose called");
        }
    }

[Ver demonstração][1]

`using` é [sintático sugar][2] para um bloco `try/finally`; o uso acima se traduziria aproximadamente em:

    {
        var foo = new Foo();
        try
        {
            // do foo stuff
        }
        finally
        {
            if (foo != null)
                ((IDisposable)foo).Dispose();
        }
    }

[1]: https://dotnetfiddle.net/StEPc2
[2]: https://en.wikipedia.org/wiki/Syntactic_sugar

## Em uma classe herdada com recursos gerenciados
É bastante comum que você possa criar uma classe que implemente `IDisposable` e, em seguida, derivar classes que também contêm recursos gerenciados. Recomenda-se marcar o método `Dispose` com a palavra-chave `virtual` para que os clientes tenham a capacidade de limpar quaisquer recursos que possuam.

    public class Parent : IDisposable
    {
        private ManagedResource parentManagedResource = new ManagedResource();

        public virtual void Dispose()
        {
            if (parentManagedResource != null)
            {
                parentManagedResource.Dispose();
            }
        }
    }

    public class Child : Parent
    {
        private ManagedResource childManagedResource = new ManagedResource();

        public override void Dispose()
        {
            if (childManagedResource != null)
            {
                childManagedResource.Dispose();
            }
            //clean up the parent's resources
            base.Dispose();
        }
    }

