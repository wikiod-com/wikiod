---
title: "Contratos de código"
slug: "contratos-de-codigo"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

Os contratos de código permitem a análise de compilação ou tempo de execução de condições pré/pós de métodos e condições invariáveis ​​para objetos. Essas condições podem ser usadas para garantir que os chamadores e o valor de retorno correspondam aos estados válidos para o processamento do aplicativo. Outros usos para contratos de código incluem geração de documentação.

## Contratos para Interfaces
Usando Contratos de Código é possível aplicar um contrato a uma interface. Isso é feito declarando uma classe abstrata que implementa as interfaces. A interface deve ser marcada com o `ContractClassAttribute` e a definição do contrato (a classe abstrata) deve ser marcada com o `ContractClassForAttribute`

**Exemplo de C#...**

    [ContractClass(typeof(MyInterfaceContract))]
    public interface IMyInterface
    {
        string DoWork(string input);
    }
    //Never inherit from this contract defintion class
    [ContractClassFor(typeof(IMyInterface))]
    internal abstract class MyInterfaceContract : IMyInterface
    {
        private MyInterfaceContract() { }

        public string DoWork(string input)
        {
            Contract.Requires(!string.IsNullOrEmpty(input));
            Contract.Ensures(!string.IsNullOrEmpty(Contract.Result<string>()));
            throw new NotSupportedException();
        }
    }
    public class MyInterfaceImplmentation : IMyInterface
    {
        public string DoWork(string input)
        {
            return input;
        }
    }

**Resultado da Análise Estática...**

[![digite a descrição da imagem aqui][1]][1]


[1]: http://i.stack.imgur.com/eDxbs.png

## Pré-condições
As pré-condições permitem que os métodos forneçam valores mínimos necessários para os parâmetros de entrada

**Exemplo...**

    void DoWork(string input)
    {
        Contract.Requires(!string.IsNullOrEmpty(input));

        //do work
    }

**Resultado da Análise Estática...**

[![digite a descrição da imagem aqui][1]][1]


[1]: http://i.stack.imgur.com/ZFVU0.png

## Pós-condições
As pós-condições garantem que os resultados retornados de um método correspondam à definição fornecida. Isso fornece ao chamador uma definição do resultado esperado. As pós-condições podem permitir implementações simplificadas, pois alguns resultados possíveis podem ser fornecidos pelo analisador estático.

**Exemplo...**

    string GetValue()
    {
        Contract.Ensures(Contract.Result<string>() != null);

        return null;
    }

**Resultado da Análise Estática...**

[![digite a descrição da imagem aqui][1]][1]


[1]: http://i.stack.imgur.com/gpCrS.png

## Instalando e habilitando contratos de código
Enquanto o `System.Diagnostics.Contracts` está incluído no .Net Framework. Para usar contratos de código, você deve instalar as extensões do Visual Studio.

Em `Extensões e Atualizações` procure por `Contratos de Código` e instale as `Ferramentas de Contratos de Código`

[![Instalar ferramentas de contrato de código][1]][1]

Após a instalação das ferramentas, você deve habilitar os `Code Contracts` em sua solução do Project. No mínimo, você provavelmente deseja habilitar a `Verificação estática` (verificar após a compilação). Se você estiver implementando uma biblioteca que será usada por outras soluções, você pode querer considerar também habilitar o `Runtime Checking`.

[![Configurações do projeto][2]][2]


[1]: http://i.stack.imgur.com/hTYJ1.png
[2]: http://i.stack.imgur.com/f4f1Z.png

