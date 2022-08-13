---
title: "Convenções de nomenclatura"
slug: "convencoes-de-nomenclatura"
draft: false
images: []
weight: 9868
type: docs
toc: true
---

Este tópico descreve algumas convenções básicas de nomenclatura usadas ao escrever na linguagem C#. Como todas as convenções, elas não são impostas pelo compilador, mas garantem a legibilidade entre os desenvolvedores.

Para obter diretrizes abrangentes de design da estrutura .NET, consulte [docs.microsoft.com/dotnet/standard/design-guidelines](https://docs.microsoft.com/dotnet/standard/design-guidelines/).

## Escolha nomes de identificadores facilmente legíveis
Por exemplo, uma propriedade chamada HorizontalAlignment é mais legível em inglês do que AlignmentHorizontal.

## Favorecer a legibilidade sobre a brevidade
O nome da propriedade `CanScrollHorizontally` é melhor que `ScrollableX` (uma referência obscura ao eixo X).

Evite usar sublinhados, hífens ou quaisquer outros caracteres não alfanuméricos.

## **Não** use a notação húngara
A notação húngara é a prática de incluir um prefixo em identificadores para codificar alguns metadados sobre o parâmetro, como o tipo de dados do identificador, por exemplo, `string strName`.

Além disso, evite usar identificadores que entrem em conflito com palavras-chave já usadas em C#.

## Abreviações e Acrônimos
Em geral, você não deve usar abreviações ou siglas; estes tornam seus nomes menos legíveis. Da mesma forma, é difícil saber quando é seguro assumir que um acrônimo é amplamente reconhecido.

## Convenções de maiúsculas
Os termos a seguir descrevem diferentes maneiras de identificar identificadores de caso.
## Embalagem Pascal
A primeira letra no identificador e a primeira letra de cada palavra concatenada subsequente são maiúsculas. Você pode usar maiúsculas e minúsculas em Pascal para identificadores de três ou mais caracteres. Por exemplo: `BackColor`

## Carcaça de camelo
A primeira letra de um identificador é minúscula e a primeira letra de cada palavra concatenada subsequente é maiúscula. Por exemplo: `backColor`

## Maiúsculas
Todas as letras no identificador são maiúsculas. Por exemplo: `IO`

---

## Regras
Quando um identificador consiste em várias palavras, não use separadores, como sublinhados ("_") ou hifens ("-"), entre as palavras. Em vez disso, use maiúsculas para indicar o início de cada palavra.

A tabela a seguir resume as regras de capitalização para identificadores e fornece exemplos para os diferentes tipos de identificadores:

Identificador | Caso | Exemplo
---------------------- | ------ | -------
Variável local | Camelo | carName
Classe | Pascal | AppDomain
Tipo de enumeração | Pascal | Nível de erro
Valores de enumeração | Pascal | Erro fatal
Evento | Pascal | Valor alterado
Classe de exceção | Pascal | WebException
Campo estático somente leitura | Pascal | Valor Vermelho
Interface | Pascal | I Descartável
Método | Pascal | Para sequenciar
Espaço de nomes | Pascal | Sistema.Desenho
Parâmetro | Camelo | Digite o nome
Propriedade | Pascal | Cor de fundo

Mais informações podem ser encontradas em [MSDN][1].


[1]: https://msdn.microsoft.com/library/ms229043(v=vs.110).aspx

## Enumerações
## Use um nome singular para a maioria dos Enums

    public enum Volume
    {
       Low,
       Medium,
       High
    }

## Use um nome plural para tipos de Enum que são campos de bits

    [Flags]
    public enum MyColors
    {
        Yellow = 1,
        Green = 2,
        Red = 4,
        Blue = 8
    }
*Observação: sempre adicione o [`FlagsAttribute`][1] a um tipo de Enum de campo de bit.*

## **Não** adicione 'enum' como sufixo

    public enum VolumeEnum // Incorrect

## **Não** use o nome enum em cada entrada

    public enum Color
    {
        ColorBlue, // Remove Color, unnecessary
        ColorGreen,
    }


[1]: https://msdn.microsoft.com/en-us/library/system.flagsattribute(v=vs.110).aspx

## Interfaces
As interfaces devem ser nomeadas com substantivos ou frases nominais, ou adjetivos que descrevam o comportamento. Por exemplo, `IComponent` usa um substantivo descritivo, `ICustomAttributeProvider` usa uma frase nominal e `IPersistable` usa um adjetivo.

Os nomes das interfaces devem ser prefixados com a letra `I`, para indicar que o tipo é uma interface, e deve-se usar maiúsculas e minúsculas em Pascal.

Abaixo estão as interfaces nomeadas corretamente:

    public interface IServiceProvider
    public interface IFormatable

## Exceções
## Adicione 'exceção' como sufixo
Os nomes de exceção personalizados devem ter o sufixo "-Exception".

Abaixo estão as exceções nomeadas corretamente:

    public class MyCustomException : Exception
    public class FooException : Exception

## Campos privados
Existem duas convenções comuns para campos privados: `camelCase` e ​​`_camelCaseWithLeadingUnderscore`.

## Estojo de camelo

    public class Rational
    {
        private readonly int numerator;
        private readonly int denominator;

        public Rational(int numerator, int denominator)
        {
            // "this" keyword is required to refer to the class-scope field
            this.numerator = numerator;
            this.denominator = denominator;
        }
    }

## Caixa de camelo com sublinhado

    public class Rational
    {
        private readonly int _numerator;
        private readonly int _denominator;

        public Rational(int numerator, int denominator)
        {
            // Names are unique, so "this" keyword is not required
            _numerator = numerator;
            _denominator = denominator;
        }
    }

## Espaços de nomes
O formato geral para namespaces é:

    <Company>.(<Product>|<Technology>)[.<Feature>][.<Subnamespace>].

Exemplos incluem:

    Fabrikam.Math
    Litware.Security

Prefixar nomes de namespace com um nome de empresa impede que namespaces de empresas diferentes tenham o mesmo nome.



