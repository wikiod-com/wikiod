---
title: "BigInteger"
slug: "biginteger"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Quando usar ##
Objetos `BigInteger` são, por sua própria natureza, muito pesados ​​em RAM. Consequentemente, eles só devem ser usados ​​quando absolutamente necessário, ou seja, para números em uma escala verdadeiramente astronômica.

Além disso, todas as operações aritméticas nesses objetos são uma ordem de magnitude mais lentas do que suas contrapartes primitivas, esse problema fica ainda mais complicado à medida que o número cresce, pois não são de tamanho fixo. Portanto, é possível que um 'BigInteger' desonesto cause uma falha consumindo toda a RAM disponível.

## Alternativas ##

Se a velocidade for um imperativo para sua solução, pode ser mais eficiente implementar essa funcionalidade você mesmo usando uma classe envolvendo um `Byte[]` e sobrecarregando os operadores necessários. No entanto, isso requer uma quantidade significativa de esforço extra.



## Calcular o primeiro número de Fibonacci de 1.000 dígitos
Inclua `using System.Numerics` e adicione uma referência a `System.Numerics` ao projeto.

    using System;
    using System.Numerics;
    
    namespace Euler_25
    {
        class Program
        {
            static void Main(string[] args)
            {
                BigInteger l1 = 1;
                BigInteger l2 = 1;
                BigInteger current = l1 + l2;
                while (current.ToString().Length < 1000)
                {
                    l2 = l1;
                    l1 = current;
                    current = l1 + l2;
                }
                Console.WriteLine(current);
            }
        }
    }

Esse algoritmo simples itera pelos números de Fibonacci até atingir um de pelo menos 1.000 dígitos decimais de comprimento e, em seguida, imprime. Este valor é significativamente maior do que um `ulong` poderia conter.

Teoricamente, o único limite na classe `BigInteger` é a quantidade de RAM que seu aplicativo pode consumir.

Nota: `BigInteger` só está disponível em .NET 4.0 e superior.

