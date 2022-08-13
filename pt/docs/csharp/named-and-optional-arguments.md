---
title: "Argumentos nomeados e opcionais"
slug: "argumentos-nomeados-e-opcionais"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

**Argumentos nomeados**

*Ref: MSDN* Os argumentos nomeados permitem que você especifique um argumento para um parâmetro específico associando o argumento ao nome do parâmetro em vez de à posição do parâmetro na lista de parâmetros.

Como dito pelo MSDN, Um argumento nomeado,

- Permite passar o argumento para a função associando o
nome do parâmetro.
- Não há necessidade de lembrar a posição dos parâmetros que não estamos
ciente de sempre.
- Não há necessidade de procurar a ordem dos parâmetros na lista de parâmetros de
chamada função.
- Podemos especificar o parâmetro para cada argumento pelo seu nome.

**Argumentos opcionais**

*Ref: MSDN* A definição de um método, construtor, indexador ou delegado pode especificar que seus parâmetros são obrigatórios ou opcionais. Qualquer chamada deve fornecer argumentos para todos os parâmetros obrigatórios, mas pode omitir argumentos para parâmetros opcionais.

Como dito pelo MSDN, um argumento opcional,

- Podemos omitir o argumento na chamada se esse argumento for um Optional
Argumento
- Cada argumento opcional tem seu próprio valor padrão
- Levará o valor padrão se não fornecermos o valor
- Um valor padrão de um argumento opcional deve ser um
- Expressão constante.
- Deve ser um tipo de valor, como enum ou struct.
- Deve ser uma expressão do formulário default(valueType)
- Deve ser definido no final da lista de parâmetros

## Argumentos Opcionais
Considere precedente é nossa definição de função com argumentos opcionais.

    private static double FindAreaWithOptional(int length, int width=56)
           {
               try
               {
                   return (length * width);
               }
               catch (Exception)
               {
                   throw new NotImplementedException();
               }
           }

Aqui definimos o valor para largura como opcional e demos o valor como 56. Se você notar, o próprio IntelliSense mostra o argumento opcional conforme mostrado na imagem abaixo.

[![digite a descrição da imagem aqui][1]][1]

    Console.WriteLine("Area with Optional Argument : ");
    area = FindAreaWithOptional(120);
    Console.WriteLine(area);
    Console.Read();

Observe que não recebemos nenhum erro durante a compilação e ele fornecerá uma saída da seguinte maneira.

[![digite a descrição da imagem aqui][2]][2]



**Usando o atributo opcional.**

Outra maneira de implementar o argumento opcional é usando a palavra-chave `[Optional]`. Se você não passar o valor para o argumento opcional, o valor padrão desse tipo de dados será atribuído a esse argumento. A palavra-chave `Optional` está presente no namespace “Runtime.InteropServices”.

    using System.Runtime.InteropServices;  
    private static double FindAreaWithOptional(int length, [Optional]int width)
       {
           try
           {
               return (length * width);
           }
           catch (Exception)
           {
               throw new NotImplementedException();
           }
       } 

    area = FindAreaWithOptional(120);  //area=0
E quando chamamos a função, obtemos 0 porque o segundo argumento não é passado e o valor padrão de int é 0 e, portanto, o produto é 0.
    


[1]: http://i.stack.imgur.com/Uaszw.png
[2]: http://i.stack.imgur.com/3BWQA.png

## Argumentos nomeados
Considere a seguir nossa chamada de função.

    FindArea(120, 56);
Neste nosso primeiro argumento é comprimento (ou seja, 120) e segundo argumento é largura (ou seja, 56). E estamos calculando a área por essa função. E a seguir está a definição da função.

    private static double FindArea(int length, int width)
           {
               try
               {
                   return (length* width);
               }
               catch (Exception)
               {
                   throw new NotImplementedException();
               }
           }

Então, na primeira chamada de função, acabamos de passar os argumentos por sua posição. Certo?

    double area;
    Console.WriteLine("Area with positioned argument is: ");
    area = FindArea(120, 56);
    Console.WriteLine(area);
    Console.Read();
Se você executar isso, obterá uma saída da seguinte maneira.

[![digite a descrição da imagem aqui][1]][1]

Agora aqui vem os recursos de argumentos nomeados. Consulte a chamada de função anterior.


    Console.WriteLine("Area with Named argument is: ");
    area = FindArea(length: 120, width: 56);
    Console.WriteLine(area);
    Console.Read();

Aqui estamos dando os argumentos nomeados na chamada do método.

    area = FindArea(length: 120, width: 56);
Agora, se você executar este programa, obterá o mesmo resultado. Podemos dar os nomes vice-versa na chamada do método se estivermos usando os argumentos nomeados.

    Console.WriteLine("Area with Named argument vice versa is: ");
    area = FindArea(width: 120, length: 56);
    Console.WriteLine(area);
    Console.Read();

Um dos usos importantes de um argumento nomeado é que, quando você o usa em seu programa, melhora a legibilidade do seu código. Ele simplesmente diz qual é o seu argumento, ou o que é?.

Você também pode fornecer os argumentos posicionais. Isso significa uma combinação de argumento posicional e argumento nomeado.

    Console.WriteLine("Area with Named argument Positional Argument : ");
                area = FindArea(120, width: 56);
                Console.WriteLine(area);
                Console.Read();

No exemplo acima, passamos 120 como comprimento e 56 como argumento nomeado para o parâmetro largura.

Existem algumas limitações também. Vamos discutir a limitação de argumentos nomeados agora.

**Limitação de usar um argumento nomeado**

A especificação do argumento nomeado deve aparecer após a especificação de todos os argumentos fixos.

Se você usar um argumento nomeado antes de um argumento fixo, receberá um erro de tempo de compilação da seguinte maneira.

[![digite a descrição da imagem aqui][2]][2]

A especificação do argumento nomeado deve aparecer após a especificação de todos os argumentos fixos


[1]: http://i.stack.imgur.com/aCYyR.png
[2]: http://i.stack.imgur.com/n8z4Y.png

