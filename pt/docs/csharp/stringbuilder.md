---
title: "StringBuilder"
slug: "stringbuilder"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## O que é um StringBuilder e quando usar um
Um [`StringBuilder`][1] representa uma série de caracteres que, ao contrário de uma string normal, são mutáveis. Muitas vezes há a necessidade de modificar strings que já fizemos, mas o objeto string padrão não é mutável. Isso significa que cada vez que uma string é modificada, um novo objeto string precisa ser criado, copiado e reatribuído.

    string myString = "Apples";
    mystring += " are my favorite fruit";

No exemplo acima, `myString` inicialmente tem apenas o valor `"Maçãs"`. No entanto, quando concatenamos `" are my favorite fruit"', o que a classe string precisa fazer internamente envolve:

- Criando um novo array de caracteres igual ao comprimento de `myString` e a nova string que estamos anexando.
- Copiando todos os caracteres de `myString` no início do nosso novo array e copiando a nova string no final do array.
- Crie um novo objeto string na memória e reatribua-o a `myString`.

Para uma única concatenação, isso é relativamente trivial. No entanto, e se for necessário executar muitas operações de acréscimo, digamos, em um loop?

    String myString = "";
    for (int i = 0; i < 10000; i++)
        myString += " "; // puts 10,000 spaces into our string

Devido às repetidas cópias e criação de objetos, isso prejudicará significativamente o desempenho do nosso programa. Podemos evitar isso usando um `StringBuilder`.

    StringBuilder myStringBuilder = new StringBuilder();    
    for (int i = 0; i < 10000; i++)
        myStringBuilder.Append(' ');

Agora, quando o mesmo loop é executado, o desempenho e a velocidade do tempo de execução do programa serão significativamente mais rápidos do que usar uma string normal. Para tornar o `StringBuilder` uma string normal, podemos simplesmente chamar o método `ToString()` de `StringBuilder`.


----------
No entanto, esta não é a única otimização que o `StringBuilder` tem. Para otimizar ainda mais as funções, podemos aproveitar outras propriedades que ajudam a melhorar o desempenho.

    StringBuilder sb = new StringBuilder(10000); // initializes the capacity to 10000

Se soubermos com antecedência quanto tempo nosso `StringBuilder` precisa ter, podemos especificar seu tamanho com antecedência, o que evitará que ele precise redimensionar o array de caracteres que possui internamente.

    sb.Append('k', 2000);

Embora usar o `StringBuilder` para anexar seja muito mais rápido do que uma string, ele pode ser executado ainda mais rápido se você precisar adicionar um único caractere várias vezes.

Uma vez que você tenha completado a construção de sua string, você pode usar o método `ToString()` no `StringBuilder` para convertê-la em uma `string` básica. Isso geralmente é necessário porque a classe `StringBuilder` não herda de `string`.

Por exemplo, aqui está como você pode usar um `StringBuilder` para criar uma `string`:

    string RepeatCharacterTimes(char character, int times)
    {
        StringBuilder builder = new StringBuilder("");
        for (int counter = 0; counter < times; counter++)
        {
            //Append one instance of the character to the StringBuilder.
            builder.Append(character);
        }
        //Convert the result to string and return it.
        return builder.ToString();
    }

----------
Concluindo, o `StringBuilder` deve ser usado no lugar da string quando muitas modificações em uma string precisam ser feitas com o desempenho em mente.


[1]: https://msdn.microsoft.com/en-us/library/system.text.stringbuilder(v=vs.110).aspx

## Use StringBuilder para criar string de um grande número de registros
    public string GetCustomerNamesCsv()
    {
        List<CustomerData> customerDataRecords = GetCustomerData(); // Returns a large number of records, say, 10000+
    
        StringBuilder customerNamesCsv = new StringBuilder();
        foreach (CustomerData record in customerDataRecords)
        {
           customerNamesCsv
               .Append(record.LastName)
               .Append(',')
               .Append(record.FirstName)
               .Append(Environment.NewLine);
        }

        return customerNamesCsv.ToString();
    }


