---
title: "Manipulando FormatException ao converter string para outros tipos"
slug: "manipulando-formatexception-ao-converter-string-para-outros-tipos"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Convertendo string para inteiro
Existem vários métodos disponíveis para converter explicitamente uma `string` para um `integer`, como:
1. `Convert.ToInt16();`

2. `Convert.ToInt32();`

3. `Convert.ToInt64();`

4. `int.Parse();`

Mas todos esses métodos lançarão um `FormatException`, se a string de entrada contiver caracteres não numéricos. Para isso, precisamos escrever um tratamento de exceção adicional(`try..catch`) para tratá-los em tais casos.

<hr/>
 
**Explicação com exemplos:**

Então, seja nossa entrada:

    string inputString = "10.2";


**Exemplo 1:** `Convert.ToInt32()`

    int convertedInt = Convert.ToInt32(inputString); // Failed to Convert 
    // Throws an Exception "Input string was not in a correct format."

***Nota:** O mesmo vale para os outros métodos mencionados, a saber - `Convert.ToInt16();` e `Convert.ToInt64();`*
 

**Exemplo 2:** `int.Parse()`

    int convertedInt = int.Parse(inputString); // Same result "Input string was not in a correct format.

***Como podemos contornar isso?***

Como dito anteriormente, para lidar com as exceções, geralmente precisamos de um `try..catch` como mostrado abaixo:

    try
    {
        string inputString = "10.2";
        int convertedInt = int.Parse(inputString);
    }
    catch (Exception Ex)
    {
        //Display some message, that the conversion has failed.         
    }
Mas, usar o `try..catch` em todos os lugares não será uma boa prática, e pode haver alguns cenários em que queremos dar `0` se a entrada estiver errada, _(Se seguirmos o método acima, precisamos atribuir `0` para `convertedInt` do bloco catch)._
Para lidar com tais cenários podemos usar um método especial chamado `.TryParse()`.

O método `.TryParse()` possui um tratamento de exceção interno, que lhe dará a saída para o parâmetro `out`, e retorna um valor booleano indicando o status da conversão _(`true` se a conversão foi bem sucedida; `false` se falhou)._ Com base no valor de retorno, podemos determinar o status da conversão. Vamos ver um Exemplo:

**Uso 1:** armazene o valor de retorno em uma variável booleana

     int convertedInt; // Be the required integer
     bool isSuccessConversion = int.TryParse(inputString, out convertedInt);
Podemos verificar a variável `isSuccessConversion` após a execução para verificar o status da conversão. Se for false então o valor de `convertedInt` será `0`_(não é necessário verificar o valor de retorno se você quiser `0` para falha de conversão)._

**Uso 2:** verifique o valor de retorno com `if`

    if (int.TryParse(inputString, out convertedInt))
    {
        // convertedInt will have the converted value
        // Proceed with that
    }
    else 
    {
     // Display an error message
    }
**Uso 3:** sem verificar o valor de retorno
você pode usar o seguinte, se você não se importa com o valor de retorno _(convertido ou não, `0` será ok)_

    int.TryParse(inputString, out convertedInt);
    // use the value of convertedInt
    // But it will be 0 if not converted

