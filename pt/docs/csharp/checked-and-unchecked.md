---
title: "Verificado e desmarcado"
slug: "verificado-e-desmarcado"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## Sintaxe
- check(a + b) // expressão verificada
- unchecked(a + b) // expressão não verificada
- verificado { c = a + b; c+= 5; } // bloco verificado
- desmarcado { c = a + b; c+= 5; } // bloco desmarcado

## Marcado e desmarcado
As instruções C# são executadas no contexto marcado ou não verificado. Em um contexto verificado, estouro aritmético gera uma exceção. Em um contexto não verificado, o estouro aritmético é ignorado e o resultado é truncado.

    short m = 32767;   
    short n = 32767;
    int result1 =  checked((short)(m + n));   //will throw an OverflowException
    int result2 =  unchecked((short)(m + n)); // will return -2

Se nenhum deles for especificado, o contexto padrão dependerá de outros fatores, como opções do compilador.

## Marcado e desmarcado como escopo
As palavras-chave também podem criar escopos para (des)verificar várias operações.

    short m = 32767;
    short n = 32767;
    checked
    {
        int result1 = (short)(m + n); //will throw an OverflowException
    }
    unchecked
    {
        int result2 = (short)(m + n); // will return -2
    }

