---
title: "Marcado y sin marcar"
slug: "marcado-y-sin-marcar"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## Sintaxis
- verificado(a + b) // expresión verificada
- unchecked(a + b) // expresión sin marcar
- comprobado { c = a + b; c+= 5; } // bloque marcado
- sin marcar { c = a + b; c+= 5; } // bloque sin marcar

## Marcado y no marcado
Las declaraciones de C# se ejecutan en contexto marcado o no marcado. En un contexto comprobado, el desbordamiento aritmético genera una excepción. En un contexto no verificado, el desbordamiento aritmético se ignora y el resultado se trunca.

    short m = 32767;   
    short n = 32767;
    int result1 =  checked((short)(m + n));   //will throw an OverflowException
    int result2 =  unchecked((short)(m + n)); // will return -2

Si no se especifica ninguno de estos, el contexto predeterminado se basará en otros factores, como las opciones del compilador.

## Marcado y no marcado como ámbito
Las palabras clave también pueden crear ámbitos para (des) verificar múltiples operaciones.

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

