---
title: "Código de Contratos"
slug: "codigo-de-contratos"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

Los contratos de código permiten compilar o analizar en tiempo de ejecución las condiciones previas y posteriores de los métodos y las condiciones invariantes de los objetos. Estas condiciones se pueden usar para garantizar que las personas que llaman y el valor devuelto coincidan con los estados válidos para el procesamiento de la aplicación. Otros usos de los Contratos de código incluyen la generación de documentación.

## Contratos para Interfaces
Usando Code Contracts es posible aplicar un contrato a una interfaz. Esto se hace declarando una clase abstracta que implementa las interfaces. La interfaz debe etiquetarse con `ContractClassAttribute` y la definición del contrato (la clase abstracta) debe etiquetarse con `ContractClassForAttribute`

**C# Ejemplo...**

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

**Resultado del análisis estático...**

[![ingrese la descripción de la imagen aquí][1]][1]


[1]: http://i.stack.imgur.com/eDxbs.png

## Condiciones previas
Las condiciones previas permiten que los métodos proporcionen los valores mínimos requeridos para los parámetros de entrada

**Ejemplo...**

    void DoWork(string input)
    {
        Contract.Requires(!string.IsNullOrEmpty(input));

        //do work
    }

**Resultado del análisis estático...**

[![ingrese la descripción de la imagen aquí][1]][1]


[1]: http://i.stack.imgur.com/ZFVU0.png

## Postcondiciones
Las condiciones posteriores aseguran que los resultados devueltos por un método coincidirán con la definición proporcionada. Esto proporciona a la persona que llama una definición del resultado esperado. Las condiciones posteriores pueden permitir implementaciones simplificadas, ya que el analizador estático puede proporcionar algunos resultados posibles.

**Ejemplo...**

    string GetValue()
    {
        Contract.Ensures(Contract.Result<string>() != null);

        return null;
    }

**Resultado del análisis estático...**

[![ingrese la descripción de la imagen aquí][1]][1]


[1]: http://i.stack.imgur.com/gpCrS.png

## Instalación y habilitación de contratos de código
Mientras que `System.Diagnostics.Contracts` está incluido dentro de .Net Framework. Para usar Code Contracts, debe instalar las extensiones de Visual Studio.

En "Extensiones y actualizaciones", busque "Contratos de código" y luego instale las "Herramientas de contratos de código".

[![Instalación de herramientas de contrato de código][1]][1]

Una vez instaladas las herramientas, debe habilitar los "Contratos de código" dentro de su solución de Proyecto. Como mínimo, probablemente desee habilitar la `Comprobación estática` (comprobación después de la compilación). Si está implementando una biblioteca que será utilizada por otras soluciones, es posible que desee considerar habilitar también la "Comprobación en tiempo de ejecución".

[![Configuración del proyecto][2]][2]


[1]: http://i.stack.imgur.com/hTYJ1.png
[2]: http://i.stack.imgur.com/f4f1Z.png

