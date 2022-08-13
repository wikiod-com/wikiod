---
title: "Código de Contratos y Afirmaciones"
slug: "codigo-de-contratos-y-afirmaciones"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Las afirmaciones para verificar la lógica siempre deben ser verdaderas
Las aserciones no se utilizan para realizar pruebas de parámetros de entrada, sino para verificar que el flujo del programa es correcto, es decir, que puede hacer ciertas suposiciones sobre su código en un momento determinado. En otras palabras: una prueba realizada con `Debug.Assert` debe *siempre* asumir que el valor probado es `verdadero`.

Debug.Assert solo se ejecuta en compilaciones DEBUG; se filtra de las compilaciones RELEASE. Debe considerarse una herramienta de depuración además de las pruebas unitarias y no como un reemplazo de los contratos de código o métodos de validación de entrada.

Por ejemplo, esta es una buena afirmación:

    var systemData = RetrieveSystemConfiguration();
    Debug.Assert(systemData != null);

Aquí, afirmar es una buena opción porque podemos suponer que RetrieveSystemConfiguration() devolverá un valor válido y nunca devolverá un valor nulo.

Aquí hay otro buen ejemplo:

    UserData user = RetrieveUserData();
    Debug.Assert(user != null);
    Debug.Assert(user.Age > 0);
    int year = DateTime.Today.Year - user.Age;

Primero, podemos suponer que RetrieveUserData() devolverá un valor válido. Luego, antes de usar la propiedad Edad, verificamos la suposición (que siempre debe ser cierta) de que la edad del usuario es estrictamente positiva.

Este es un mal ejemplo de afirmación:

    string input = Console.ReadLine();
    int age = Convert.ToInt32(input);
    Debug.Assert(age > 16);
    Console.WriteLine("Great, you are over 16");

Assert no es para la validación de entrada porque es incorrecto suponer que esta afirmación siempre será cierta. Debe usar métodos de validación de entrada para eso. En el caso anterior, también debe verificar que el valor de entrada sea un número en primer lugar.


