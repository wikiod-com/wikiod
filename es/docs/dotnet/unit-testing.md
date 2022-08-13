---
title: "Examen de la unidad"
slug: "examen-de-la-unidad"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Adición del proyecto de pruebas unitarias de MSTest a una solución existente
* Haga clic derecho en la solución, Agregar nuevo proyecto
* En la sección Prueba, seleccione un Proyecto de prueba de unidad
* Elija un nombre para el ensamblaje: si está probando el proyecto `Foo`, el nombre puede ser `Foo.Tests`
* Agregue una referencia al proyecto probado en las referencias del proyecto de prueba unitaria


## Crear un método de prueba de muestra
MSTest (el marco de prueba predeterminado) requiere que sus clases de prueba estén decoradas con un atributo `[TestClass]` y los métodos de prueba con un atributo `[TestMethod]`, y que sean públicos.

    [TestClass]
    public class FizzBuzzFixture
    {
        [TestMethod]
        public void Test1()
        {
            //arrange
            var solver = new FizzBuzzSolver();
            //act
            var result = solver.FizzBuzz(1);
            //assert
            Assert.AreEqual("1",result);
        }
    }

