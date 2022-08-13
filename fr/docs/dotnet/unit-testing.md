---
title: "Tests unitaires"
slug: "tests-unitaires"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Ajout du projet de test unitaire MSTest à une solution existante
* Faites un clic droit sur la solution, Ajouter un nouveau projet
* Dans la section Test, sélectionnez un projet de test unitaire
* Choisissez un nom pour l'assembly - si vous testez le projet `Foo`, le nom peut être `Foo.Tests`
* Ajouter une référence au projet testé dans les références du projet de test unitaire


## Création d'un exemple de méthode de test
MSTest (le framework de test par défaut) nécessite que vos classes de test soient décorées par un attribut `[TestClass]` et les méthodes de test avec un attribut `[TestMethod]`, et qu'elles soient publiques.

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

