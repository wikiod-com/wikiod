---
title: "Contrats de code"
slug: "contrats-de-code"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

Les contrats de code permettent la compilation ou l'analyse d'exécution des conditions pré/post des méthodes et des conditions invariantes des objets. Ces conditions peuvent être utilisées pour garantir que les appelants et la valeur de retour correspondent à des états valides pour le traitement de l'application. Les autres utilisations des contrats de code incluent la génération de documentation.

## Contrats pour les interfaces
En utilisant les contrats de code, il est possible d'appliquer un contrat à une interface. Cela se fait en déclarant une classe abstraite qui implémente les interfaces. L'interface doit être étiquetée avec `ContractClassAttribute` et la définition du contrat (la classe abstraite) doit être étiquetée avec `ContractClassForAttribute`

**Exemple C#...**

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

**Résultat de l'analyse statique...**

[![entrez la description de l'image ici][1]][1]


[1] : http://i.stack.imgur.com/eDxbs.png

## Conditions préalables
Les conditions préalables permettent aux méthodes de fournir des valeurs minimales requises pour les paramètres d'entrée

**Exemple...**

    void DoWork(string input)
    {
        Contract.Requires(!string.IsNullOrEmpty(input));

        //do work
    }

**Résultat de l'analyse statique...**

[![entrez la description de l'image ici][1]][1]


[1] : http://i.stack.imgur.com/ZFVU0.png

## Postconditions
Les postconditions garantissent que les résultats renvoyés par une méthode correspondent à la définition fournie. Cela fournit à l'appelant une définition du résultat attendu. Les postconditions peuvent permettre des implémentations simplifiées car certains résultats possibles peuvent être fournis par l'analyseur statique.

**Exemple...**

    string GetValue()
    {
        Contract.Ensures(Contract.Result<string>() != null);

        return null;
    }

**Résultat de l'analyse statique...**

[![entrez la description de l'image ici][1]][1]


[1] : http://i.stack.imgur.com/gpCrS.png

## Installation et activation des contrats de code
Alors que `System.Diagnostics.Contracts` est inclus dans le .Net Framework. Pour utiliser les contrats de code, vous devez installer les extensions Visual Studio.

Sous `Extensions et mises à jour`, recherchez `Code Contracts` puis installez `Code Contracts Tools`

[![Installation des outils de contrat de code][1]][1]

Une fois les outils installés, vous devez activer `Code Contracts` dans votre solution Project. Au minimum, vous souhaiterez probablement activer la "Vérification statique" (vérifier après la construction). Si vous implémentez une bibliothèque qui sera utilisée par d'autres solutions, vous pouvez également envisager d'activer la "vérification de l'exécution".

[![Paramètres du projet][2]][2]


[1] : http://i.stack.imgur.com/hTYJ1.png
[2] : http://i.stack.imgur.com/f4f1Z.png

