---
title: "Implémentation du modèle de conception de décorateur"
slug: "implementation-du-modele-de-conception-de-decorateur"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

Avantages d'utiliser Decorator :
- vous pouvez ajouter de nouvelles fonctionnalités lors de l'exécution dans différentes configurations
- bonne alternative pour l'héritage
- le client peut choisir la configuration qu'il veut utiliser

## Cafétéria simulée
Le décorateur est l'un des modèles de conception structurelle. Il est utilisé pour ajouter, supprimer ou modifier le comportement d'un objet. Ce document vous apprendra comment utiliser correctement Decorator DP.

Laissez-moi vous en expliquer l'idée sur un exemple simple. Imaginez que vous êtes maintenant chez Starbobs, célèbre compagnie de café. Vous pouvez passer une commande pour n'importe quel café que vous voulez - avec de la crème et du sucre, avec de la crème et de la garniture et bien d'autres combinaisons ! Mais la base de toutes les boissons est le café - une boisson sombre et amère que vous pouvez modifier. Écrivons un programme simple qui simule une machine à café.

Tout d'abord, nous devons créer une classe abstraite décrivant notre boisson de base :

    public abstract class AbstractCoffee
    {
        protected AbstractCoffee k = null;
 
        public AbstractCoffee(AbstractCoffee k)
        {
            this.k = k;
        }
 
        public abstract string ShowCoffee();
    }

Maintenant, créons quelques extras, comme le sucre, le lait et la garniture. Les classes créées doivent implémenter `AbstractCoffee` - elles le décoreront :

    public class Milk : AbstractCoffee
    {
        public Milk(AbstractCoffee c) : base(c) { }
        public override string ShowCoffee()
        {
            if (k != null)
                return k.ShowCoffee() + " with Milk";
            else return "Milk";
        }
    }
    public class Sugar : AbstractCoffee
    {
        public Sugar(AbstractCoffee c) : base(c) { }
 
        public override string ShowCoffee()
        {
            if (k != null) return k.ShowCoffee() + " with Sugar";
            else return "Sugar";
        }
    }
    public class Topping : AbstractCoffee
    {
        public Topping(AbstractCoffee c) : base(c) { }
 
        public override string ShowCoffee()
        {
            if (k != null) return k.ShowCoffee() + " with Topping";
            else return "Topping";
        }
    }
Nous pouvons maintenant créer notre café préféré :

    public class Program
    {
        public static void Main(string[] args)
        {
            AbstractCoffee coffee = null; //we cant create instance of abstract class
            coffee = new Topping(coffee); //passing null
            coffee = new Sugar(coffee); //passing topping instance
            coffee = new Milk(coffee);  //passing sugar
            Console.WriteLine("Coffee with " + coffee.ShowCoffee());
 
        }
    }
L'exécution du code produira la sortie suivante :
> Café avec Topping au Sucre au Lait

