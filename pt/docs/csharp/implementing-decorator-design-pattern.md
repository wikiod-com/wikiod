---
title: "Implementando o padrão de design do decorador"
slug: "implementando-o-padrao-de-design-do-decorador"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

Prós de usar o Decorator:
- você pode adicionar novas funcionalidades em tempo de execução em diferentes configurações
- boa alternativa para herança
- o cliente pode escolher a configuração que deseja usar

## Simulando cafeteria
Decorator é um dos padrões de projeto estrutural. É usado para adicionar, remover ou alterar o comportamento do objeto. Este documento irá ensiná-lo a usar o Decorator DP corretamente.

Deixe-me explicar a idéia disso para você em um exemplo simples. Imagine que você está agora na Starbobs, famosa empresa de café. Você pode encomendar o café que quiser - com creme e açúcar, com creme e cobertura e muito mais combinações! Mas, a base de todas as bebidas é o café - bebida escura e amarga, você pode modificar. Vamos escrever um programa simples que simule uma máquina de café.

Primeiro, precisamos criar uma classe abstrata que descreva nossa bebida base:

    public abstract class AbstractCoffee
    {
        protected AbstractCoffee k = null;
 
        public AbstractCoffee(AbstractCoffee k)
        {
            this.k = k;
        }
 
        public abstract string ShowCoffee();
    }

Agora, vamos criar alguns extras, como açúcar, leite e cobertura. As classes criadas devem implementar `AbstractCoffee` - elas irão decorá-lo:

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
Agora podemos criar nosso café favorito:

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
A execução do código produzirá a seguinte saída:
> Café com Cobertura com Açúcar com Leite

