---
title: "Implementación del patrón de diseño Decorator"
slug: "implementacion-del-patron-de-diseno-decorator"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

Ventajas de usar Decorator:
- puede agregar nuevas funcionalidades en tiempo de ejecución en diferentes configuraciones
- buena alternativa para la herencia
- el cliente puede elegir la configuración que quiere usar

## Simulando cafetería
El decorador es uno de los patrones de diseño estructural. Se utiliza para agregar, eliminar o cambiar el comportamiento del objeto. Este documento le enseñará cómo usar Decorator DP correctamente.

Déjame explicarte la idea de esto con un ejemplo simple. Imagina que ahora estás en Starbobs, la famosa compañía de café. Puede hacer un pedido de cualquier café que desee: ¡con crema y azúcar, con crema y topping y muchas más combinaciones! Pero, la base de todas las bebidas es el café - una bebida oscura y amarga que puedes modificar. Escribamos un programa simple que simule una máquina de café.

Primero, necesitamos crear una clase abstracta que describa nuestra bebida base:

    public abstract class AbstractCoffee
    {
        protected AbstractCoffee k = null;
 
        public AbstractCoffee(AbstractCoffee k)
        {
            this.k = k;
        }
 
        public abstract string ShowCoffee();
    }

Ahora, vamos a crear algunos extras, como azúcar, leche y cobertura. Las clases creadas deben implementar `AbstractCoffee`; lo decorarán:

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
Ahora podemos crear nuestro café favorito:

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
Ejecutar el código producirá el siguiente resultado:
> Café con Topping de Azúcar con Leche

