---
title: "Patrones de diseño estructural"
slug: "patrones-de-diseno-estructural"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Los patrones de diseño estructural son patrones que describen cómo se pueden combinar objetos y clases para formar una gran estructura y que facilitan el diseño al identificar una forma sencilla de realizar relaciones entre entidades. Hay siete patrones estructurales descritos. Son los siguientes: Adapter, Bridge, Composite, Decorator, Facade, Flyweight y Proxy

## Patrón de diseño del adaptador
> [**“Adaptador”**][1] como sugiere el nombre es el objeto que permite que dos
> las interfaces incompatibles se comunican entre sí.

> **Por ejemplo:** si compras un Iphone 8 (o cualquier otro producto de Apple) necesitas mucho
> adaptadores. Debido a que la interfaz predeterminada no es compatible con audio jac o
>USB. Con estos adaptadores puedes usar auriculares con cables o puedes usar un cable Ethernet normal. Entonces *"dos interfaces mutuamente incompatibles se comunican entre sí"*.

> **Entonces, en términos técnicos, esto significa:** Convertir la interfaz de un
> clase en otra interfaz que los clientes esperan. Adaptador dejado
> las clases trabajan juntas que de otro modo no podrían debido a la incompatibilidad
> interfaces. Las clases y objetos que participan en este patrón.
> son:

**El patrón adaptador sale de 4 elementos**
> 1. **ITarget:** Esta es la interfaz que utiliza el cliente para lograr la funcionalidad.
> 2. **Adaptado:** Esta es la funcionalidad que el cliente desea pero su interfaz no es compatible con el cliente.
> 3. **Cliente:** Esta es la clase que quiere lograr alguna funcionalidad usando el código del adaptado.
> 4. **Adaptador:** Esta es la clase que implementaría ITarget y llamaría al código Adaptee que el cliente quiere llamar.

**UML**

[![ingrese la descripción de la imagen aquí][2]][2]

**Primer ejemplo de código (Ejemplo teórico)**.

    public interface ITarget
    {
        void MethodA();
    }

    public class Adaptee
    {
        public void MethodB()
        {
            Console.WriteLine("MethodB() is called");
        }
    }

    public class Client
    {
        private ITarget target;

        public Client(ITarget target)
        {
            this.target = target;
        }

        public void MakeRequest()
        {
            target.MethodA();
        }
    }  

    public class Adapter : Adaptee, ITarget
    {
        public void MethodA()
        {
            MethodB();
        }
    }

**Segundo ejemplo de código (implementación en el mundo real)**

    /// <summary>
    ///  Interface: This is the interface which is used by the client to achieve functionality.
    /// </summary>
    public interface ITarget
    {
        List<string> GetEmployeeList();
    }

    /// <summary>
    /// Adaptee: This is the functionality which the client desires but its interface is not compatible with the client.
    /// </summary>
    public class CompanyEmplyees
    {
        public string[][] GetEmployees()
        {
            string[][] employees = new string[4][];

            employees[0] = new string[] { "100", "Deepak", "Team Leader" };
            employees[1] = new string[] { "101", "Rohit", "Developer" };
            employees[2] = new string[] { "102", "Gautam", "Developer" };
            employees[3] = new string[] { "103", "Dev", "Tester" };

            return employees;
        }
    }

    /// <summary>
    /// Client: This is the class which wants to achieve some functionality by using the adaptee’s code (list of employees).
    /// </summary>
    public class ThirdPartyBillingSystem
    {
        /* 
         * This class is from a thirt party and you do'n have any control over it. 
         * But it requires a Emplyee list to do its work
         */

        private ITarget employeeSource;

        public ThirdPartyBillingSystem(ITarget employeeSource)
        {
            this.employeeSource = employeeSource;
        }

        public void ShowEmployeeList()
        {
            // call the clietn list in the interface
            List<string> employee = employeeSource.GetEmployeeList();

            Console.WriteLine("######### Employee List ##########");
            foreach (var item in employee)
            {
                Console.Write(item);
            }

        }
    }

    /// <summary>
    /// Adapter: This is the class which would implement ITarget and would call the Adaptee code which the client wants to call.
    /// </summary>
    public class EmployeeAdapter : CompanyEmplyees, ITarget
    {
        public List<string> GetEmployeeList()
        {
            List<string> employeeList = new List<string>();
            string[][] employees = GetEmployees();
            foreach (string[] employee in employees)
            {
                employeeList.Add(employee[0]);
                employeeList.Add(",");
                employeeList.Add(employee[1]);
                employeeList.Add(",");
                employeeList.Add(employee[2]);
                employeeList.Add("\n");
            }

            return employeeList;
        }
    }

    /// 
    /// Demo
    /// 
    class Programs
    {
        static void Main(string[] args)
        {
            ITarget Itarget = new EmployeeAdapter();
            ThirdPartyBillingSystem client = new ThirdPartyBillingSystem(Itarget);
            client.ShowEmployeeList();
            Console.ReadKey();
        }
    }

**Cuándo usar**
- Permitir que un sistema use clases de otro sistema que es incompatible
con eso.
- Permitir la comunicación entre el sistema nuevo y el ya existente que son
independientes entre si
- Ado.Net SqlAdapter, OracleAdapter, MySqlAdapter son el mejor ejemplo de
Patrón adaptador.


[1]: https://en.wikipedia.org/wiki/Adapter_pattern
[2]: https://i.stack.imgur.com/oYMFy.gif

