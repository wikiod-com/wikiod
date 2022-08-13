---
title: "Modèles de conception structurelle"
slug: "modeles-de-conception-structurelle"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Les modèles de conception structurelle sont des modèles qui décrivent comment les objets et les classes peuvent être combinés et former une grande structure et qui facilitent la conception en identifiant un moyen simple de réaliser des relations entre les entités. Sept modèles structurels sont décrits. Ils sont les suivants : Adapter, Bridge, Composite, Decorator, Facade, Flyweight et Proxy

## Modèle de conception d'adaptateur
> [**"Adaptateur"**][1] comme son nom l'indique est l'objet qui permet à deux
> les interfaces incompatibles communiquent entre elles.

> **Par exemple :** si vous achetez un Iphone 8 (ou tout autre produit Apple), vous avez besoin de beaucoup de
> adaptateurs. Parce que l'interface par défaut ne prend pas en charge audio jac ou
>USB. Avec ces adaptateurs, vous pouvez utiliser des écouteurs avec des fils ou vous pouvez utiliser un câble Ethernet normal. Donc *"deux interfaces mutuellement incompatibles communiquent entre elles"*.

> **Donc, en termes techniques, cela signifie :** Convertir l'interface d'un
> classe dans une autre interface attendue par les clients. Adaptateur laisser
> les classes travaillent ensemble ce qui ne pourrait pas autrement en raison d'incompatibilités
> interfaces. Les classes et les objets participant à ce modèle
> sont :

**Le modèle d'adaptateur sort sur 4 éléments**
> 1. **ITarget :** Il s'agit de l'interface utilisée par le client pour obtenir la fonctionnalité.
> 2. **Adapté :** C'est la fonctionnalité que souhaite le client mais son interface n'est pas compatible avec le client.
> 3. **Client :** Il s'agit de la classe qui souhaite obtenir certaines fonctionnalités en utilisant le code de l'adapté.
> 4. **Adapter :** C'est la classe qui implémenterait ITarget et appellerait le code Adaptee que le client veut appeler.

**UML**

[![entrez la description de l'image ici][2]][2]

**Premier exemple de code (exemple théorique)**.

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

**Deuxième exemple de code (implémentation dans le monde réel)**

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

**Quand utiliser**
- Autoriser un système à utiliser les classes d'un autre système qui est incompatible
avec ça.
- Permettre la communication entre le nouveau système et le système déjà existant qui sont
indépendants les uns des autres
- Ado.Net SqlAdapter, OracleAdapter, MySqlAdapter sont le meilleur exemple de
Modèle d'adaptateur.


[1] : https://en.wikipedia.org/wiki/Adapter_pattern
[2] : https://i.stack.imgur.com/oYMFy.gif

