---
title: "Padrões de Projeto Estrutural"
slug: "padroes-de-projeto-estrutural"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Padrões de projeto estrutural são padrões que descrevem como objetos e classes podem ser combinados e formar uma grande estrutura e que facilitam o projeto identificando uma maneira simples de realizar relacionamentos entre entidades. Há sete padrões estruturais descritos. São eles: Adapter, Bridge, Composite, Decorator, Facade, Flyweight e Proxy

## Padrão de design do adaptador
> [**“Adaptador”**][1] como o nome sugere é o objeto que permite que dois
> interfaces incompatíveis se comunicam entre si.

> **Por exemplo:** se você comprar um Iphone 8 (ou qualquer outro produto da Apple) precisará de muito
> adaptadores. Como a interface padrão não suporta áudio jac ou
> USB. Com esses adaptadores, você pode usar fones de ouvido com fios ou pode usar um cabo Ethernet normal. Então *"duas interfaces mutuamente incompatíveis se comunicam"*.

> **Então, em termos técnicos, isso significa:** Converter a interface de um
> class em outra interface que um cliente espera. Adaptador let
> classes trabalham juntas que não poderiam de outra forma por causa de incompatibilidade
> interfaces. As classes e objetos que participam deste padrão
> são:

**O padrão do adaptador sai de 4 elementos**
> 1. **ITarget:** Esta é a interface que é usada pelo cliente para obter funcionalidade.
> 2. **Adaptee:** Esta é a funcionalidade que o cliente deseja, mas sua interface não é compatível com o cliente.
> 3. **Cliente:** Esta é a classe que deseja obter alguma funcionalidade usando o código do adaptee.
> 4. **Adapter:** Esta é a classe que implementaria ITarget e chamaria o código Adaptee que o cliente deseja chamar.

**UML**

[![digite a descrição da imagem aqui][2]][2]

**Exemplo do primeiro código (exemplo teórico)**.

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

**Segundo exemplo de código (implementação no mundo real)**

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

**Quando usar**
- Permitir que um sistema use classes de outro sistema que seja incompatível
com isso.
- Permitir a comunicação entre o sistema novo e o já existente que são
independentes entre si
- Ado.Net SqlAdapter, OracleAdapter, MySqlAdapter são os melhores exemplos de
Padrão Adaptador.


[1]: https://en.wikipedia.org/wiki/Adapter_pattern
[2]: https://i.stack.imgur.com/oYMFy.gif

