---
title: "Injeção de dependência"
slug: "injecao-de-dependencia"
draft: false
images: []
weight: 9876
type: docs
toc: true
---

**Problemas resolvidos por injeção de dependência**

Se não usássemos injeção de dependência, a classe `Greeter` poderia se parecer mais com isso:

    public class ControlFreakGreeter
    {
        public void Greet()
        {
            var greetingProvider = new SqlGreetingProvider(
                ConfigurationManager.ConnectionStrings["myConnectionString"].ConnectionString);
            var greeting = greetingProvider.GetGreeting();
            Console.WriteLine(greeting);
        }
    }

É um "control freak" porque controla a criação da classe que fornece a saudação, controla de onde vem a string de conexão SQL e controla a saída.

Usando injeção de dependência, a classe `Greeter` renuncia a essas responsabilidades em favor de uma única responsabilidade, escrevendo uma saudação fornecida a ela.

O [Princípio da Inversão de Dependência][1] sugere que as classes devem depender de abstrações (como interfaces) em vez de outras classes concretas. Dependências diretas (acoplamento) entre classes podem dificultar progressivamente a manutenção. Dependendo de abstrações pode reduzir esse acoplamento.

A injeção de dependência nos ajuda a alcançar essa inversão de dependência porque leva a escrever classes que dependem de abstrações. A classe `Greeter` "não sabe" nada dos detalhes de implementação de `IGreetingProvider` e `IGreetingWriter`. Ele só sabe que as dependências injetadas implementam essas interfaces. Isso significa que mudanças nas classes concretas que implementam `IGreetingProvider` e `IGreetingWriter` não afetarão o `Greeter`. Nem irá substituí-los por implementações totalmente diferentes. Apenas as alterações nas interfaces serão. `Greeter` é desacoplado.

`ControlFreakGreeter` é impossível testar corretamente a unidade. Queremos testar uma pequena unidade de código, mas, em vez disso, nosso teste inclui a conexão com o SQL e a execução de um procedimento armazenado. Também incluiria testar a saída do console. Como o ControlFreakGreeter faz tanto, é impossível testar isoladamente de outras classes.

`Greeter` é fácil de testar de unidade porque podemos injetar implementações simuladas de suas dependências que são mais fáceis de executar e verificar do que chamar um procedimento armazenado ou ler a saída do console. Ele não requer uma string de conexão em app.config.

As implementações concretas de `IGreetingProvider` e `IGreetingWriter` podem se tornar mais complexas. Eles, por sua vez, podem ter suas próprias dependências que são injetadas neles. (Por exemplo, injetaríamos a string de conexão SQL em `SqlGreetingProvider`.) Mas essa complexidade está "escondida" de outras classes que dependem apenas das interfaces. Isso facilita a modificação de uma classe sem um "efeito cascata" que exige que façamos alterações correspondentes em outras classes.

[1]: https://en.wikipedia.org/wiki/Dependency_inversion_principle

## Injeção de Dependência - Exemplo simples
Esta classe é chamada de `Greeter`. Sua responsabilidade é a saída de uma saudação. Tem duas *dependências*. Ele precisa de algo que forneça a saudação para a saída e, em seguida, precisa de uma maneira de enviar essa saudação. Essas dependências são descritas como interfaces, `IGreetingProvider` e `IGreetingWriter`. Neste exemplo, essas duas dependências são "injetadas" no `Greeter`. (Explicação adicional seguindo o exemplo.)

    public class Greeter
    {
        private readonly IGreetingProvider _greetingProvider;
        private readonly IGreetingWriter _greetingWriter;

        public Greeter(IGreetingProvider greetingProvider, IGreetingWriter greetingWriter)
        {
            _greetingProvider = greetingProvider;
            _greetingWriter = greetingWriter;
        }

        public void Greet()
        {
            var greeting = _greetingProvider.GetGreeting();
            _greetingWriter.WriteGreeting(greeting);
        }
    }
  
    public interface IGreetingProvider
    {
        string GetGreeting();
    }

    public interface IGreetingWriter
    {
        void WriteGreeting(string greeting);
    }

A classe `Greeting` depende de `IGreetingProvider` e `IGreetingWriter`, mas não é responsável por criar instâncias de ambos. Em vez disso, ele os requer em seu construtor. O que quer que crie uma instância de `Greeting` deve fornecer essas duas dependências. Podemos chamar isso de "injetar" as dependências.

Como as dependências são fornecidas à classe em seu construtor, isso também é chamado de "injeção de construtor".

Algumas convenções comuns:

- O construtor salva as dependências como campos `privados`. Assim que a classe for instanciada, essas dependências estarão disponíveis para todos os outros métodos não estáticos da classe.
- Os campos `private` são `readonly`. Uma vez definidos no construtor, eles não podem ser alterados. Isso indica que esses campos não devem (e não podem) ser modificados fora do construtor. Isso garante ainda mais que essas dependências estarão disponíveis durante o tempo de vida da classe.
- As dependências são interfaces. Isso não é estritamente necessário, mas é comum porque facilita a substituição de uma implementação da dependência por outra. Também permite fornecer uma versão simulada da interface para fins de teste de unidade.

## Como a injeção de dependência facilita o teste de unidade
Isso se baseia no exemplo anterior da classe `Greeter` que tem duas dependências, `IGreetingProvider` e `IGreetingWriter`.

A implementação real de `IGreetingProvider` pode recuperar uma string de uma chamada de API ou de um banco de dados. A implementação de `IGreetingWriter` pode exibir a saudação no console. Mas como o `Greeter` tem suas dependências injetadas em seu construtor, é fácil escrever um teste de unidade que injeta versões simuladas dessas interfaces. Na vida real, podemos usar um framework como [Moq][1], mas neste caso escreverei essas implementações simuladas.

    public class TestGreetingProvider : IGreetingProvider
    {
        public const string TestGreeting = "Hello!";

        public string GetGreeting()
        {
            return TestGreeting;
        }
    }

    public class TestGreetingWriter : List<string>, IGreetingWriter
    {
        public void WriteGreeting(string greeting)
        {
            Add(greeting);
        }
    }

    [TestClass]
    public class GreeterTests
    {
        [TestMethod]
        public void Greeter_WritesGreeting()
        {
            var greetingProvider = new TestGreetingProvider();
            var greetingWriter = new TestGreetingWriter();
            var greeter = new Greeter(greetingProvider, greetingWriter);
            greeter.Greet();
            Assert.AreEqual(greetingWriter[0], TestGreetingProvider.TestGreeting);
        }
    }

O comportamento de `IGreetingProvider` e `IGreetingWriter` não são relevantes para este teste. Queremos testar se o `Greeter` recebe uma saudação e a escreve. O design do `Greeter` (usando injeção de dependência) nos permite injetar dependências simuladas sem partes móveis complicadas. Tudo o que estamos testando é que o `Greeter` interage com essas dependências como esperamos.

[1]: http://www.moqthis.com/

## Por que usamos contêineres de injeção de dependência (contêineres IoC)
Injeção de dependência significa escrever classes para que elas não controlem suas dependências - em vez disso, suas dependências são fornecidas a elas ("injetadas").

Isso não é a mesma coisa que usar uma estrutura de injeção de dependência (geralmente chamada de "contêiner DI", "contêiner IoC" ou apenas "contêiner") como Castle Windsor, Autofac, SimpleInjector, Ninject, Unity ou outros.

Um contêiner apenas facilita a injeção de dependência. Por exemplo, suponha que você escreva várias classes que dependem de injeção de dependência. Uma classe depende de várias interfaces, as classes que implementam essas interfaces dependem de outras interfaces e assim por diante. Alguns dependem de valores específicos. E apenas por diversão, algumas dessas classes implementam `IDisposable` e precisam ser descartadas.

Cada classe individual é bem escrita e fácil de testar. Mas agora há um problema diferente: criar uma instância de uma classe se tornou muito mais complicado. Suponha que estamos criando uma instância de uma classe `CustomerService`. Ele tem dependências e suas dependências têm dependências. A construção de uma instância pode ser algo assim:

    public CustomerData GetCustomerData(string customerNumber)
    {
        var customerApiEndpoint = ConfigurationManager.AppSettings["customerApi:customerApiEndpoint"];
        var logFilePath = ConfigurationManager.AppSettings["logwriter:logFilePath"];
        var authConnectionString = ConfigurationManager.ConnectionStrings["authorization"].ConnectionString;
        using(var logWriter = new LogWriter(logFilePath ))
        {
            using(var customerApiClient = new CustomerApiClient(customerApiEndpoint))
            {
                var customerService = new CustomerService(
                    new SqlAuthorizationRepository(authorizationConnectionString, logWriter),
                    new CustomerDataRepository(customerApiClient, logWriter),
                    logWriter
                );   
                
                // All this just to create an instance of CustomerService!         
                return customerService.GetCustomerData(string customerNumber);
            }
        }
    }

Você pode se perguntar, por que não colocar toda a construção gigante em uma função separada que apenas retorna `CustomerService`? Uma razão é que, como as dependências de cada classe são injetadas nela, uma classe não é responsável por saber se essas dependências são 'IDisposable' ou descartá-las. Ele apenas os usa. Portanto, se tivéssemos uma função `GetCustomerService()` que retornasse um `CustomerService` totalmente construído, essa classe poderia conter vários recursos descartáveis ​​e nenhuma maneira de acessá-los ou eliminá-los.

E além de descartar `IDisposable`, quem quer chamar uma série de construtores aninhados como esse, nunca? Esse é um exemplo curto. Pode ficar muito, muito pior. Novamente, isso não significa que escrevemos as classes da maneira errada. As aulas podem ser individualmente perfeitas. O desafio é compô-los juntos.

Um contêiner de injeção de dependência simplifica isso. Ele nos permite especificar qual classe ou valor deve ser usado para preencher cada dependência. Este exemplo um pouco simplificado usa Castle Windsor:

    var container = new WindsorContainer()
    container.Register(
        Component.For<CustomerService>(),
        Component.For<ILogWriter, LogWriter>()
            .DependsOn(Dependency.OnAppSettingsValue("logFilePath", "logWriter:logFilePath")),
        Component.For<IAuthorizationRepository, SqlAuthorizationRepository>()
            .DependsOn(Dependency.OnValue(connectionString, ConfigurationManager.ConnectionStrings["authorization"].ConnectionString)),
        Component.For<ICustomerDataProvider, CustomerApiClient>()
             .DependsOn(Dependency.OnAppSettingsValue("apiEndpoint", "customerApi:customerApiEndpoint"))   
    );

Chamamos isso de "registrar dependências" ou "configurar o contêiner". Traduzido, isso diz ao nosso `WindsorContainer`:

- Se uma classe requer `ILogWriter`, crie uma instância de `LogWriter`. `LogWriter` requer um caminho de arquivo. Use este valor de `AppSettings`.
- Se uma classe requer `IAuthorizationRepository`, crie uma instância de `SqlAuthorizationRepository`. Requer uma cadeia de conexão. Use este valor da seção `ConnectionStrings`.
- Se uma classe requer `ICustomerDataProvider`, crie um `CustomerApiClient` e forneça a string necessária de `AppSettings`.

Quando solicitamos uma dependência do contêiner, chamamos isso de "resolver" uma dependência. É uma má prática fazer isso diretamente usando o contêiner, mas isso é outra história. Para fins de demonstração, podemos agora fazer isso:

    var customerService = container.Resolve<CustomerService>();
    var data = customerService.GetCustomerData(customerNumber);
    container.Release(customerService);

O contêiner sabe que `CustomerService` depende de `IAuthorizationRepository` e `ICustomerDataProvider`. Ele sabe quais classes precisa criar para atender a esses requisitos. Essas classes, por sua vez, têm mais dependências, e o contêiner sabe como preenchê-las. Ele criará todas as classes necessárias até que possa retornar uma instância de `CustomerService`.

Se chegar a um ponto em que uma classe requer uma dependência que não registramos, como `IDoesSomethingElse`, quando tentarmos resolver `CustomerService`, ele lançará uma exceção clara nos informando que não registramos nada para atender esse requisito.

Cada estrutura de DI se comporta de maneira um pouco diferente, mas normalmente eles nos dão algum controle sobre como certas classes são instanciadas. Por exemplo, queremos que ele crie uma instância de `LogWriter` e a forneça para todas as classes que dependem de `ILogWriter`, ou queremos que ele crie uma nova todas as vezes? A maioria dos contêineres tem uma maneira de especificar isso.

E as classes que implementam `IDisposable`? É por isso que chamamos `container.Release(customerService);` no final. A maioria dos contêineres (incluindo o Windsor) retrocederá por todas as dependências criadas e 'Descarta' aquelas que precisam ser descartadas. Se `CustomerService` for `IDisposable`, ele também o descartará.

Registrar dependências como visto acima pode parecer mais código para escrever. Mas quando temos muitas classes com muitas dependências, isso realmente compensa. E se tivéssemos que escrever essas mesmas classes *sem* usar injeção de dependência, então esse mesmo aplicativo com muitas classes se tornaria difícil de manter e testar.

Isso arranha a superfície do *por que* usamos contêineres de injeção de dependência. *Como* configuramos nosso aplicativo para usar um (e usá-lo corretamente) não é apenas um tópico - são vários tópicos, pois as instruções e os exemplos variam de um contêiner para outro.

