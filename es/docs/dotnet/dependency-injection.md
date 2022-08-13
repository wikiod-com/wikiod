---
title: "Inyección de dependencia"
slug: "inyeccion-de-dependencia"
draft: false
images: []
weight: 9876
type: docs
toc: true
---

**Problemas resueltos por inyección de dependencia**

Si no usáramos la inyección de dependencia, la clase `Greeter` se parecería más a esto:

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

Es un "fanático del control" porque controla la creación de la clase que proporciona el saludo, controla de dónde proviene la cadena de conexión SQL y controla la salida.

Usando la inyección de dependencia, la clase 'Greeter' renuncia a esas responsabilidades a favor de una única responsabilidad, escribiendo un saludo proporcionado.

El [Principio de inversión de dependencia][1] sugiere que las clases deben depender de abstracciones (como interfaces) en lugar de otras clases concretas. Las dependencias directas (acoplamiento) entre clases pueden dificultar progresivamente el mantenimiento. Dependiendo de las abstracciones puede reducir ese acoplamiento.

La inyección de dependencia nos ayuda a lograr esa inversión de dependencia porque lleva a escribir clases que dependen de abstracciones. La clase `Greeter` "no sabe" nada en absoluto de los detalles de implementación de `IGreetingProvider` y `IGreetingWriter`. Solo sabe que las dependencias inyectadas implementan esas interfaces. Eso significa que los cambios en las clases concretas que implementan `IGreetingProvider` y `IGreetingWriter` no afectarán a `Greeter`. Tampoco lo hará reemplazarlos con implementaciones completamente diferentes. Solo lo harán los cambios en las interfaces. `Greeter` está desacoplado.

`ControlFreakGreeter` es imposible de realizar una prueba unitaria adecuada. Queremos probar una pequeña unidad de código, pero nuestra prueba incluiría conectarse a SQL y ejecutar un procedimiento almacenado. También incluiría probar la salida de la consola. Debido a que ControlFreakGreeter hace tanto, es imposible realizar pruebas de forma aislada de otras clases.

`Greeter` es fácil de probar porque podemos inyectar implementaciones simuladas de sus dependencias que son más fáciles de ejecutar y verificar que llamar a un procedimiento almacenado o leer la salida de la consola. No requiere una cadena de conexión en app.config.

Las implementaciones concretas de `IGreetingProvider` y `IGreetingWriter` pueden volverse más complejas. Ellos, a su vez, pueden tener sus propias dependencias que se inyectan en ellos. (Por ejemplo, inyectaríamos la cadena de conexión SQL en `SqlGreetingProvider`). Pero esa complejidad está "oculta" de otras clases que solo dependen de las interfaces. Eso hace que sea más fácil modificar una clase sin un "efecto dominó" que nos obligue a realizar los cambios correspondientes en otras clases.

[1]: https://en.wikipedia.org/wiki/Dependency_inversion_principle

## Inyección de dependencia - Ejemplo simple
Esta clase se llama `Greeter`. Su responsabilidad es emitir un saludo. Tiene dos *dependencias*. Necesita algo que le dé el saludo para generar, y luego necesita una forma de generar ese saludo. Esas dependencias se describen como interfaces, `IGreetingProvider` e `IGreetingWriter`. En este ejemplo, esas dos dependencias se "inyectan" en `Greeter`. (Explicación adicional siguiendo el ejemplo.)

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

La clase `Greeting` depende tanto de `IGreetingProvider` como de `IGreetingWriter`, pero no es responsable de crear instancias de ninguno. En cambio, los requiere en su constructor. Lo que sea que cree una instancia de `Saludo` debe proporcionar esas dos dependencias. Podemos llamar a eso "inyectar" las dependencias.

Dado que las dependencias se proporcionan a la clase en su constructor, esto también se denomina "inyección de constructor".

Algunas convenciones comunes:

- El constructor guarda las dependencias como campos `privados`. Tan pronto como se crea una instancia de la clase, esas dependencias están disponibles para todos los demás métodos no estáticos de la clase.
- Los campos `privados` son de `solo lectura`. Una vez que se configuran en el constructor, no se pueden cambiar. Esto indica que esos campos no deben (y no pueden) modificarse fuera del constructor. Eso asegura aún más que esas dependencias estarán disponibles durante la vida útil de la clase.
- Las dependencias son interfaces. Esto no es estrictamente necesario, pero es común porque facilita la sustitución de una implementación de la dependencia por otra. También permite proporcionar una versión simulada de la interfaz para fines de prueba unitaria.

## Cómo la inyección de dependencia facilita las pruebas unitarias
Esto se basa en el ejemplo anterior de la clase `Greeter` que tiene dos dependencias, `IGreetingProvider` y `IGreetingWriter`.

La implementación real de `IGreetingProvider` podría recuperar una cadena de una llamada API o una base de datos. La implementación de `IGreetingWriter` podría mostrar el saludo en la consola. Pero debido a que `Greeter` tiene sus dependencias inyectadas en su constructor, es fácil escribir una prueba unitaria que inyecte versiones simuladas de esas interfaces. En la vida real podríamos usar un marco como [Moq][1], pero en este caso escribiré esas implementaciones simuladas.

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

El comportamiento de `IGreetingProvider` y `IGreetingWriter` no son relevantes para esta prueba. Queremos probar que `Greeter` recibe un saludo y lo escribe. El diseño de `Greeter` (usando inyección de dependencia) nos permite inyectar dependencias simuladas sin partes móviles complicadas. Todo lo que estamos probando es que `Greeter` interactúa con esas dependencias como esperamos.

[1]: http://www.moqthis.com/

## Por qué usamos contenedores de inyección de dependencia (contenedores IoC)
La inyección de dependencia significa escribir clases para que no controlen sus dependencias; en cambio, se les proporcionan sus dependencias ("inyectadas").

Esto no es lo mismo que usar un marco de inyección de dependencia (a menudo llamado "contenedor DI", "contenedor IoC" o simplemente "contenedor") como Castle Windsor, Autofac, SimpleInjector, Ninject, Unity u otros.

Un contenedor simplemente facilita la inyección de dependencia. Por ejemplo, suponga que escribe varias clases que dependen de la inyección de dependencia. Una clase depende de varias interfaces, las clases que implementan esas interfaces dependen de otras interfaces, y así sucesivamente. Algunos dependen de valores específicos. Y solo por diversión, algunas de esas clases implementan `IDisposable` y deben desecharse.

Cada clase individual está bien escrita y es fácil de probar. Pero ahora hay un problema diferente: crear una instancia de una clase se ha vuelto mucho más complicado. Supongamos que estamos creando una instancia de una clase `CustomerService`. Tiene dependencias y sus dependencias tienen dependencias. La construcción de una instancia podría verse así:

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

Quizás se pregunte, ¿por qué no poner toda la construcción gigante en una función separada que simplemente devuelva `CustomerService`? Una razón es que debido a que las dependencias para cada clase se inyectan en ella, una clase no es responsable de saber si esas dependencias son `IDisposable` o de desecharlas. Simplemente los usa. Entonces, si tuviéramos una función `GetCustomerService()` que devolviera un `CustomerService` completamente construido, esa clase podría contener una cantidad de recursos desechables y no hay forma de acceder a ellos o desecharlos.

Y aparte de desechar `IDisposable`, ¿quién quiere llamar así a una serie de constructores anidados? Ese es un breve ejemplo. Podría ponerse mucho, mucho peor. Nuevamente, eso no significa que escribimos las clases de manera incorrecta. Las clases pueden ser individualmente perfectas. El desafío es componerlos juntos.

Un contenedor de inyección de dependencia simplifica eso. Nos permite especificar qué clase o valor se debe usar para cumplir con cada dependencia. Este ejemplo ligeramente simplificado utiliza Castle Windsor:

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

A esto lo llamamos "registro de dependencias" o "configuración del contenedor". Traducido, esto le dice a nuestro `WindsorContainer`:

- Si una clase requiere `ILogWriter`, cree una instancia de `LogWriter`. `LogWriter` requiere una ruta de archivo. Utilice este valor de `AppSettings`.
- Si una clase requiere `IAuthorizationRepository`, cree una instancia de `SqlAuthorizationRepository`. Requiere una cadena de conexión. Utilice este valor de la sección `ConnectionStrings`.
- Si una clase requiere `ICustomerDataProvider`, cree un `CustomerApiClient` y proporcione la cadena que necesita de `AppSettings`.

Cuando solicitamos una dependencia del contenedor, lo llamamos "resolver" una dependencia. Es una mala práctica hacer eso directamente usando el contenedor, pero esa es una historia diferente. Para fines de demostración, ahora podríamos hacer esto:

    var customerService = container.Resolve<CustomerService>();
    var data = customerService.GetCustomerData(customerNumber);
    container.Release(customerService);

El contenedor sabe que `CustomerService` depende de `IAuthorizationRepository` y `ICustomerDataProvider`. Sabe qué clases necesita crear para cumplir con esos requisitos. Esas clases, a su vez, tienen más dependencias y el contenedor sabe cómo satisfacerlas. Creará cada clase que necesite hasta que pueda devolver una instancia de `CustomerService`.

Si llega a un punto en el que una clase requiere una dependencia que no hemos registrado, como `IDoesSomethingElse`, cuando intentemos resolver `CustomerService` lanzará una clara excepción que nos indicará que no hemos registrado nada que cumplir. ese requisito

Cada marco DI se comporta de manera un poco diferente, pero por lo general nos dan cierto control sobre cómo se instancian ciertas clases. Por ejemplo, ¿queremos que cree una instancia de `LogWriter` y la proporcione a cada clase que dependa de `ILogWriter`, o queremos que cree una nueva cada vez? La mayoría de los contenedores tienen una forma de especificar eso.

¿Qué pasa con las clases que implementan `IDisposable`? Por eso llamamos `container.Release(customerService);` al final. La mayoría de los contenedores (incluido Windsor) retrocederán a través de todas las dependencias creadas y "Dispose" las que necesitan eliminarse. Si `CustomerService` es `IDisposable`, también lo eliminará.

El registro de dependencias como se ve arriba podría parecer más código para escribir. Pero cuando tenemos muchas clases con muchas dependencias, realmente vale la pena. Y si tuviéramos que escribir esas mismas clases *sin* usar la inyección de dependencia, entonces esa misma aplicación con muchas clases sería difícil de mantener y probar.

Esto rasca la superficie de *por qué* usamos contenedores de inyección de dependencia. *Cómo* configuramos nuestra aplicación para usar uno (y usarlo correctamente) no es solo un tema, es una serie de temas, ya que las instrucciones y los ejemplos varían de un contenedor a otro.

