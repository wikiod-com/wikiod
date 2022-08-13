---
title: "Manejo de excepciones"
slug: "manejo-de-excepciones"
draft: false
images: []
weight: 9143
type: docs
toc: true
---

## Creación de excepciones personalizadas
Puede implementar excepciones personalizadas que se pueden lanzar como cualquier otra excepción. Esto tiene sentido cuando desea que sus excepciones se distingan de otros errores durante el tiempo de ejecución.

En este ejemplo, crearemos una excepción personalizada para un manejo claro de los problemas que la aplicación pueda tener al analizar una entrada compleja.

# Creación de una clase de excepción personalizada

Para crear una excepción personalizada, cree una subclase de `Exception`:

    public class ParserException : Exception
    {
        public ParserException() : 
          base("The parsing went wrong and we have no additional information.") { }
    }

La excepción personalizada se vuelve muy útil cuando desea proporcionar información adicional al receptor:

    public class ParserException : Exception
    {
        public ParserException(string fileName, int lineNumber) : 
          base($"Parser error in {fileName}:{lineNumber}") 
        {
          FileName = fileName;
          LineNumber = lineNumber;
        }
        public string FileName {get; private set;}
        public int LineNumber {get; private set;}    
    }

Ahora, cuando `catch(ParserException x)` tendrá semántica adicional para afinar el manejo de excepciones.

Las clases personalizadas pueden implementar las siguientes funciones para admitir escenarios adicionales.

# volver a lanzar

Durante el proceso de análisis, la excepción original sigue siendo de interés. En este ejemplo, es una `FormatException` porque el código intenta analizar un fragmento de cadena, que se espera que sea un número. En este caso, la excepción personalizada debería admitir la inclusión de '**InnerException**':

    //new constructor:
    ParserException(string msg, Exception inner) : base(msg, inner) {
    }

# serialización

En algunos casos, es posible que sus excepciones deban cruzar los límites de AppDomain. Este es el caso si su analizador se ejecuta en su propio dominio de aplicación para admitir la recarga en caliente de nuevas configuraciones de analizador. En Visual Studio, puede usar la plantilla `Exception` para generar código como este.

    [Serializable]
    public class ParserException : Exception
    {
        // Constructor without arguments allows throwing your exception without
        // providing any information, including error message. Should be included
        // if your exception is meaningful without any additional details. Should
        // set message by calling base constructor (default message is not helpful).
        public ParserException()
            : base("Parser failure.")
        {}

        // Constructor with message argument allows overriding default error message.
        // Should be included if users can provide more helpful messages than
        // generic automatically generated messages.
        public ParserException(string message) 
            : base(message)
        {}

        // Constructor for serialization support. If your exception contains custom
        // properties, read their values here.
        protected ParserException(SerializationInfo info, StreamingContext context) 
            : base(info, context)
        {}
    }

# Usando la ParserException
   
    try
    {
        Process.StartRun(fileName)
    }
    catch (ParserException ex)
    {
        Console.WriteLine($"{ex.Message} in ${ex.FileName}:${ex.LineNumber}");
    }
    catch (PostProcessException x) 
    {
        ...
    }

También puede usar excepciones personalizadas para capturar y envolver excepciones. De esta manera, muchos errores diferentes se pueden convertir en un solo tipo de error que es más útil para la aplicación:

    try
    {
        int foo = int.Parse(token);
    }
    catch (FormatException ex)
    {
        //Assuming you added this constructor
        throw new ParserException(
          $"Failed to read {token} as number.", 
          FileName, 
          LineNumber, 
          ex);
    }

Cuando maneje excepciones generando sus propias excepciones personalizadas, generalmente debe incluir una referencia a la excepción original en la propiedad `InnerException`, como se muestra arriba.

# Preocupaciones de seguridad

Si exponer el motivo de la excepción puede comprometer la seguridad al permitir que los usuarios vean el funcionamiento interno de su aplicación, puede ser una mala idea envolver la excepción interna. Esto podría aplicarse si está creando una biblioteca de clases que otros usuarios utilizarán.

Así es como podría generar una excepción personalizada sin envolver la excepción interna:

    try
    {
      // ...
    }
    catch (SomeStandardException ex)
    {
      // ...
      throw new MyCustomException(someMessage);
    }

# Conclusión

Al generar una excepción personalizada (ya sea con envoltura o con una nueva excepción sin empaquetar), debe generar una excepción que sea significativa para la persona que llama. Por ejemplo, un usuario de una biblioteca de clases puede no saber mucho acerca de cómo esa biblioteca hace su trabajo interno. Las excepciones que generan las dependencias de la biblioteca de clases no son significativas. Más bien, el usuario quiere una excepción que sea relevante para la forma en que la biblioteca de clases usa esas dependencias de manera errónea.

    try
    {
      // ...
    }
    catch (IOException ex)
    {
      // ...
      throw new StorageServiceException(@"The Storage Service encountered a problem saving
    your data. Please consult the inner exception for technical details. 
    If you are not able to resolve the problem, please call 555-555-1234 for technical       
    assistance.", ex);
    }

## Finalmente bloque
    try
    {
        /* code that could throw an exception */
    }
    catch (Exception)
    {
        /* handle the exception */
    }
    finally
    {
        /* Code that will be executed, regardless if an exception was thrown / caught or not */
    }
    
El bloque `try/catch/finally` puede ser muy útil al leer archivos.
Por ejemplo:

    FileStream f = null;
    
    try
    {
        f = File.OpenRead("file.txt");
        /* process the file here */
    }
    finally
    {
        f?.Close(); // f may be null, so use the null conditional operator.
    }


Un bloque de prueba debe ir seguido de un bloque `catch` o `finally`. Sin embargo, dado que no hay un bloque catch, la ejecución provocará la finalización. Antes de la terminación, se ejecutarán las declaraciones dentro del bloque finalmente.

En la lectura de archivos podríamos haber usado un bloque `using` ya que `FileStream` (lo que devuelve `OpenRead`) implementa `IDisposable`.

Incluso si hay una instrucción `return` en el bloque `try`, el bloque `finally` generalmente se ejecutará; hay algunos casos en los que no lo hará:

- Cuando se produce un [StackOverflow][1].
- [`Environment.FailFast`](https://msdn.microsoft.com/en-us/library/system.environment.failfast.aspx)
- El proceso de solicitud se elimina, generalmente por una fuente externa.


[1]: https://msdn.microsoft.com/en-us/library/system.stackoverflowexception(v=vs.110).aspx

## Mejores prácticas
## Hoja de trucos

| HACER| NO |
| ------ | ------ |
| Flujo de control con sentencias de control | Flujo de control con excepciones|
| Realice un seguimiento de la excepción ignorada (absorbida) registrando|Ignorar excepción|
| Repetir la excepción usando `throw`|Re-lanzar excepción - `throw new ArgumentNullException()` o `throw ex` |
| Lanzar excepciones de sistema predefinidas | Lanzar excepciones personalizadas similares a las excepciones del sistema predefinidas |
| Lanzar una excepción personalizada/predefinida si es crucial para la lógica de la aplicación | Lance excepciones personalizadas/predefinidas para indicar una advertencia en el flujo |
| Captura las excepciones que quieras manejar | Captura todas las excepciones |


## NO administre la lógica comercial con excepciones. ##

El control de flujo NO debe hacerse por excepciones. Utilice sentencias condicionales en su lugar. Si un control se puede hacer claramente con la declaración `if-else`, no use excepciones porque reduce la legibilidad y el rendimiento.

Considere el siguiente fragmento de Mr. Bad Practices:

    // This is a snippet example for DO NOT
    object myObject;
    void DoingSomethingWithMyObject()
    {
        Console.WriteLine(myObject.ToString());
    }

Cuando la ejecución llega a `Console.WriteLine(myObject.ToString());`, la aplicación generará una NullReferenceException. El Sr. Malas Prácticas se dio cuenta de que `myObject` es nulo y editó su fragmento para capturar y manejar `NullReferenceException`:

    // This is a snippet example for DO NOT
    object myObject;
    void DoingSomethingWithMyObject()
    {
        try
        {
            Console.WriteLine(myObject.ToString());
        }
        catch(NullReferenceException ex)
        {
            // Hmmm, if I create a new instance of object and assign it to myObject:
            myObject = new object();
            // Nice, now I can continue to work with myObject
            DoSomethingElseWithMyObject();
        }
    }

Dado que el fragmento anterior solo cubre la lógica de excepción, ¿qué debo hacer si `myObject` no es nulo en este punto? ¿Dónde debo cubrir esta parte de la lógica? Justo después de `Console.WriteLine(myObject.ToString());`? ¿Qué tal después del bloque `try...catch`?

¿Qué hay del Sr. Mejores Prácticas? ¿Cómo manejaría esto?

    // This is a snippet example for DO
    object myObject;
    void DoingSomethingWithMyObject()
    {
        if(myObject == null)
            myObject = new object();
        
        // When execution reaches this point, we are sure that myObject is not null
        DoSomethingElseWithMyObject();
    }

Mr. Best Practices logró la misma lógica con menos código y una lógica clara y comprensible.

## NO vuelva a lanzar Excepciones ##

Volver a lanzar excepciones es costoso. Afecta negativamente al rendimiento. Para el código que falla de forma rutinaria, puede usar patrones de diseño para minimizar los problemas de rendimiento. [Este tema][1] describe dos patrones de diseño que son útiles cuando las excepciones pueden afectar significativamente el rendimiento.

## NO absorba excepciones sin registro ##

    try
    {
        //Some code that might throw an exception
    }
    catch(Exception ex)
    {
        //empty catch block, bad practice
    }

Nunca te tragues las excepciones. Ignorar las excepciones salvará ese momento, pero creará un caos para la mantenibilidad más adelante. Al registrar excepciones, siempre debe registrar la instancia de excepción para que se registre el seguimiento completo de la pila y no solo el mensaje de excepción.

    try
    {
        //Some code that might throw an exception
    }
    catch(NullException ex)
    {
        LogManager.Log(ex.ToString());
    }

## No detecte excepciones que no pueda manejar ##

Muchos recursos, como [este][2], le recomiendan encarecidamente que considere por qué detecta una excepción en el lugar en el que la detecta. Solo debe capturar una excepción si puede manejarla en esa ubicación. Si puede hacer algo allí para ayudar a mitigar el problema, como probar un algoritmo alternativo, conectarse a una base de datos de respaldo, probar con otro nombre de archivo, esperar 30 segundos y volver a intentarlo o notificar a un administrador, puede detectar el error y hacerlo. Si no hay nada que pueda hacer de manera plausible y razonable, simplemente "déjelo ir" y deje que la excepción se maneje en un nivel superior. Si la excepción es lo suficientemente catastrófica y no hay otra opción razonable que no sea que todo el programa se bloquee debido a la gravedad del problema, déjelo que se bloquee.

    try
    {
        //Try to save the data to the main database.
    }
    catch(SqlException ex)
    {
        //Try to save the data to the alternative database.
    }
    //If anything other than a SqlException is thrown, there is nothing we can do here. Let the exception bubble up to a level where it can be handled.

[1]: https://msdn.microsoft.com/en-us/library/ms229009(v=vs.100).aspx
[2]: http://c2.com/cgi/wiki?DontCatchExceptions

## Antipatrones de excepción
# Excepciones para tragar

Siempre se debe volver a lanzar la excepción de la siguiente manera:

    try
    {
        ...
    }
    catch (Exception ex)
    {
        ...
        throw;
    }


Volver a lanzar una excepción como la siguiente ofuscará la excepción original y perderá el seguimiento de la pila original. ¡Uno nunca debería hacer esto! Se perderá el seguimiento de la pila antes de la captura y el lanzamiento.

    try
    {
        ...
    }
    catch (Exception ex)
    {
        ...
        throw ex;
    }

# Manejo de excepciones de béisbol

No se deben usar excepciones como un [sustituto de las construcciones de control de flujo normales] [1] como las declaraciones if-then y los bucles while. Este antipatrón a veces se denomina [Manejo de excepciones de béisbol][2].

Aquí hay un ejemplo del anti-patrón:

    try
    {
        while (AccountManager.HasMoreAccounts())
        {
            account = AccountManager.GetNextAccount();
            if (account.Name == userName)
            {
                //We found it
                throw new AccountFoundException(account);
            }
        }
    }
    catch (AccountFoundException found)
    {
        Console.Write("Here are your account details: " + found.Account.Details.ToString());
    }

Aquí hay una mejor manera de hacerlo:

    Account found = null;
    while (AccountManager.HasMoreAccounts() && (found==null))
    {
        account = AccountManager.GetNextAccount();
        if (account.Name == userName)
        {
            //We found it
            found = account;
        }
    }
    Console.Write("Here are your account details: " + found.Details.ToString());

# captura (Excepción)

Casi no hay razones (¡algunos dicen que ninguna!) para capturar el tipo de excepción genérica en su código. Debe capturar solo los tipos de excepción que espera que sucedan, porque de lo contrario ocultará errores en su código.

    try 
    {
         var f = File.Open(myfile);
         // do something
    }
    catch (Exception x)
    {
         // Assume file not found
         Console.Write("Could not open file");
         // but maybe the error was a NullReferenceException because of a bug in the file handling code?
    }

Mejor hazlo:

    try 
    {
         var f = File.Open(myfile);
         // do something which should normally not throw exceptions
    }
    catch (IOException)
    {
         Console.Write("File not found");
    }
    // Unfortunatelly, this one does not derive from the above, so declare separatelly
    catch (UnauthorizedAccessException) 
    {
         Console.Write("Insufficient rights");
    }

Si ocurre cualquier otra excepción, dejamos que la aplicación se bloquee a propósito, de modo que interviene directamente en el depurador y podemos solucionar el problema. No debemos enviar un programa en el que ocurran otras excepciones además de estas, por lo que no es un problema tener un bloqueo.

El siguiente también es un mal ejemplo, porque usa excepciones para evitar un error de programación. Eso no es para lo que están diseñados.

    public void DoSomething(String s)
    {
         if (s == null)
             throw new ArgumentNullException(nameof(s));
         // Implementation goes here
    }
    
    try 
    {    
         DoSomething(myString);
    }
    catch(ArgumentNullException x)
    {
        // if this happens, we have a programming error and we should check
        // why myString was null in the first place.
    }

[1]: http://c2.com/cgi/wiki?DontUseExceptionsForFlowControl
[2]: http://www.stackprinter.com/questions/new-programming-jargon-you-coined.html

## Manejo básico de excepciones
    try
    {
        /* code that could throw an exception */
    }
    catch (Exception ex)
    {
        /* handle the exception */
    }
Tenga en cuenta que manejar todas las excepciones con el mismo código a menudo no es el mejor enfoque.
Esto se usa comúnmente cuando falla cualquier rutina interna de manejo de excepciones, como último recurso.

## Manejo de tipos de excepción específicos
    try
    {
        /* code to open a file */
    }
    catch (System.IO.FileNotFoundException)
    {
        /* code to handle the file being not found */
    }
    catch (System.IO.UnauthorizedAccessException)
    {
        /* code to handle not being allowed access to the file */
    }
    catch (System.IO.IOException)
    {
        /* code to handle IOException or it's descendant other than the previous two */
    }
    catch (System.Exception)
    {
        /* code to handle other errors */
    }

Tenga cuidado de que las excepciones se evalúen en orden y se aplique la herencia. Por lo tanto, debe comenzar con los más específicos y terminar con su antepasado.
En cualquier punto dado, solo se ejecutará un bloque catch.

## Excepciones agregadas/múltiples excepciones de un método
¿Quién dice que no puede lanzar múltiples excepciones en un método? Si no está acostumbrado a jugar con AggregateExceptions, puede sentirse tentado a crear su propia estructura de datos para representar muchas cosas que van mal. Por supuesto, hay otra estructura de datos que no es una excepción sería más ideal, como los resultados de una validación. Incluso si juega con AggregateExceptions, puede estar en el lado receptor y siempre manejándolos sin darse cuenta de que pueden serle útiles.

Es bastante plausible que se ejecute un método y, aunque será una falla en su conjunto, querrá resaltar varias cosas que salieron mal en las excepciones que se lanzan. Como ejemplo, este comportamiento se puede ver con la forma en que funcionan los métodos Parallel si una tarea se dividiera en varios subprocesos y cualquier número de ellos podría generar excepciones y esto debe informarse. Aquí hay un ejemplo tonto de cómo podrías beneficiarte de esto:

        public void Run()
        {
            try
            {
                this.SillyMethod(1, 2);
            }
            catch (AggregateException ex)
            {
                Console.WriteLine(ex.Message);
                foreach (Exception innerException in ex.InnerExceptions)
                {
                    Console.WriteLine(innerException.Message);
                }
            }
        }

        private void SillyMethod(int input1, int input2)
        {
            var exceptions = new List<Exception>();

            if (input1 == 1)
            {
                exceptions.Add(new ArgumentException("I do not like ones"));
            }
            if (input2 == 2)
            {
                exceptions.Add(new ArgumentException("I do not like twos"));
            }
            if (exceptions.Any())
            {
                throw new AggregateException("Funny stuff happended during execution", exceptions);
            }
        }

## Lanzar una excepción
Su código puede, y a menudo debe, generar una excepción cuando sucede algo inusual.

    public void WalkInto(Destination destination)
    {
        if (destination.Name == "Mordor")
        {
            throw new InvalidOperationException("One does not simply walk into Mordor.");
        }
        // ... Implement your normal walking code here.
    }

## Excepción no controlada y subproceso
**AppDomain.UnhandledException**
Este evento proporciona una notificación de excepciones no detectadas. Permite que la aplicación registre información sobre la excepción antes de que el controlador predeterminado del sistema informe la excepción al usuario y finalice la aplicación. Si hay suficiente información disponible sobre el estado de la aplicación, se pueden realizar otras acciones. llevado a cabo, como guardar los datos del programa para una recuperación posterior. Se recomienda precaución, ya que los datos del programa pueden corromperse cuando no se manejan las excepciones.

        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        private static void Main(string[] args)
        {
            AppDomain.CurrentDomain.UnhandledException += new UnhandledExceptionEventHandler(UnhandledException);            
        }
**Aplicación.ThreadException**
Este evento permite que su aplicación de Windows Forms controle las excepciones no controladas que se producen en los subprocesos de Windows Forms. Adjunte sus controladores de eventos al evento ThreadException para manejar estas excepciones, lo que dejará su aplicación en un estado desconocido. Siempre que sea posible, las excepciones deben ser manejadas por un bloque estructurado de manejo de excepciones.

        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        private static void Main(string[] args)
        {
            AppDomain.CurrentDomain.UnhandledException += new UnhandledExceptionEventHandler(UnhandledException);
            Application.ThreadException += new ThreadExceptionEventHandler(ThreadException);
        }
Y finalmente el manejo de excepciones.

    static void UnhandledException(object sender, UnhandledExceptionEventArgs e)
        {
            Exception ex = (Exception)e.ExceptionObject;
            // your code
        }

    static void ThreadException(object sender, ThreadExceptionEventArgs e)
        {
            Exception ex = e.Exception;
            // your code
        }

## Usando el objeto de excepción
Se le permite crear y lanzar excepciones en su propio código.
La instanciación de una excepción se realiza de la misma manera que cualquier otro objeto de C#.

    Exception ex = new Exception();

    // constructor with an overload that takes a message string
    Exception ex = new Exception("Error message"); 

Luego puede usar la palabra clave `throw` para generar la excepción:

    
    try
    {
        throw new Exception("Error");
    }
    catch (Exception ex)
    {
        Console.Write(ex.Message); // Logs 'Error' to the output window
    } 


**Nota:** Si lanza una nueva excepción dentro de un bloque catch, asegúrese de que la excepción original se pase como "excepción interna", p.

    void DoSomething() 
    {
        int b=1; int c=5;
        try
        {
            var a = 1; 
            b = a - 1;
            c = a / b;
            a = a / c;
        }        
        catch (DivideByZeroException dEx) when (b==0)
        {
            // we're throwing the same kind of exception
            throw new DivideByZeroException("Cannot divide by b because it is zero", dEx);
        }
        catch (DivideByZeroException dEx) when (c==0)
        {
            // we're throwing the same kind of exception
            throw new DivideByZeroException("Cannot divide by c because it is zero", dEx);
        }
    }

    void Main()
    {    
        try
        {
            DoSomething();
        }
        catch (Exception ex)
        {
            // Logs full error information (incl. inner exception)
            Console.Write(ex.ToString()); 
        }    
    }

En este caso, se supone que la excepción no se puede manejar, pero se agrega información útil al mensaje (y aún se puede acceder a la excepción original a través de `ex.InnerException` mediante un bloque de excepción externo).

Mostrará algo como:

> System.DivideByZeroException: No se puede dividir por b porque es cero ---> System.DivideByZeroException: Se intentó dividir por cero. <br/>
> en UserQuery.<Principal>g__DoSomething0_0() en C:\[...]\LINQPadQuery.cs:línea 36 <br/>
> --- Fin del seguimiento de la pila de excepción interna --- <br/>
> en UserQuery.<Principal>g__DoSomething0_0() en C:\[...]\LINQPadQuery.cs:line 42 <br/>
> en UserQuery.Main() en C:\[...]\LINQPadQuery.cs:línea 55 <br/>

Si está probando este ejemplo en LinqPad, notará que los números de línea no son muy significativos (no siempre lo ayudan). Pero pasar un texto de error útil como se sugirió anteriormente a menudo reduce significativamente el tiempo para rastrear la ubicación del error, que en este ejemplo es claramente la línea

>c = a/b;

en la función `Hacer Algo()`.

**[Pruébalo en .NET Fiddle](https://dotnetfiddle.net/Widget/JLUXXY)**


## Implementando IErrorHandler para servicios WCF
La implementación de IErrorHandler para los servicios de WCF es una excelente manera de centralizar el registro y el control de errores. La implementación que se muestra aquí debe detectar cualquier excepción no controlada que se genere como resultado de una llamada a uno de sus servicios WCF. En este ejemplo también se muestra cómo devolver un objeto personalizado y cómo devolver JSON en lugar del XML predeterminado.

Implementar IErrorHandler:
 
    using System.ServiceModel.Channels;
    using System.ServiceModel.Dispatcher;
    using System.Runtime.Serialization.Json;
    using System.ServiceModel;
    using System.ServiceModel.Web;

    namespace BehaviorsAndInspectors
    {
        public class ErrorHandler : IErrorHandler
        {

            public bool HandleError(Exception ex)
            {
                // Log exceptions here

                return true;

            } // end

            public void ProvideFault(Exception ex, MessageVersion version, ref Message fault)
            {
                // Get the outgoing response portion of the current context
                var response = WebOperationContext.Current.OutgoingResponse;

                // Set the default http status code 
                response.StatusCode = HttpStatusCode.InternalServerError;

                // Add ContentType header that specifies we are using JSON
                response.ContentType = new MediaTypeHeaderValue("application/json").ToString();

                // Create the fault message that is returned (note the ref parameter) with BaseDataResponseContract                
                fault = Message.CreateMessage(
                    version,
                    string.Empty,
                    new CustomReturnType { ErrorMessage = "An unhandled exception occurred!" },
                    new DataContractJsonSerializer(typeof(BaseDataResponseContract), new List<Type> { typeof(BaseDataResponseContract) }));

                if (ex.GetType() == typeof(VariousExceptionTypes))
                {
                     // You might want to catch different types of exceptions here and process them differently
                }

                // Tell WCF to use JSON encoding rather than default XML
                var webBodyFormatMessageProperty = new WebBodyFormatMessageProperty(WebContentFormat.Json);
                fault.Properties.Add(WebBodyFormatMessageProperty.Name, webBodyFormatMessageProperty);

            } // end

        } // end class

    } // end namespace

En este ejemplo, adjuntamos el controlador al comportamiento del servicio. También puede adjuntar esto a IEndpointBehavior, IContractBehavior o IOperationBehavior de manera similar.
    
Adjuntar a comportamientos de servicio:

    using System;
    using System.Collections.ObjectModel;
    using System.ServiceModel;
    using System.ServiceModel.Channels;
    using System.ServiceModel.Configuration;
    using System.ServiceModel.Description;
    using System.ServiceModel.Dispatcher;

    namespace BehaviorsAndInspectors
    {
        public class ErrorHandlerExtension : BehaviorExtensionElement, IServiceBehavior
        {
            public override Type BehaviorType
            {
                get { return GetType(); }
            }

            protected override object CreateBehavior()
            {
                return this;
            }

            private IErrorHandler GetInstance()
            {
                return new ErrorHandler();
            }

            void IServiceBehavior.AddBindingParameters(ServiceDescription serviceDescription, ServiceHostBase serviceHostBase, Collection<ServiceEndpoint> endpoints, BindingParameterCollection bindingParameters) { } // end

            void IServiceBehavior.ApplyDispatchBehavior(ServiceDescription serviceDescription, ServiceHostBase serviceHostBase)
            {
                var errorHandlerInstance = GetInstance();

                foreach (ChannelDispatcher dispatcher in serviceHostBase.ChannelDispatchers)
                {
                    dispatcher.ErrorHandlers.Add(errorHandlerInstance);
                }
            }

            void IServiceBehavior.Validate(ServiceDescription serviceDescription, ServiceHostBase serviceHostBase) { } // end
          
        } // end class

    } // end namespace

Configuraciones en Web.config:

    ...
    <system.serviceModel>

        <services>      
          <service name="WebServices.MyService">
            <endpoint binding="webHttpBinding" contract="WebServices.IMyService" />
          </service>
        </services>

        <extensions>      
          <behaviorExtensions>        
            <!-- This extension if for the WCF Error Handling-->
            <add name="ErrorHandlerBehavior" type="WebServices.BehaviorsAndInspectors.ErrorHandlerExtensionBehavior, WebServices, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null" />      
          </behaviorExtensions>    
        </extensions>

        <behaviors>          
          <serviceBehaviors>        
            <behavior>
              <serviceMetadata httpGetEnabled="true"/>
              <serviceDebug includeExceptionDetailInFaults="true"/>
              <ErrorHandlerBehavior />
            </behavior>     
          </serviceBehaviors>    
        </behaviors>

        ....
    </system.serviceModel>
    ...

Aquí hay algunos enlaces que pueden ser útiles en este tema:

https://msdn.microsoft.com/en-us/library/system.servicemodel.dispatcher.ierrorhandler(v=vs.100).aspx

http://www.brainthud.com/cards/5218/25441/which-four-behavior-interfaces-exist-for-interacting-with-a-service-or-client-description-what-methods-do-they- implementar-y

Otros ejemplos:

http://stackoverflow.com/questions/38231970/ierrorhandler-returning-wrong-message-body-when-http-status-code-is-401-unauthor

http://stackoverflow.com/questions/3036692/ierrorhandler-doesnt-parece-ser-manejar-mis-errores-en-wcf-any-ideas

http://stackoverflow.com/questions/1149037/how-to-make-custom-wcf-error-handler-return-json-response-with-non-ok-http-code

http://stackoverflow.com/questions/10679214/how-do-you-set-the-content-type-header-for-an-httpclient-request?rq=1

## Anidamiento de excepciones e intente atrapar bloques.
Uno es capaz de anidar un bloque de excepción / `intentar` `atrapar` dentro del otro.

De esta manera, uno puede administrar pequeños bloques de código que son capaces de funcionar sin interrumpir todo su mecanismo.

    try 
    {
    //some code here
        try 
        {
            //some thing which throws an exception. For Eg : divide by 0
        }
        catch (DivideByZeroException dzEx)
        {
            //handle here only this exception
            //throw from here will be passed on to the parent catch block
        }
        finally
        {
            //any thing to do after it is done.
        }
     //resume from here & proceed as normal; 
    }
    catch(Exception e)
    {
        //handle here
    }

**Nota:** Evite [Excepciones de deglución][1] al lanzar al bloque de captura principal


[1]: https://www.wikiod.com/es/docs/c%23/40/exception-handling/6940/exception-anti-patterns#t=201707281310293021372

