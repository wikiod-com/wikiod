---
title: "Manipulação de exceção"
slug: "manipulacao-de-excecao"
draft: false
images: []
weight: 9143
type: docs
toc: true
---

## Criando exceções personalizadas
Você tem permissão para implementar exceções personalizadas que podem ser lançadas como qualquer outra exceção. Isso faz sentido quando você deseja tornar suas exceções distinguíveis de outros erros durante o tempo de execução.

Neste exemplo, criaremos uma exceção personalizada para tratamento claro de problemas que o aplicativo pode ter ao analisar uma entrada complexa.

# Criando uma classe de exceção personalizada

Para criar uma exceção personalizada, crie uma subclasse de `Exception`:

    public class ParserException : Exception
    {
        public ParserException() : 
          base("The parsing went wrong and we have no additional information.") { }
    }

A exceção personalizada se torna muito útil quando você deseja fornecer informações adicionais ao coletor:

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

Agora, quando você `catch(ParserException x)`, você terá semântica adicional para ajustar o tratamento de exceções.

As classes personalizadas podem implementar os seguintes recursos para dar suporte a cenários adicionais.

#relançamento

Durante o processo de análise, a exceção original ainda é de interesse. Neste exemplo, é um `FormatException` porque o código tenta analisar um pedaço de string, que deve ser um número. Nesse caso, a exceção personalizada deve suportar a inclusão de '**InnerException**':

    //new constructor:
    ParserException(string msg, Exception inner) : base(msg, inner) {
    }

# serialização

Em alguns casos, suas exceções podem ter que cruzar os limites do AppDomain. Este é o caso se o seu analisador estiver sendo executado em seu próprio AppDomain para dar suporte ao recarregamento a quente de novas configurações do analisador. No Visual Studio, você pode usar o modelo `Exception` para gerar código como este.

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

# Usando o ParserException
   
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

Você também pode usar exceções personalizadas para capturar e encapsular exceções. Dessa forma, muitos erros diferentes podem ser convertidos em um único tipo de erro mais útil para a aplicação:

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

Ao lidar com exceções levantando suas próprias exceções personalizadas, geralmente você deve incluir uma referência à exceção original na propriedade `InnerException`, conforme mostrado acima.

# Preocupações com segurança

Se expor o motivo da exceção pode comprometer a segurança ao permitir que os usuários vejam o funcionamento interno do seu aplicativo, pode ser uma má ideia encapsular a exceção interna. Isso pode se aplicar se você estiver criando uma biblioteca de classes que será usada por outras pessoas.

Aqui está como você pode gerar uma exceção personalizada sem envolver a exceção interna:

    try
    {
      // ...
    }
    catch (SomeStandardException ex)
    {
      // ...
      throw new MyCustomException(someMessage);
    }

# Conclusão

Ao gerar uma exceção personalizada (com encapsulamento ou com uma nova exceção não encapsulada), você deve gerar uma exceção que seja significativa para o chamador. Por exemplo, um usuário de uma biblioteca de classes pode não saber muito sobre como essa biblioteca faz seu trabalho interno. As exceções lançadas pelas dependências da biblioteca de classes não são significativas. Em vez disso, o usuário deseja uma exceção que seja relevante para como a biblioteca de classes está usando essas dependências de maneira errônea.

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

## Finalmente bloquear
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
    
O bloco `try / catch / finally` pode ser muito útil ao ler arquivos.
Por exemplo:

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


Um bloco try deve ser seguido por um bloco `catch` ou `finally`. No entanto, como não há bloco catch, a execução causará o encerramento. Antes do término, as instruções dentro do bloco finally serão executadas.

Na leitura do arquivo poderíamos ter usado um bloco `using` como `FileStream` (o que `OpenRead` retorna) implementa `IDisposable`.

Mesmo se houver uma instrução `return` no bloco `try`, o bloco `finally` normalmente será executado; existem alguns casos em que não vai:

- Quando ocorre um [StackOverflow][1].
- [`Environment.FailFast`](https://msdn.microsoft.com/en-us/library/system.environment.failfast.aspx)
- O processo do aplicativo é encerrado, geralmente por uma fonte externa.


[1]: https://msdn.microsoft.com/en-us/library/system.stackoverflowexception(v=vs.110).aspx

## Melhores Práticas
## Folha de dicas

| FAÇA| NÃO |
| ------ | ------ |
| Fluxo de controle com instruções de controle | Controle de fluxo com exceções|
| Acompanhe a exceção ignorada (absorvida) registrando|Ignorar exceção|
| Repita a exceção usando `throw`|Re-throw exception - `throw new ArgumentNullException()` ou `throw ex` |
| Lança exceções de sistema predefinidas| Lançar exceções personalizadas semelhantes às exceções de sistema predefinidas|
| Lançar exceção personalizada/predefinida se for crucial para a lógica do aplicativo | Lance exceções personalizadas/predefinidas para declarar um aviso em flow|
| Capturar exceções que você deseja manipular| Pegue todas as exceções |


## NÃO gerencie a lógica de negócios com exceções. ##

O controle de fluxo NÃO deve ser feito por exceções. Use instruções condicionais em vez disso. Se um controle pode ser feito com a instrução `if-else` claramente, não use exceções porque isso reduz a legibilidade e o desempenho.

Considere o seguinte trecho de Mr. Bad Practices:

    // This is a snippet example for DO NOT
    object myObject;
    void DoingSomethingWithMyObject()
    {
        Console.WriteLine(myObject.ToString());
    }

Quando a execução atinge `Console.WriteLine(myObject.ToString());` o aplicativo lançará uma NullReferenceException. Mr. Bad Practices percebeu que `myObject` é nulo e editou seu snippet para capturar e manipular `NullReferenceException`:

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

Como o trecho anterior cobre apenas a lógica de exceção, o que devo fazer se `myObject` não for nulo neste momento? Onde devo cobrir essa parte da lógica? Logo após `Console.WriteLine(myObject.ToString());`? Que tal depois do bloco `try...catch`?

Que tal Mr. Best Practices? Como ele lidaria com isso?

    // This is a snippet example for DO
    object myObject;
    void DoingSomethingWithMyObject()
    {
        if(myObject == null)
            myObject = new object();
        
        // When execution reaches this point, we are sure that myObject is not null
        DoSomethingElseWithMyObject();
    }

Mr. Best Practices alcançou a mesma lógica com menos código e uma lógica clara e compreensível.

## NÃO lance novamente Exceções ##

Relançar exceções é caro. Afeta negativamente o desempenho. Para código que falha rotineiramente, você pode usar padrões de design para minimizar problemas de desempenho. [Este tópico][1] descreve dois padrões de design que são úteis quando as exceções podem afetar significativamente o desempenho.

## NÃO absorva exceções sem registro ##

    try
    {
        //Some code that might throw an exception
    }
    catch(Exception ex)
    {
        //empty catch block, bad practice
    }

Nunca engula exceções. Ignorar exceções salvará esse momento, mas criará um caos para a manutenção mais tarde. Ao registrar exceções, você deve sempre registrar a instância de exceção para que o rastreamento de pilha completo seja registrado e não apenas a mensagem de exceção.

    try
    {
        //Some code that might throw an exception
    }
    catch(NullException ex)
    {
        LogManager.Log(ex.ToString());
    }

## Não capture exceções que você não pode manipular ##

Muitos recursos, como [este] [2], recomendam fortemente que você considere por que está capturando uma exceção no local em que a está capturando. Você só deve capturar uma exceção se puder tratá-la nesse local. Se você puder fazer algo lá para ajudar a mitigar o problema, como tentar um algoritmo alternativo, conectar-se a um banco de dados de backup, tentar outro nome de arquivo, aguardar 30 segundos e tentar novamente ou notificar um administrador, poderá detectar o erro e fazer isso. Se não houver nada que você possa fazer de maneira plausível e razoável, apenas "deixe ir" e deixe a exceção ser tratada em um nível mais alto. Se a exceção for suficientemente catastrófica e não houver outra opção razoável que não seja o travamento de todo o programa devido à gravidade do problema, deixe-o travar.

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

## Antipadrões de exceção
# Exceções de deglutição

Deve-se sempre relançar a exceção da seguinte maneira:

    try
    {
        ...
    }
    catch (Exception ex)
    {
        ...
        throw;
    }


Relançar uma exceção como abaixo irá ofuscar a exceção original e perderá o rastreamento de pilha original. Nunca se deve fazer isso! O rastreamento de pilha antes da captura e relançamento será perdido.

    try
    {
        ...
    }
    catch (Exception ex)
    {
        ...
        throw ex;
    }

# Tratamento de exceção de beisebol

Não se deve usar exceções como um [substituto para construções de controle de fluxo normal][1] como instruções if-then e loops while. Esse antipadrão às vezes é chamado de [Manuseio de Exceções de Basebol][2].

Aqui está um exemplo do anti-padrão:

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

Aqui está uma maneira melhor de fazer isso:

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

# captura (exceção)

Quase não há (alguns dizem que nenhum!) motivos para capturar o tipo de exceção genérico em seu código. Você deve capturar apenas os tipos de exceção que espera que aconteçam, porque, caso contrário, ocultará bugs em seu código.

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

Melhor fazer:

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

Se qualquer outra exceção acontecer, nós propositadamente deixamos o aplicativo travar, para que ele entre diretamente no depurador e possamos corrigir o problema. Nós não devemos enviar um programa onde quaisquer outras exceções que não essas aconteçam de qualquer maneira, então não é um problema ter uma falha.

O exemplo a seguir também é ruim, porque usa exceções para contornar um erro de programação. Não é para isso que eles foram projetados.

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

## Tratamento básico de exceções
    try
    {
        /* code that could throw an exception */
    }
    catch (Exception ex)
    {
        /* handle the exception */
    }
Observe que lidar com todas as exceções com o mesmo código geralmente não é a melhor abordagem.
Isso é comumente usado quando qualquer rotina de tratamento de exceção interna falha, como último recurso.

## Manipulando tipos de exceção específicos
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

Tenha cuidado para que as exceções sejam avaliadas em ordem e a herança seja aplicada. Então você precisa começar com os mais específicos e terminar com seu ancestral.
Em qualquer ponto, apenas um bloco catch será executado.

## Exceções agregadas/exceções múltiplas de um método
Quem disse que você não pode lançar várias exceções em um método. Se você não está acostumado a brincar com AggregateExceptions, pode ficar tentado a criar sua própria estrutura de dados para representar muitas coisas que estão dando errado. É claro que há outra estrutura de dados que não seja uma exceção seria mais ideal, como os resultados de uma validação. Mesmo que você jogue com AggregateExceptions, você pode estar do lado receptor e sempre lidando com eles sem perceber que eles podem ser úteis para você.

É bastante plausível que um método seja executado e, mesmo que seja uma falha como um todo, você desejará destacar várias coisas que deram errado nas exceções lançadas. Como exemplo, esse comportamento pode ser visto em como os métodos paralelos funcionam quando uma tarefa é dividida em vários threads e qualquer número deles pode gerar exceções e isso precisa ser relatado. Aqui está um exemplo bobo de como você pode se beneficiar disso:

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

## Lançando uma exceção
Seu código pode, e muitas vezes deve, lançar uma exceção quando algo incomum acontece.

    public void WalkInto(Destination destination)
    {
        if (destination.Name == "Mordor")
        {
            throw new InvalidOperationException("One does not simply walk into Mordor.");
        }
        // ... Implement your normal walking code here.
    }

## Exceção sem tratamento e thread
**AppDomain.UnhandledException**
Este evento fornece notificação de exceções não capturadas. Ele permite que o aplicativo registre informações sobre a exceção antes que o manipulador padrão do sistema relate a exceção ao usuário e encerre o aplicativo. Se informações suficientes sobre o estado do aplicativo estiverem disponíveis, outras ações podem ser realizadas — como salvar dados do programa para recuperação posterior. Recomenda-se cuidado, pois os dados do programa podem ser corrompidos quando as exceções não são tratadas.

        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        private static void Main(string[] args)
        {
            AppDomain.CurrentDomain.UnhandledException += new UnhandledExceptionEventHandler(UnhandledException);            
        }
**Application.ThreadException**
Esse evento permite que seu aplicativo Windows Forms manipule exceções sem tratamento que ocorrem em threads Windows Forms. Anexe seus manipuladores de eventos ao evento ThreadException para lidar com essas exceções, que deixarão seu aplicativo em um estado desconhecido. Sempre que possível, as exceções devem ser tratadas por um bloco de tratamento de exceção estruturado.

        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        private static void Main(string[] args)
        {
            AppDomain.CurrentDomain.UnhandledException += new UnhandledExceptionEventHandler(UnhandledException);
            Application.ThreadException += new ThreadExceptionEventHandler(ThreadException);
        }
E finalmente o tratamento de exceções

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

## Usando o objeto de exceção
Você tem permissão para criar e lançar exceções em seu próprio código.
A instanciação de uma exceção é feita da mesma forma que qualquer outro objeto C#.

    Exception ex = new Exception();

    // constructor with an overload that takes a message string
    Exception ex = new Exception("Error message"); 

Você pode então usar a palavra-chave `throw` para gerar a exceção:

    
    try
    {
        throw new Exception("Error");
    }
    catch (Exception ex)
    {
        Console.Write(ex.Message); // Logs 'Error' to the output window
    } 


**Observação:** se você estiver lançando uma nova exceção dentro de um bloco catch, certifique-se de que a exceção original seja passada como "exceção interna", por exemplo,

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

Neste caso, assume-se que a exceção não pode ser tratada, mas algumas informações úteis são adicionadas à mensagem (e a exceção original ainda pode ser acessada via `ex.InnerException` por um bloco de exceção externo).

Ele mostrará algo como:

> System.DivideByZeroException: Não é possível dividir por b porque é zero ---> System.DivideByZeroException: Tentativa de dividir por zero. <br/>
> em UserQuery.<Main>g__DoSomething0_0() em C:\[...]\LINQPadQuery.cs:line 36 <br/>
> --- Fim do rastreamento de pilha de exceção interna --- <br/>
> em UserQuery.<Main>g__DoSomething0_0() em C:\[...]\LINQPadQuery.cs:line 42 <br/>
> em UserQuery.Main() em C:\[...]\LINQPadQuery.cs:line 55 <br/>

Se você estiver tentando este exemplo no LinqPad, notará que os números de linha não são muito significativos (nem sempre ajudam você). Mas passar um texto de erro útil, conforme sugerido acima, muitas vezes reduz significativamente o tempo para rastrear a localização do erro, que neste exemplo é claramente a linha

> c = a/b;

na função `DoSomething()`.

**[Experimente no .NET Fiddle](https://dotnetfiddle.net/Widget/JLUXXY)**


## Implementando IErrorHandler para serviços WCF
A implementação de IErrorHandler para serviços WCF é uma ótima maneira de centralizar a manipulação e o registro de erros. A implementação mostrada aqui deve capturar qualquer exceção não tratada que seja lançada como resultado de uma chamada para um de seus serviços WCF. Também é mostrado neste exemplo como retornar um objeto personalizado e como retornar JSON em vez do XML padrão.

Implemente IErrorHandler:
 
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

Neste exemplo, anexamos o manipulador ao comportamento do serviço. Você também pode anexar isso a IEndpointBehavior, IContractBehavior ou IOperationBehavior de maneira semelhante.
    
Anexar aos comportamentos de serviço:

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

Configurações em Web.config:

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

Aqui estão alguns links que podem ser úteis sobre este tema:

https://msdn.microsoft.com/en-us/library/system.servicemodel.dispatcher.ierrorhandler(v=vs.100).aspx

http://www.brainthud.com/cards/5218/25441/which-four-behavior-interfaces-exist-for-interacting-with-a-service-or-client-description-what-methods-do-they- implementar-e

Outros exemplos:

http://stackoverflow.com/questions/38231970/ierrorhandler-returning-wrong-message-body-when-http-status-code-is-401-unauthor

http://stackoverflow.com/questions/3036692/ierrorhandler-doesnt-seem-to-be-handling-my-errors-in-wcf-any-ideas

http://stackoverflow.com/questions/1149037/how-to-make-custom-wcf-error-handler-return-json-response-with-non-ok-http-code

http://stackoverflow.com/questions/10679214/how-do-you-set-the-content-type-header-for-an-httpclient-request?rq=1

## Aninhamento de exceções e tente blocos de captura.
Um é capaz de aninhar um bloco de exceção / `try` `catch` dentro do outro.

Desta forma, pode-se gerenciar pequenos blocos de código que são capazes de funcionar sem interromper todo o seu mecanismo.

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

**Observação:** evite [exceções de deglutição][1] ao lançar para o bloco catch pai


[1]: https://www.wikiod.com/pt/docs/c%23/40/exception-handling/6940/exception-anti-patterns#t=201707281310293021372

