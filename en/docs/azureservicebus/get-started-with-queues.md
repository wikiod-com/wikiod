---
title: "Get started with queues"
slug: "get-started-with-queues"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

# Sample code
More sample code can be found [here][azure-servicebus-messaging-samples].


  [azure-servicebus-messaging-samples]: https://github.com/Azure-Samples/azure-servicebus-messaging-samples

## 1. Create a namespace using the Azure portal
1. Log on to the Azure classic portal.

2. In the left navigation pane of the portal, click **Service Bus**.

3. In the lower pane of the portal, click **Create**.

    ![Select Create][select-create]
   
4. In the **Add a new namespace** dialog, enter a namespace name. The system immediately checks to see if the name is available.

    ![Namespace name][namespace-name]
  
5. After making sure the namespace name is available, choose the country or region in which your namespace should be hosted.

6. Leave the other fields in the dialog with their default values (**Messaging** and **Standard Tier**), then click the OK check mark. The system now creates your namespace and enables it. You might have to wait several minutes as the system provisions resources for your account.
 
    ![Created successfully][created-successfully]

###Obtain the credentials
1. In the left navigation pane, click the **Service Bus** node, to display the list of available namespaces:
 
    ![Select service bus][select-service-bus]
  
2. Select the namespace you just created from the list shown:
 
    ![Select namespace][select-namespace]
 
3. Click **Connection Information**.

    ![Connection information][connection-information]
  
4. In the **Access connection information** pane, find the connection string that contains the SAS key and key name.

    ![Access connection information][access-connection-information]
  
5. Make a note of the key, or copy it to the clipboard.

<!--Image references-->

[select-create]: https://raw.githubusercontent.com/Azure/azure-content/master/includes/media/service-bus-create-namespace-portal/select-create.png
[namespace-name]: https://raw.githubusercontent.com/Azure/azure-content/master/includes/media/service-bus-create-namespace-portal/namespace-name.png
[created-successfully]: https://raw.githubusercontent.com/Azure/azure-content/master/includes/media/service-bus-create-namespace-portal/created-successfully.png
[select-service-bus]: https://raw.githubusercontent.com/Azure/azure-content/master/includes/media/service-bus-create-namespace-portal/select-service-bus.png
[select-namespace]: https://raw.githubusercontent.com/Azure/azure-content/master/includes/media/service-bus-create-namespace-portal/select-namespace.png
[connection-information]: https://raw.githubusercontent.com/Azure/azure-content/master/includes/media/service-bus-create-namespace-portal/connection-information.png
[access-connection-information]: ./media/service-bus-create-namespace-portal/access-connection-information.png


<!--Reference style links - using these makes the source content way more readable than using inline links-->
[classic-portal]: https://manage.windowsazure.com

## 2. Create a queue using the Azure portal
1. Log on to the [Azure classic portal][classic-portal].

2. In the left navigation pane of the portal, click **Service Bus**.

3. Select the namespace that you would like to create the queue in. In this case, it is “mytestns1.”

    ![Selecting a namespace][select-namespace]

4. Select **Queues**.

    ![Select Queues][select-queue]

5. Select **New** in the bottom left corner, then select **Quick Create**.

    ![Select New][select-new]

6. Enter the **Queue Name** and ensure the proper namespace is selected.

7. Select **Create a new queue**.

    ![Create a queue][create-queue]

[select-namespace]: https://raw.githubusercontent.com/Azure/azure-content/master/includes/media/service-bus-create-queue-portal/select-namespace.png
[select-queue]: https://raw.githubusercontent.com/Azure/azure-content/master/includes/media/service-bus-create-queue-portal/select-queue.png
[select-new]: https://raw.githubusercontent.com/Azure/azure-content/master/includes/media/service-bus-create-queue-portal/select-new.png
[create-queue]: https://raw.githubusercontent.com/Azure/azure-content/master/includes/media/service-bus-create-queue-portal/create-queue.png

[classic-portal]: https://manage.windowsazure.com

## 3. Send messages to the queue
In order to send messages to the queue, we will write a C# console application using Visual Studio.

### Create a console application

1. Launch Visual Studio and create a new Console application.

### Add the Service Bus NuGet package

1. Right click on the newly created project and select **Manage NuGet Packages**.

2. Click the **Browse** tab, then search for “Microsoft Azure Service Bus” and select the **Microsoft Azure Service Bus** item. Click **Install** to complete the installation, then close this dialog box.

    ![Select a NuGet package][nuget-pkg]

### Write some code to send a message to the queue

1. Add the following using statement to the top of the Program.cs file.

    ```
    using Microsoft.ServiceBus.Messaging;
    ```
    
2. Add the following code to the `Main` method, set the **connectionString** variable as the connection string that was obtained when creating the namespace, and set **queueName** as the queue name that used when creating the queue.

    ```
    var connectionString = "<Your connection string>";
    var queueName = "<Your queue name>";
  
    var client = QueueClient.CreateFromConnectionString(connectionString, queueName);
    var message = new BrokeredMessage("This is a test message!");
    client.Send(message);
    ```

    Here is what your Program.cs should look like.

    ```
    using System;
    using Microsoft.ServiceBus.Messaging;

    namespace GettingStartedWithQueues
    {
        class Program
        {
            static void Main(string[] args)
            {
                var connectionString = "<Your connection string>";
                var queueName = "<Your queue name>";

                var client = QueueClient.CreateFromConnectionString(connectionString, queueName);
                var message = new BrokeredMessage("This is a test message!");

                client.Send(message);
            }
        }
    }
    ```
  
3. Run the program, and check the Azure classic portal. Notice that the **Queue Length** value should now be 1.
    
      ![Queue length][queue-length-send]


<!--Image references-->

[nuget-pkg]: https://raw.githubusercontent.com/Azure/azure-content/master/articles/service-bus/media/service-bus-dotnet-get-started-with-queues/nuget-package.png
[queue-length-send]: https://raw.githubusercontent.com/Azure/azure-content/master/articles/service-bus/media/service-bus-dotnet-get-started-with-queues/queue-length-send.png

## 4. Receive messages from the queue
1. Create a new console application and add a reference to the Service Bus NuGet package, similar to the sending application above.

2. Add the following `using` statement to the top of the Program.cs file.
  
    ```
    using Microsoft.ServiceBus.Messaging;
    ```
  
3. Add the following code to the `Main` method, set the **connectionString** variable as the connection string that was obtained when creating the namespace, and set **queueName** as the queue name that you used when creating the queue.

    ```
    var connectionString = "";
    var queueName = "samplequeue";
  
    var client = QueueClient.CreateFromConnectionString(connectionString, queueName);
  
    client.OnMessage(message =>
    {
      Console.WriteLine(String.Format("Message body: {0}", message.GetBody<String>()));
      Console.WriteLine(String.Format("Message id: {0}", message.MessageId));
    });
  
    Console.ReadLine();
    ```

    Here is what your Program.cs file should look like:

    ```
    using System;
    using Microsoft.ServiceBus.Messaging;
  
    namespace GettingStartedWithQueues
    {
      class Program
      {
        static void Main(string[] args)
        {
          var connectionString = "";
          var queueName = "samplequeue";
  
          var client = QueueClient.CreateFromConnectionString(connectionString, queueName);
  
          client.OnMessage(message =>
          {
            Console.WriteLine(String.Format("Message body: {0}", message.GetBody<String>()));
            Console.WriteLine(String.Format("Message id: {0}", message.MessageId));
          });
  
          Console.ReadLine();
        }
      }
    }
    ```
  
4. Run the program, and check the portal. Notice that the **Queue Length** value should now be 0.

    ![Queue length][queue-length-receive]
  
Congratulations! You have now created a queue, sent a message, and received a message.

<!--Image references-->

[queue-length-receive]: https://raw.githubusercontent.com/Azure/azure-content/master/articles/service-bus/media/service-bus-dotnet-get-started-with-queues/queue-length-receive.png


