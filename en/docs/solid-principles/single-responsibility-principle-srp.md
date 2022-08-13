---
title: "Single Responsibility Principle (SRP)"
slug: "single-responsibility-principle-srp"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

There should never be more than one reason for change anything in software entities (class, function, file etc). A class, function, file etc should have only one reason to change.

Just because you can, doesn't mean you should.

## Single Responsibility Principle C#
Let's go through the problem first. Have a look on the code below:

    public class BankAccount
    {             
        public BankAccount()  {}
    
        public string AccountNumber { get; set; }
        public decimal AccountBalance { get; set; }
    
        public decimal CalculateInterest()
        {
            // Code to calculate Interest
        }
    }

Here, **BankAccount** class contains the properties of account and also *calculate the interest* of account. Now look at the few change Request we received from business:
1. Please add a new Property *AccountHolderName* .
2. Some new rule has been introduced to calculate interest.

This are totally different type of change request. One is changing on features; where as other one is impacting the functionality. We have 2 different types of reason to change one class. This violates Single Responsibility Principle.

Now let's try to implement **SRP** to resolved this violation. Look at the code below: 

    public interface IBankAccount
    {
        string AccountNumber { get; set; }
        decimal AccountBalance { get; set; }
    }
    
    public interface IInterstCalculator
    {
        decimal CalculateInterest();
    }
    
    public class BankAccount : IBankAccount
    {
        public string AccountNumber { get; set; }
        public decimal AccountBalance { get; set; }
    }
    
    public class InterstCalculator : IInterstCalculator
    {
        public decimal CalculateInterest(IBankAccount account)
        {
            // Write your logic here
            return 1000;
        }
    }
Now our **BankAccount** class is just responsible for properties of the bank account. If we want to add any new *business rule* for the *Calculation of Interest*, we don't need to change **BankAccount** class.

And also **InterestCalculator** class requires no changes, in case we need to add a new Property *AccountHolderName*.  So this is the *implementation of Single Responsibility Principle*.

We have also used Interfaces to communicate between *InterestCalculator and BankAccount* class. This will help us to manage dependencies between classes.


## Single Responsibility Principle
**Introduction** 

SRP can be defined as “a class handles only one responsibility”. This is a very short definition for something influential on to the other principles of S.O.L.I.D. I believe that if we get this right, it will have a positive knock-on effect on the upcoming principles, so let’s get started! 
Practical Example
Let’s say we have an online store where people can order some items or products and in the code base we have a class OrderProcessor that processes the new orders when people click on Pay Now button.

The reason `OrderProcessor` is written is to carry out the following tasks, in other words `OrderProcessor` class has the following responsibilities:

1.    Check the credit card has been accepted – finance
2.    Check the money has been charged – finance 
3.    Check the item is in stock – inventory 
4.    Request the item for reservation
5.    Get the estimated delivery time
6.    Email the confirmation to the customer    

Here is the definition of `OrderProcessor` class 

    public class OrderProcessor
    {
        public bool ProcessOrder(Order orderToProcess)
        {
            // 1)    Check the credit card has been accepted.
            int creditCardId = orderToProcess.CreditCardId;
    
            CreditCard creditCardDetails = new CreditCard();
    
            using (SqlConnection connect = new SqlConnection())
            {
                using (SqlCommand command = new SqlCommand())
                {
                    command.Connection = connect;
                    command.CommandText = "<schema>.<spName>";
                    command.CommandType = CommandType.StoredProcedure;
                    SqlParameter idParam = new SqlParameter();
                    idParam.Direction = ParameterDirection.Input;
                    idParam.Value = creditCardId;
                    idParam.ParameterName = "@Id";
                    idParam.DbType = DbType.Int32;
                    command.Parameters.Add(idParam);
                    using (SqlDataReader reader = command.ExecuteReader())
                    {
                        while (reader.Read())
                        {
                            creditCardDetails.CardId = int.Parse(reader["Id"].ToString());
                            creditCardDetails.CardLongNumber = reader["CardLongNumber"].ToString();
                            creditCardDetails.CvcNumber = int.Parse(reader["CvcNumber"].ToString());
                            creditCardDetails.ExpiryDate = DateTime.Parse(reader["ExpiryDate"].ToString());
                            creditCardDetails.NameOnTheCard = reader["NameOnTheCard"].ToString();
                            creditCardDetails.StartDate = DateTime.Parse(reader["StartDate"].ToString());
                        }
                    }
    
                }
            }
    
            // charge the total amount using the credit card details..
    
            decimal amountToCharge = orderToProcess.OrderTotal;
            using (WebClient webClient = new WebClient())
            {
                string response = webClient.DownloadString($"https://CreditCardProcessor/api/ProcessesPayments?amount={amountToCharge}&CreditCard={creditCardDetails}");
                // processes response: check if its been successful or failure then proceed further....
            }
    
    
    
            // check the item is in the stock
    
    
            Dictionary<int, bool> productAvailability = new Dictionary<int, bool>();
    
            foreach (int productId in orderToProcess.ProductIds)
            {
                using (SqlConnection connection = new SqlConnection())
                {
                    using (SqlCommand command = new SqlCommand())
                    {
                        command.Connection = connection;
                        command.CommandText = "<schema>.<spName>";
                        command.CommandType = CommandType.StoredProcedure;
                        SqlParameter idParam = new SqlParameter();
                        idParam.Direction = ParameterDirection.Input;
                        idParam.Value = productId;
                        idParam.ParameterName = "@Id";
                        idParam.DbType = DbType.Int32;
                        command.Parameters.Add(idParam);
    
                        object resultObject = command.ExecuteScalar();
                        bool prductAvailable = bool.Parse(resultObject.ToString());
                        if (prductAvailable)
                            productAvailability.Add(productId, true);
                    }
                }
            }
    
            // request item for reservation
    
            ReservationServiceClientProxy client = new ReservationServiceClientProxy();
    
            foreach (KeyValuePair<int, bool> nextProduct in productAvailability)
            {
                ReservationRequest requst = new ReservationRequest() { ProductId = nextProduct.Key };
                ReservationResponse response = client.ReserveProduct(requst);
            }
    
            // calculate estimated time of delivery...
            DeliveryService ds = new DeliveryService();
            int totalMinutes = 0;
            foreach (KeyValuePair<int, bool> nextProduct in productAvailability)
            {
                totalMinutes += ds.EstimateDeliveryTimeInMinutes(nextProduct.Key);
            }
    
    
            // email customer
    
    
            int customerId = orderToProcess.CustomerId;
            string customerEmail = string.Empty;
    
            using (SqlConnection connection = new SqlConnection())
            {
                using (SqlCommand command = new SqlCommand())
                {
                    command.Connection = connection;
                    command.CommandText = "<schema>.<spName>";
                    command.CommandType = CommandType.StoredProcedure;
                    SqlParameter idParam = new SqlParameter();
                    idParam.Direction = ParameterDirection.Input;
                    idParam.Value = customerId;
                    idParam.ParameterName = "@customerId";
                    idParam.DbType = DbType.Int32;
                    command.Parameters.Add(idParam);
    
                    object resultObject = command.ExecuteScalar();
                    customerEmail = resultObject.ToString();
                }
            }
    
            MailMessage message = new MailMessage(new MailAddress("Some.One@SuperCheapStore.co.uk"), new MailAddress(customerEmail));
            message.Body = $"You item has been dispatched and will be delivered in {totalMinutes / 1440} days";
            message.Subject = "Your order update!";
    
            SmtpClient smtpClient = new SmtpClient("HostName/IPAddress");
            smtpClient.Send(message);
    
            return true;
    
        }
    }


As we can see in the class definition that `OrderProcessor` has taken more than one responsibility. Lets turn our attention to the version 2 of `OrderProcesser` class that is written by keeping SRP in mind.

    public class OrderProcessorV2
    {
        public bool ProcessOrder(Order orderToProcess)
        {
            // 1)    Check the credit card has been accepted.
            CreditCardDataAccess creditCardAccess = new CreditCardDataAccess();
            CreditCard cardDetails = creditCardAccess.GetCreditCardDetails(orderToProcess.CreditCardId);
    
            // 2)    Check the money has been charged – finance
    
            PaymentProcessor paymentProcessor = new PaymentProcessor();
            paymentProcessor.DebitAmount(orderToProcess.OrderTotal, cardDetails);
    
            // 3)    Check the item is in stock – inventory 
    
            InventoryService inventory = new InventoryService();
    
            Dictionary<int, bool> productAvailability = inventory.CheckStockAvailability(orderToProcess.ProductIds);
    
            foreach (int nextProductId in orderToProcess.ProductIds)
            {
                inventory.CheckStockAvailability(nextProductId);
            }
    
    
            // 4)    Request the item for reservation
            ReservationService reservation = new ReservationService();
    
            foreach (int nextProductId in orderToProcess.ProductIds)
            {
                reservation.ReserveProduct(nextProductId);
            }
    
    
            // 5)    Get the estimated delivery time
            // calculate estimated time of delivery...
            DeliveryService ds = new DeliveryService();
            int totalMinutes = 0;
            foreach (KeyValuePair<int, bool> nextProduct in productAvailability)
            {
                totalMinutes += ds.EstimateDeliveryTimeInMinutes(nextProduct.Key);
            }
    
    
            // 6)    Email the confirmation to the customer
    
            CustomerDataAccess customerDataAccess = new CustomerDataAccess();
    
            Customer cust = customerDataAccess.GetCustomerDetails(orderToProcess.CustomerId);
    
            EmailService mailService = new EmailService();
            mailService.NotifyCustomer(cust.Email);
    
            // if everything step is successful then return true..
    
        }
    }

The amount of code lines in `OrderProcessorV2` has radically changed so does the readability. The overall responsibility of `OrderProcessorV2` can be understood within the amount of time that has taken to read this line. This results in more productivity. 

