---
title: "Getting started with solid-principles"
slug: "getting-started-with-solid-principles"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Liskov Substitution Principle
----------


**Why to use LSP**

**Scenario:** Suppose we have 3 databases (Mortgage Customers, Current Accounts Customers and Savings Account Customers) that provide customer data and we need customer details for given customer's last name. Now we may get more than 1 customer detail from those 3 databases against given last name.


----------


**Implementation**





**BUSINESS MODEL LAYER:**

    public class Customer
    {
        // customer detail properties...
    }


----------


**DATA ACCESS LAYER:**

    public interface IDataAccess
    {
        Customer GetDetails(string lastName);
    }
Above interface is implemented by the abstract class

    public abstract class BaseDataAccess : IDataAccess
    {
        /// <summary> Enterprise library data block Database object. </summary>
        public Database Database;    
        public Customer GetDetails(string lastName)
        {
            // use the database object to call the stored procedure to retirve the customer detials
        }
    }

This abstract class has a common method "GetDetails" for all 3 databases which is extended by each of the database classes as shown below

**MORTGAGE CUSTOMER DATA ACCESS:**

    public class MortgageCustomerDataAccess : BaseDataAccess
    {
        public MortgageCustomerDataAccess(IDatabaseFactory factory)
        {
            this.Database = factory.GetMortgageCustomerDatabase();
        }
    }

**CURRENT ACCOUNT CUSTOMER DATA ACCESS:**

    public class CurrentAccountCustomerDataAccess : BaseDataAccess
    {
        public CurrentAccountCustomerDataAccess(IDatabaseFactory factory)
        {
            this.Database = factory.GetCurrentAccountCustomerDatabase();
        }
    }

**SAVINGS ACCOUNT CUSTOMER DATA ACCESS:**

    public class SavingsAccountCustomerDataAccess : BaseDataAccess
    {
        public SavingsAccountCustomerDataAccess(IDatabaseFactory factory)
        {
            this.Database = factory.GetSavingsAccountCustomerDatabase();
        }
    }

Once these 3 data access classes are set, now we draw our attention to the client. In the Business layer we have CustomerServiceManager class that returns the customer detials to its clients.


----------


**BUSINESS LAYER:**

    public class CustomerServiceManager : ICustomerServiceManager, BaseServiceManager
    {
       public IEnumerable<Customer> GetCustomerDetails(string lastName)
       {
            IEnumerable<IDataAccess> dataAccess = new List<IDataAccess>()
            {
                new MortgageCustomerDataAccess(new DatabaseFactory()), 
                new CurrentAccountCustomerDataAccess(new DatabaseFactory()),
                new SavingsAccountCustomerDataAccess(new DatabaseFactory())
            };
    
            IList<Customer> customers = new List<Customer>();
    
           foreach (IDataAccess nextDataAccess in dataAccess)
           {
                Customer customerDetail = nextDataAccess.GetDetails(lastName);
                customers.Add(customerDetail);
           }
    
            return customers;
       }
    }


----------


I havent shown the Dependency Injection to keep it simple as its already getting complicated now.

Now if we have a new customer detail database we can just add a new class that extends BaseDataAccess and provides its database object.

Of course we need identical stored procedures in all participating databases.

Lastly, the client for CustomerServiceManagerclass will only call GetCustomerDetails method, pass the lastName and should not care about how and where the data is coming from.

Hope this will give you a practical approach to understand LSP.


## Installation or Setup
You can use any IDE and OOP language to implement **S.O.L.I.D Principles**. In the sample code I have used C# as it is the most widely used language in .NET word and is closely resembles Java and C++.

