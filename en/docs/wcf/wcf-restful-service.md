---
title: "WCF Restful Service"
slug: "wcf-restful-service"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## WCF Resful Service
    [ServiceContract]
    public interface IBookService
    {
        [OperationContract]
        [WebGet]
        List<Book> GetBooksList();
    
        [OperationContract]
        [WebGet(UriTemplate  = "Book/{id}")]
        Book GetBookById(string id);
    
        [OperationContract]
        [WebInvoke(UriTemplate = "AddBook/{name}")]
        void AddBook(string name);
    
        [OperationContract]
        [WebInvoke(UriTemplate = "UpdateBook/{id}/{name}")]
        void UpdateBook(string id, string name);
    
        [OperationContract]
        [WebInvoke(UriTemplate = "DeleteBook/{id}")]
        void DeleteBook(string id);
    }

**Implementing the Service**  

Now the service implementation part will use the entity framework generated context and entities to perform all the respective operations.

    public class BookService : IBookService
    {
        public List<Book> GetBooksList()
        {
            using (SampleDbEntities entities = new SampleDbEntities())
            {
                return entities.Books.ToList();
            }
        }
    
        public Book GetBookById(string id)
        {
            try
            {
                int bookId = Convert.ToInt32(id);
    
                using (SampleDbEntities entities = new SampleDbEntities())
                {
                    return entities.Books.SingleOrDefault(book => book.ID == bookId);
                }
            }
            catch
            {
                throw new FaultException("Something went wrong");
            }
        }
    
        public void AddBook(string name)
        {
            using (SampleDbEntities entities = new SampleDbEntities())
            {
                Book book = new Book { BookName = name };
                entities.Books.AddObject(book);
                entities.SaveChanges();
            }
        }
    
        public void UpdateBook(string id, string name)
        {
            try
            {
                int bookId = Convert.ToInt32(id);
    
                using (SampleDbEntities entities = new SampleDbEntities())
                {
                    Book book = entities.Books.SingleOrDefault(b => b.ID == bookId);
                    book.BookName = name;
                    entities.SaveChanges();
                }
            }
            catch
            {
                throw new FaultException("Something went wrong");
            }
        }
    
        public void DeleteBook(string id)
        {
            try
            {
                int bookId = Convert.ToInt32(id);
    
                using (SampleDbEntities entities = new SampleDbEntities())
                {
                    Book book = entities.Books.SingleOrDefault(b => b.ID == bookId);
                    entities.Books.DeleteObject(book);
                    entities.SaveChanges();
                }
            }
            catch
            {
                throw new FaultException("Something went wrong");
            }
        }
    }

**Restful WCF service Configuration**

Now from the ServiceContract perspective the service is ready to serve the REST request but to access this service over rest we need to do some changes in the service behavior and binding too.

To make the service available over REST protocol the binding that needs to be used is the webHttpBinding. Also, we need to set the endpoint's behavior configuration and define the webHttp parameter in the endpointBehavior. So our resulting configuration will look something like:

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/pdTTD.jpg

