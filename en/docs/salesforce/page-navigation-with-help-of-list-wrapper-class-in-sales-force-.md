---
title: "Page Navigation with help of list wrapper class in sales force."
slug: "page-navigation-with-help-of-list-wrapper-class-in-sales-force"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Slaesforce StandardSetController hold only List of sObject, It will not holdslist of wrapper class object. Below source code demonstrates the usage of Paginating using Wrapper Class in sales-force.

## Pagination Controller
Code example :
Now Start with Creating the Pagination Controller

    public with sharing class Pagination {
    
    }

i am going to show all the contacts in the form of pagination and there should be one checkbox for each contact to selected or deselect contact to perform delete operation on contacts.
So i need to be create a wrapper class to hold contact and Boolean Variable for selection.
First create Wrapper class for controller Pagination
Insert below code into pagination controller.

     public class contactWrapper{
            public Contact cont {get;set;}
            public Boolean isSelected{get;set;}
            public contactWrapper(contact c,Boolean s)
            {
                cont=c;
                isSelected=s;
            }
        }

Now Retrieving Data in Apex to Paginate with the help of StandardSetController. The StandardSetController is an extremely powerful tool with built-in functionality that you can use to greatly simplify the custom code in your Visualforce pages. Because the server returns only the data for the page being requested, the StandardSetController can significantly reduce view state, especially compared to the view state you would get while using SOQL.
   

     Public Integer noOfRecords{get; set;} // Future reference in Visual force Page
        Public Integer size{get;set;}  // Future reference in Visual force Page
        public final Integer Page_Size=10; // Number records in a Page should be displayed 
    
     public ApexPages.StandardSetController setCon {
            get{
                if(setCon == null){
                    size=Page_Size;
                    string queryString = 'Select Id,Name, Email, Birthdate, Phone, MobilePhone from Contact order by Name';
                    setCon = new ApexPages.StandardSetController(Database.getQueryLocator(queryString));
                    setCon.setPageSize(size);
                    noOfRecords = setCon.getResultSize();
                }
                return setCon;
            }set;
        }

Now you have the contacts in the Variable setCon , whenever your requested for setCon.getRecords() it will retrieve the first 10 contact records from the setCon.
Here i have create simple wrapper class to show you the demo. You can create your own wrapper class based on requirement. But always you must be aware that ApexPages.StandardSetController hold only List of sObject, it will not hold the list of wrapper class object . That's the reason i have written extra code below to accomplish this future in different way.
Below code convert list of contacts into list of wrapper class objects, So that when ever you call contacts in visual force page it will receive the list of wrapper class object.
   

     public list<contactWrapper> contWpr{get;set;} 
        public set<id> selectedContactIds{ get;private set;} // to maintain state of the selected contact
                                                             // through out paginating 
    
        public Pagination() {
           selectedContactIds=new  set<id>();
        }
    
          Public list<contactWrapper> getContacts(){
             contWpr =new list<contactWrapper>();
            for(Contact c: (List<Contact>)setCon.getRecords())
                if(selectedContactIds.contains(c.id))
                    contWpr.add(new contactWrapper(c,true));
                    else
                    contWpr.add(new contactWrapper(c,false));
            return contWpr;
        }

Now you have written code for generating result But how to navigate across the page?
This can be done with easy step with ApexPages.StandardSetController.Look at the below code beauty of the StandardSetController, No need to maintain page number,offset and limit etc.. Just use the StandardSetController methods. Copy the below code into Pagination controller.
 

    public Boolean hasNext {
            get {
                return setCon.getHasNext();
            }
            set;
        }
        public Boolean hasPrevious {
            get {
                return setCon.getHasPrevious();
            }
            set;
        }
    
        public Integer pageNumber {
            get {
                return setCon.getPageNumber();
            }
            set;
        }
    
        public void first() {
            setCon.first();
            // do you operation here 
        }
    
        public void last() {
            setCon.last();
                // do you operation here 
        }
    
        public void previous() {
            setCon.previous();
                // do you operation here 
        }
    
        public void next() {
            setCon.next();
                // do you operation here 
        }

Your almost done with Paginating the contacts. Last few methods i have added to fulfill my entire page functionality. As i mention earlier we have additional checkbox for selecting contact and perform delete operation on selected contacts.
 

    public void contactSelection()
        {
            Id id=(Id)ApexPages.currentPage().getParameters().get('cId');
            if(selectedContactIds.contains(id))
                selectedContactIds.remove(id);
                else
                selectedContactIds.add(id);
        }
    
     public void deleteContacts()
        {
            List<contact> contactToDelete=[select id from contact where id in :selectedContactIds];
            if(contactToDelete.size()!=0)  //   if(!contactToDelete.isEmpty()) // Best Practice 
               { 
                    try {  delete contactToDelete; }  // You may get Exception if you try to delete the 
                                                    // related contact ,include try block to avoid error.
                    catch(exception ex){ System.debug(ex); }
                    refresh();
               }
        }   
    
     public pageReference refresh() {
            setCon = null;
            selectedContactIds=new set<id>();
            getContacts();
            setCon.setPageNumber(1);
            return null;
        }

