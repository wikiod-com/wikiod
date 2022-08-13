---
title: "Facade"
slug: "facade"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

## Facade example in java
**Facade** is structural design pattern. It hides the complexities of large system and provides a simple interface to client. 

**Client** uses only **Facade** and it's not worried about inter dependencies of sub-systems.

Definition from Gang of Four book:

> Provide a unified interface to a set of interfaces in a subsystem. Façade defines a higher-level interface that makes the subsystem easier to use

Structure:

[![enter image description here][1]][1]

*Real world example:*

Think of some travel booking sites like makemytrip, cleartrip which offers services to book Trains, Flights and Hotels.

Code snippet:

    import java.util.*;
    
    public class TravelFacade{
        FlightBooking flightBooking;
        TrainBooking trainBooking;
        HotelBooking hotelBooking;
    
        enum BookingType {
            Flight,Train,Hotel,Flight_And_Hotel,Train_And_Hotel;
        }; 
        
        public TravelFacade(){
            flightBooking = new FlightBooking();
            trainBooking = new TrainBooking();
            hotelBooking = new HotelBooking();        
        }
        public void book(BookingType type, BookingInfo info){
            switch(type){
                case Flight:
                    // book flight;
                    flightBooking.bookFlight(info);
                    return;
                case Hotel:
                    // book hotel;
                    hotelBooking.bookHotel(info);
                    return;
                case Train:
                    // book Train;
                    trainBooking.bookTrain(info);
                    return;
                case Flight_And_Hotel:
                    // book Flight and Hotel
                    flightBooking.bookFlight(info);
                    hotelBooking.bookHotel(info);
                    return;
                 case Train_And_Hotel:
                    // book Train and Hotel
                    trainBooking.bookTrain(info);
                    hotelBooking.bookHotel(info);
                    return;                
            }
        }
    }
    class BookingInfo{
        String source;
        String destination;
        Date    fromDate;
        Date     toDate;
        List<PersonInfo> list;
    }
    class PersonInfo{
        String name;
        int       age;
        Address address;
    }
    class Address{
    
    }
    class FlightBooking{
        public FlightBooking(){
        
        }
        public void bookFlight(BookingInfo info){
        
        }
    }
    class HotelBooking{
        public HotelBooking(){
        
        }
        public void bookHotel(BookingInfo info){
        
        }
    }
    class TrainBooking{
        public TrainBooking(){
        
        }
        public void bookTrain(BookingInfo info){
        
        }
    }


Explanation:

1. `FlightBooking, TrainBooking and HotelBooking` are different sub-systems of large system : `TravelFacade`

2. `TravelFacade` offers a simple interface to book one of below options

       Flight Booking
       Train Booking 
       Hotel Booking
       Flight + Hotel booking 
       Train + Hotel booking

3. book API from TravelFacade internally calls below APIs of sub-systems

       flightBooking.bookFlight
       trainBooking.bookTrain(info);
       hotelBooking.bookHotel(info);
    
4. In this way, `TravelFacade` provides simpler and easier API with-out exposing sub-system APIs.


Applicability and Use cases (from Wikipedia) :

1. A simple interface is required to access a complex system.
2. The abstractions and implementations of a subsystem are tightly coupled.
3. Need an entry point to each level of layered software.
4. System is very complex or difficult to understand.      
    


  [1]: https://i.stack.imgur.com/8bX5K.png

## Real world facade (C#)
    public class MyDataExporterToExcell
    {
        public static void Main()
        {
            GetAndExportExcelFacade facade = new GetAndExportExcelFacade();
    
            facade.Execute();
        }
    }
    
    public class GetAndExportExcelFacade
    {

        // All services below do something by themselves, determine location for data,
        // get the data, format the data, and export the data
        private readonly DetermineExportDatabaseService _determineExportData = new DetermineExportDatabaseService();
        private readonly GetRawDataToExportFromDbService _getRawData = new GetRawDataToExportFromDbService();
        private readonly TransformRawDataForExcelService _transformData = new TransformRawDataForExcelService();
        private readonly CreateExcelExportService _createExcel = new CreateExcelExportService();

        // the facade puts all the individual pieces together, as its single responsibility.
        public void Execute()
        {
            var dataLocationForExport = _determineExportData.GetDataLocation();
            var rawData = _getRawData.GetDataFromDb(dataLocationForExport);
            var transformedData = _transformData.TransformRawToExportableObject(rawData);
            _createExcel.GenerateExcel("myFilename.xlsx");
        }
    }

