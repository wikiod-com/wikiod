---
title: "Spies"
slug: "spies"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

A spy is defined as a test specific function which intercepts calls to an underlying function in the application code and dispatches its own implementation when the underlying function is called to test the interface rather than the implementation.

## Spying on an existing function
Jasmine can spy on an existing function using the `spyOn` function.

    let calculator = {
      multiply: function(a, b) {
         return a * b;
      },

      square: function(a) {
        return this.multiply(a, a);
      }
    }


    describe('calculator', function() {
      it('squares numbers by multiplying them by themselves', function() {
        let num = 2;
        spyOn(calculator, 'multiply');
        calculator.square(NUM);
        expect(calculator.multiply).toHaveBeenCalledWith(NUM, NUM);
      })
    });

After the function has been spied on it is replaced with a spy, that can be queried for information about how and when it has been called.

## Creating a new spy
We can use `jasmine.createSpy()` to create a standalone spy.  This is often useful if we need to pass a function as a callback to another function and want to test how it is used.

    // source code
    function each(arr, fn) {
        arr.forEach(fn);
    }

    // test code
    describe('each', function() {
      let mockFn = jasmine.createSpy();

      it('calls a function for each item in the array ', function() {
        let arr = [1,2,3,4,5]
        each(arr, mockFn);
        expect(mockFn.calls.count()).toBe(arr.length);
      })
    });

    

## Spying on an angular service
In this example we have a service, let's call it search service that has a method called search() which will initiate a get request to a back end API.

    function SearchService($http) {
        const service = {};
        
        service.search = function() {
            return $http({method: 'GET', url: `/api/search`})
        }        

        return service;
    }
    angular.module('app').factory('searchService', SearchService);

Testing 

    describe('search service', function() {
        var $httpBackend;
        var searchService;
        beforeEach(angular.mock.module('app'));
        
        beforeEach(inject(function(_$httpBackend_, _searchService_) {
            $httpBackend = _$httpBackend_;            
            searchService = _searchService_;
        }));
    
        it('should perform http call to the search api', function(){
            searchService.search();
            $httpBackend.expectGET('/api/search');
        });

    })


## Spying on an angular service that doesn't call back end service
    function calculatorService() {
        const service = {};
        service.add = function(a,b) {
            return a + b
        }        

        return service;
    }

    angular.module('app').factory('calculatorService', calculatorService);

Testing 

    describe('calculator service', function() {
        var calculatorService;
        beforeEach(angular.mock.module('app'));
        
        beforeEach(inject(function(_calculatorService_) {
             calculatorService = _calculatorService_;
        }));
    
        it('should should add two numbers', function(){
            var actual = calculatorService.add(1,2);
            expect(actual).toBe(3);
        });
    })


## Spying on a property
    const foop = {
        get value() {},
        set value(v) {}
    };
    
    it('can spy on getter', () => {
        spyOnProperty(foop, 'value', 'get').and.returnValue(1);
        expect(foop.value).toBe(1);
    });
    
    it('and on setters', () => {
        const spiez = spyOnProperty(foop, 'value', 'set');
        foop.value = true;
        expect(spiez).toHaveBeenCalled();
    });

