---
title: "Indexed DB examples"
slug: "indexed-db-examples"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

## Open a database
    function openDatabase(dbName) {
      var request = indexedDB.open(dbName);
      request.onsuccess = function (e) {
        var database = request.result;
        if (database) {
          console.log("Database initialized.");
        } else {
          console.error("Database is not initialized!");
        }
        
        //Your code goes here 
        database.close();
      }
      request.onerror = function (e) {
        console.error( e.target.error.message); 
      }
    }

## Initialize database - create table - with known db version
In order to trigger a "upgradeneeded" event you need to request the database with version higher than the current version - otherwise the event won't be triggered. 

    function createTable(dbName, dbversion, tableName) {
      var request = indexedDB.open(dbName, dbversion);
      request.onupgradeneeded = function (e) {
        var database = e.target.result;
        var objectStore = database.createObjectStore(tableName, {
            keyPath: 'id'
        });
        console.log("Object Store Created");
      };
      request.onsuccess = function (e) {
        var database = e.target.result;
        
        //code to verify that the table was created    
        database.objectStoreNames.contains(storeName);
            
        database.close();
      }
      request.onerror = function (e) {
        console.error(e.target.error.message);
      }
    }

## Initialize database - create table without knowing db version
This is a more generic solution applicable in system where the user has option to add indexes to the table that he uses:

    function createTable(dbName, tableName) {
      var request = indexedDB.open(dbName);
      request.onsuccess = function (e){
        var database = e.target.result;
        var version =  parseInt(database.version);
        database.close();
        var secondRequest = indexedDB.open(dbName, version+1);
        secondRequest.onupgradeneeded = function (e) {
            var database = e.target.result;
            var objectStore = database.createObjectStore(storeName, {
                keyPath: 'id'
            });
        };
        secondRequest.onsuccess = function (e) {
            e.target.result.close();
        }
      }
    }

