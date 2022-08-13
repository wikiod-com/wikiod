---
title: "Unit testing with datastore"
slug: "unit-testing-with-datastore"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Create a context with a strongly consistent data store.
When testing with Google App Engine's testing library the challenges of eventual consistency are present in the same manner they will be in production.  Therefore in order to write something into the datastore to test retrieval you have to create a context which is strongly consistent.

    type Foo struct {
        Bar string
    }

    func TestDataStore(t *testing.T) {
        inst, err := aetest.NewInstance(
            &aetest.Options{StronglyConsistentDatastore: true})
        if err != nil {
            t.Fatal(err)
        }
        defer inst.Close()
    
        req, err := inst.NewRequest("GET", "/", nil)
        if err != nil {
            t.Fatal(err)
        }
    
        ctx := appengine.NewContext(req)

        foo := &Foo{ Bar: "baz" } 
        
        key, err := key := datastore.NewIncompleteKey(context, "Foo", nil)
        if _, err := datastore.Put(context, key, details); err != nil {
            t.Fatalf(err)
        }

        query := datastore.NewQuery("Foo").Filter("Bar =", "baz")
        for iterator := query.Run(ctx); ; {
            item := &Foo{}
            err := iterator.Next(item)
            if err == datastore.Done {
                t.Fatalf("No results")
            } 
            if err != nil {
                t.Fatal(err)
            } 
            if foo.Bar != item.Bar {
                t.Fatal("Wrong result returned.")
            }
        }       
    }

