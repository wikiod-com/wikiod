---
title: "Getting started with realm"
slug: "getting-started-with-realm"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Swift
Create Object Class:

    class Dog: Object {
        dynamic var name = ""
        dynamic var age = 0
    }   
Assign Object's Values:

    let dog = Dog()
    dog.name = "Rex"
    dog.age = 1
Save Object:
    
    let realm = try! Realm()
    try! realm.write {
        realm.add(dog)
    }
Reading Objects:

    let realm = try! Realm()
    let pups = realm.objects(Dog.self)
Filtering Objects:

    let realm = try! Realm()
    let filteredPups = realm.objects(Dog.self).filter("age < 2")
Counting Objects:
    
    let realm = try! Realm()
    let pupsCount = realm.objects(Dog.self).count

## React-Native
    class Dog {}
    
    Dog.schema = {
        name: 'Dog',
        properties: {
            name: 'string',
            age: 'int',
        }
    };
    
    let realm = new Realm();
    realm.write(() => {
        realm.create('Dog', {name: 'Rex', age: 1});
    });
    
    let pups = realm.objects('Dog').filtered('age > 2');

## Java
    public class Dog extends RealmObject {
        public String name;
        public int age;
    }
    
    Dog dog = new Dog();
    dog.name = "Rex";
    dog.age = 1;
    
    Realm realm = Realm.getDefaultInstance();
    realm.executeTransaction(new Realm.Transaction() {
        @Override
        public void execute(Realm realm) {
            realm.copyToRealmOrUpdate(dog);
        }
    });

    RealmResults<Dog> pups = realm.where(Dog.class)
                                   .lessThan("age", 2)
                                   .findAll();

## Objective-C
    @interface Dog : RLMObject
    @property NSString *name;
    @property NSInteger age;
    @end
    @implementation Dog
    @end
    
    Dog *dog = [Dog new];
    dog.name = @"Rex";
    dog.age = 1;
    
    RLMRealm *realm = [RLMRealm defaultRealm];
    [realm transactionWithBlock:^{
        [realm addObject:dog];
    }];
    
    RLMResults<Dog *> *allDogs = [Dog allObjects];
    RLMResults<Dog *> *pups = [allDogs objectsWhere:@"age < 2"];

## Xamarin
    public class Dog : RealmObject 
    {
        public string Name { get; set; }
        public int Age { get; set; }
    }
    
    var realm = Realm.GetInstance();
    realm.Write(() => 
    {
        var mydog = realm.CreateObject<Dog>();
        mydog.Name = "Rex";
        mydog.Age = 1;
    });
    
    var pups = realm.All<Dog>().Where(d => d.Age < 2);

