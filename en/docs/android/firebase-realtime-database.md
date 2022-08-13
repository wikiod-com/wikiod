---
title: "Firebase Realtime DataBase"
slug: "firebase-realtime-database"
draft: false
images: []
weight: 9884
type: docs
toc: true
---

Other related topics:
-----------------------------------------
- [Firebase][1] 


  [1]: https://www.wikiod.com/android/firebase

## Quick setup
1. Complete the [Installation and setup part][1] to connect your app to Firebase.  
This will create the project in Firebase.

2. Add the dependency for Firebase Realtime Database to your module-level `build.gradle` file:  


    compile 'com.google.firebase:firebase-database:10.2.1'

3. Configure [Firebase Database Rules][2]

Now you are ready to work with the Realtime Database in Android.

For example you write a `Hello World` message to the database under the `message` key.

    // Write a message to the database
    FirebaseDatabase database = FirebaseDatabase.getInstance();
    DatabaseReference myRef = database.getReference("message");
    
    myRef.setValue("Hello, World!");


  [1]: https://www.wikiod.com/android/firebase#Add Firebase to Your Android Project
  [2]: https://www.wikiod.com/firebase/database-rules

##  Firebase Realtime DataBase event handler
First Initialize FirebaseDatabase:

    FirebaseDatabase database = FirebaseDatabase.getInstance();

Write to your database: 

    // Write a message to the database
    FirebaseDatabase database = FirebaseDatabase.getInstance();
    DatabaseReference myRef = database.getReference("message");
    
    myRef.setValue("Hello, World!");

Read from your database:

    // Read from the database
    myRef.addValueEventListener(new ValueEventListener() {
        @Override
        public void onDataChange(DataSnapshot dataSnapshot) {
            // This method is called once with the initial value and again
            // whenever data at this location is updated.
            String value = dataSnapshot.getValue(String.class);
            Log.d(TAG, "Value is: " + value);
        }
    
        @Override
        public void onCancelled(DatabaseError error) {
            // Failed to read value
            Log.w(TAG, "Failed to read value.", error.toException());
        }
    });

Retrieve Data on Android events:

    ChildEventListener childEventListener = new ChildEventListener() {
        @Override
        public void onChildAdded(DataSnapshot dataSnapshot, String previousChildName) {
            Log.d(TAG, "onChildAdded:" + dataSnapshot.getKey());
        }
    
        @Override
        public void onChildChanged(DataSnapshot dataSnapshot, String previousChildName) {
            Log.d(TAG, "onChildChanged:" + dataSnapshot.getKey());
        }
    
        @Override
        public void onChildRemoved(DataSnapshot dataSnapshot) {
            Log.d(TAG, "onChildRemoved:" + dataSnapshot.getKey());
    
        }
    
        @Override
        public void onChildMoved(DataSnapshot dataSnapshot, String previousChildName) {
            Log.d(TAG, "onChildMoved:" + dataSnapshot.getKey());
    
        }
    
        @Override
        public void onCancelled(DatabaseError databaseError) {
            Log.w(TAG, "postComments:onCancelled", databaseError.toException());
            Toast.makeText(mContext, "Failed to load comments.",
                    Toast.LENGTH_SHORT).show();
        }
    };
    ref.addChildEventListener(childEventListener);

## Designing and understanding how to retrieve realtime data from the Firebase Database
This example assumes that you have already set up a Firebase Realtime Database. If you are a starter, then please inform yourself [here][1] on how to add Firebase to your Android project.
 
First, add the dependency of the Firebase Database to the app level _build.gradle_ file:
    
    compile 'com.google.firebase:firebase-database:9.4.0'

Now, let us create a chat app which stores data into the Firebase Database.

# Step 1: Create a class named Chat

Just create a class with some basic variables required for the chat:

    public class Chat{
        public String name, message;
    }

# Step 2: Create some JSON data

For sending/retrieving data to/from the Firebase Database, you need to use JSON. Let us assume that some chats are already stored at the root level in the database. The data of these chats could look like as follows:

    [
        {
            "name":"John Doe",
            "message":"My first Message"
        },
        {
            "name":"John Doe",
            "message":"Second Message"
        },
        {
            "name":"John Doe",
            "message":"Third Message"
        }
    ]
  
# Step 3: Adding the listeners

There are three types of listeners. In the following example we are going to use the `childEventListener`:
    
    DatabaseReference chatDb = FirebaseDatabase.getInstance().getReference() // Referencing the root of the database.
            .child("chats"); // Referencing the "chats" node under the root.

    chatDb.addChildEventListener(new ChildEventListener() {
        @Override
        public void onChildAdded(DataSnapshot dataSnapshot, String s) {
            // This function is called for every child id chat in this case, so using the above
            // example, this function is going to be called 3 times.
            
            // Retrieving the Chat object from this function is simple.
            Chat chat; // Create a null chat object.

            // Use the getValue function in the dataSnapshot and pass the object's class name to
            // which you want to convert and get data. In this case it is Chat.class.
            chat = dataSnapshot.getValue(Chat.class);

            // Now you can use this chat object and add it into an ArrayList or something like
            // that and show it in the recycler view.
        }

        @Override
        public void onChildChanged(DataSnapshot dataSnapshot, String s) {
            // This function is called when any of the node value is changed, dataSnapshot will
            // get the data with the key of the child, so you can swap the new value with the
            // old one in the ArrayList or something like that.

            // To get the key, use the .getKey() function.
            // To get the value, use code similar to the above one.
        }

        @Override
        public void onChildRemoved(DataSnapshot dataSnapshot) {
            // This function is called when any of the child node is removed. dataSnapshot will
            // get the data with the key of the child.

            // To get the key, use the s String parameter .
        }

        @Override
        public void onChildMoved(DataSnapshot dataSnapshot, String s) {
            // This function is called when any of the child nodes is moved to a different position.

            // To get the key, use the s String parameter.
        }

        @Override
        public void onCancelled(DatabaseError databaseError) {
            // If anything goes wrong, this function is going to be called.

            // You can get the exception by using databaseError.toException();
        }
    });

# Step 4: Add data to the database

Just create a Chat class object and add the values as follows:

    Chat chat=new Chat();
    chat.name="John Doe";
    chat.message="First message from android";

Now get a reference to the chats node as done in the retrieving session:

    DatabaseReference chatDb = FirebaseDatabase.getInstance().getReference().child("chats");

Before you start adding data, keep in mind that you need one more deep reference since a chat node has several more nodes and adding a new chat means adding a new node containing the chat details. We can generate a new and unique name of the node using the `push()` function on the `DatabaseReference` object, which will return another `DatabaseReference`, which in turn points to a newly formed node to insert the chat data.

# Example

    // The parameter is the chat object that was newly created a few lines above.
    chatDb.push().setValue(chat);

The `setValue()` function will make sure that all of the application's `onDataChanged` functions are getting called (including the same device), which happens to be the attached listener of the "chats" node. 


  [1]: https://firebase.google.com/docs/android/setup

## Denormalization: Flat Database Structure
Denormalization and a flat database structure is neccessary to efficiently download separate calls. With the following structure, it is also possible to maintain two-way relationships. The disadvantage of this approach is, that you always need to update the data in multiple places.

For an example, imagine an app which allows the user to store messages to himself (memos). 

Desired flat database structure:

    |--database
      |-- memos
         |-- memokey1
           |-- title: "Title"
           |-- content: "Message"
         |-- memokey2
           |-- title: "Important Title"
           |-- content: "Important Message"
      |-- users
         |-- userKey1
           |-- name: "John Doe"
           |-- memos
             |-- memokey1 : true //The values here don't matter, we only need the keys.
             |-- memokey2 : true
         |-- userKey2
           |-- name: "Max Doe"

**The used memo class**

    public class Memo {
        private String title, content;
        //getters and setters ... 

        //toMap() is necessary for the push process
        private Map<String, Object> toMap() {
            HashMap<String, Object> result = new HashMap<>();
            result.put("title", title);
            result.put("content", content);
            return result;
        }
    }

**Retrieving the memos of a user**

    //We need to store the keys and the memos seperately
    private ArrayList<String> mKeys = new ArrayList<>();
    private ArrayList<Memo> mMemos = new ArrayList<>();

    //The user needs to be logged in to retrieve the uid
    String currentUserId = FirebaseAuth.getInstance().getCurrentUser().getUid();

    //This is the reference to the list of memos a user has 
    DatabaseReference currentUserMemoReference = FirebaseDatabase.getInstance().getReference()
        .child("users").child(currentUserId).child("memos");

    //This is a reference to the list of all memos
    DatabaseReference memoReference = FirebaseDatabase.getInstance().getReference()
        .child("memos");

    //We start to listen to the users memos, 
    //this will also retrieve the memos initially
    currentUserMemoReference.addChildEventListener(new ChildEventListener() {
            @Override
            public void onChildAdded(DataSnapshot dataSnapshot, String s) {
                //Here we retrieve the key of the memo the user has.
                String key = dataSnapshot.getKey(); //for example memokey1
                //For later manipulations of the lists, we need to store the key in a list
                mKeys.add(key);
                //Now that we know which message belongs to the user, 
                //we request it from our memos:
                memoReference.child(key).addValueEventListener(new ValueEventListener() {
                    @Override
                        public void onDataChange(DataSnapshot dataSnapshot) {
                             //Here we retrieve our memo:
                             Memo memo = dataSnapshot.getValue(Memo.class);
                             mMemos.add(memo);
                        }

                    @Override
                    public void onCancelled(DatabaseError databaseError) { }
                });                           
            }

            @Override
            public void onChildChanged(DataSnapshot dataSnapshot, String s) { }

            @Override
            public void onChildRemoved(DataSnapshot dataSnapshot) { }

            @Override
            public void onChildMoved(DataSnapshot dataSnapshot, String s) { }

            @Override
            public void onCancelled(DatabaseError databaseError) { }
        }

**Creating a memo**


    //The user needs to be logged in to retrieve the uid
    String currentUserUid = FirebaseAuth.getInstance().getCurrentUser().getUid();

    //This is the path to the list of memos a user has 
    String userMemoPath = "users/" + currentUserUid + "/memos/";

    //This is the path to the list of all memos
    String memoPath = "memos/";

    //We need to retrieve an unused key from the memos reference
    DatabaseReference memoReference = FirebaseDatabase.getInstance().getReference().child("memos");
    String key = memoReference.push().getKey();
    Memo newMemo = new Memo("Important numbers", "1337, 42, 3.14159265359");

    Map<String, Object> childUpdates = new HashMap<>(); 
    //The second parameter **here** (the value) does not matter, it's just that the key exists
    childUpdates.put(userMemoPath + key, true);
    childUpdates.put(memoPath + key, newMemo.toMap());
 
    FirebaseDatabase.getInstance().getReference().updateChildren(childUpdates);
    
After the push, or database looks like this:

    |--database
      |-- memos
         |-- memokey1
           |-- title: "Title"
           |-- content: "Message"
         |-- memokey2
           |-- title: "Important Title"
           |-- content: "Important Message"
         |-- generatedMemokey3 
           |-- title: "Important numbers"
           |-- content: "1337, 42, 3.14159265359"
      |-- users
         |-- userKey1
           |-- name: "John Doe"
           |-- memos
             |-- memokey1 : true //The values here don't matter, we only need the keys.
             |-- memokey2 : true
             |-- generatedMemokey3 : true
         |-- userKey2
           |-- name: "Max Doe"
    
   


     
      

## Understanding firebase JSON database


## Retrieving data from firebase


## Listening for child updates


## Retrieving data with pagination


