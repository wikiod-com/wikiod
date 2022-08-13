---
title: "Firebase Query"
slug: "firebase-query"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Firebase Query can be used to order a collection of data based on some attributes as well as restricted to the large list of items (for like chat data) down to a number suitable for synchronizing to the client.

Just as with a Reference, you can receive data from a Query by using the on() method. You will only receive events and DataSnapshots for the subset of the data that matches your query.



## Firebase Query Example
    private void loadData(){
            DatabaseReference dbRef = FirebaseDatabase.getInstance().getReference();
    
            Query dataQuery = dbRef.child("chat").orderByChild("id").equalTo("user1");
            dataQuery.addListenerForSingleValueEvent(new ValueEventListener() {
                @Override
                public void onDataChange(DataSnapshot dataSnapshot) {
                    if (dataSnapshot.exists()) {
                        // dataSnapshot is the "issue" node with all children with id 0
                        for (DataSnapshot issue : dataSnapshot.getChildren()) {
                            // do something with the individual "issues"
                        }
                    }
                }
    
                @Override
                public void onCancelled(DatabaseError databaseError) {
    
                }
            });
        }

