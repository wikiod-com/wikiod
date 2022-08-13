---
title: "JList"
slug: "jlist"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Modify the selected elements in a JList
Given a `JList` like

    JList myList = new JList(items);

the selected items in the list can be modified through the `ListSelectionModel` of the  `JList`:

    ListSelectionModel sm = myList.getSelectionModel();  
    sm.clearSelection();                      // clears the selection
    sm.setSelectionInterval(index, index);    // Sets a selection interval
                                              // (single element, in this case)


Alternatively, `JList` also provides some convenient methods to directly manipulate the selected indexes:

    myList.setSelectionIndex(index);            // sets one selected index
                                                // could be used to define the Default Selection
    
    myList.setSelectedIndices(arrayOfIndexes);  // sets all indexes contained in
                                                // the array as selected

