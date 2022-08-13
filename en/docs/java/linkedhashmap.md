---
title: "LinkedHashMap"
slug: "linkedhashmap"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

LinkedHashMap class is Hash table and Linked list implementation of the Map interface, with predictable iteration order. It inherits HashMap class and implements the Map interface.

The important points about Java LinkedHashMap class are:
A LinkedHashMap contains values based on the key.
It contains only unique elements.
It may have one null key and multiple null values.
It is same as HashMap instead maintains insertion order.

## Java LinkedHashMap class
**Key Points:-**

- Is Hash table and Linked list implementation of the Map interface, with predictable iteration order.

- inherits HashMap class and implements the Map interface.

- contains values based on the key.
- only unique elements.
- may have one null key and multiple null values.
- same as HashMap instead maintains insertion order.

**Methods :-**

- void clear().
- boolean containsKey(Object key).
- Object get(Object key).
- protected boolean removeEldestEntry(Map.Entry eldest)

**Example :-**

 

    public static void main(String arg[])
        {
            LinkedHashMap<String, String> lhm = new LinkedHashMap<String, String>();
            lhm.put("Ramesh", "Intermediate");
            lhm.put("Shiva", "B-Tech");
            lhm.put("Santosh", "B-Com");
            lhm.put("Asha", "Msc");
            lhm.put("Raghu", "M-Tech");
            
            Set set = lhm.entrySet();
            Iterator i = set.iterator();
            while (i.hasNext()) {
                Map.Entry me = (Map.Entry) i.next();
                System.out.println(me.getKey() + " : " + me.getValue());
            }
                    
            System.out.println("The Key Contains : " + lhm.containsKey("Shiva"));
            System.out.println("The value to the corresponding to key : " + lhm.get("Asha"));    
        }



