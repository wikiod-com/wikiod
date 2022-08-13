---
title: "Sequence"
slug: "sequence"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

## Create Sequence
     CREATE SEQUENCE orders_seq
     START WITH     1000
     INCREMENT BY   1;
Creates a sequence with a starting value of 1000 which is incremented by 1.

## Using Sequences
a reference to *seq_name*.NEXTVAL is used to get the next value in a sequence. A single statement can only generate a single sequence value. If there are multiple references to NEXTVAL in a statement, they use will use the same generated number.

NEXTVAL can be used for INSERTS

    INSERT INTO Orders (Order_UID, Customer)
            VALUES (orders_seq.NEXTVAL, 1032);

It can be used for UPDATES

    UPDATE Orders
    SET Order_UID = orders_seq.NEXTVAL
    WHERE Customer = 581;
It can also be used for SELECTS

    SELECT Order_seq.NEXTVAL FROM dual;





