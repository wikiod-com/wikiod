---
title: "ActiveRecord Locking"
slug: "activerecord-locking"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

## Optimistic Locking
    user_one = User.find(1)
    user_two = User.find(1)
   
    user_one.name = "John"
    user_one.save
    # Run at the same instance 
    user_two.name = "Doe"
    user_two.save # Raises a ActiveRecord::StaleObjectError

## Pessimistic Locking
    appointment = Appointment.find(5)
    appointment.lock!
    #no other users can read this appointment, 
    #they have to wait until the lock is released
    appointment.save! 
    #lock is released, other users can read this account

