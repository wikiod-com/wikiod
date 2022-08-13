---
title: "PhantomData"
slug: "phantomdata"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Using PhantomData as a Type Marker
Using the `PhantomData` type like this allows you to use a specific type without needing it to be part of the Struct.

    use std::marker::PhantomData;
    
    struct Authenticator<T: GetInstance> {
        _marker: PhantomData<*const T>, // Using `*const T` indicates that we do not own a T
    }
    
    impl<T: GetInstance> Authenticator<T> {
        fn new() -> Authenticator<T> {
            Authenticator {
                _marker: PhantomData,
            }
        }
        
        fn auth(&self, id: i64) -> bool {
            T::get_instance(id).is_some()
        }
    }
    
    trait GetInstance {
        type Output; // Using nightly this could be defaulted to `Self`
        fn get_instance(id: i64) -> Option<Self::Output>;
    }
    
    struct Foo;
    
    impl GetInstance for Foo {
        type Output = Self; 
        fn get_instance(id: i64) -> Option<Foo> {
            // Here you could do something like a Database lookup or similarly
            if id == 1 {
                Some(Foo)
            } else {
                None
            }
        }
    }
    
    struct User;
    
    impl GetInstance for User {
        type Output = Self;
        fn get_instance(id: i64) -> Option<User> {
            // Here you could do something like a Database lookup or similarly
            if id == 2 {
                Some(User)
            } else {
                None
            }
        }
    }
    
    fn main() {
        let user_auth = Authenticator::<User>::new();
        let other_auth = Authenticator::<Foo>::new();
        
        assert!(user_auth.auth(2));
        assert!(!user_auth.auth(1));
        
        assert!(other_auth.auth(1));
        assert!(!other_auth.auth(2));
        
    }

