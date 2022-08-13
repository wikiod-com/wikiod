---
title: "Model Restraints"
slug: "model-restraints"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## One-to-many relationships
**UserType belongs to many Users <-> Users have one UserType**

One way navigation property with required 

    public class UserType
    {
        public int UserTypeId {get; set;}
    }
    public class User
    {
        public int UserId {get; set;}
        public int UserTypeId {get; set;}
        public virtual UserType UserType {get; set;}
    }

    Entity<User>().HasRequired(u => u.UserType).WithMany().HasForeignKey(u => u.UserTypeId);

One way navigation property with optional (foreign key must be `Nullable` type)

    public class UserType
    {
        public int UserTypeId {get; set;}
    }
    public class User
    {
        public int UserId {get; set;}
        public int? UserTypeId {get; set;}
        public virtual UserType UserType {get; set;}
    }

    Entity<User>().HasOptional(u => u.UserType).WithMany().HasForeignKey(u => u.UserTypeId);

Two way navigation property with (required/optional change the foreign key property as needed)

    public class UserType
    {
        public int UserTypeId {get; set;}
        public virtual ICollection<User> Users {get; set;}
    }
    public class User
    {
        public int UserId {get; set;}
        public int UserTypeId {get; set;}
        public virtual UserType UserType {get; set;}
    }

Required

    Entity<User>().HasRequired(u => u.UserType).WithMany(ut => ut.Users).HasForeignKey(u => u.UserTypeId);

Optional

    Entity<User>().HasOptional(u => u.UserType).WithMany(ut => ut.Users).HasForeignKey(u => u.UserTypeId);



