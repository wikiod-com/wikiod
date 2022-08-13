---
title: "Object validation  by Annotations"
slug: "object-validation--by-annotations"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

mvc.net introduces data anotations for model validation. This can also be done in Xamarin

## Simple example
Add nuget package `System.ComponentModel.Annotations`

Define a class:

    public class BankAccount  
    {  
  
       public enum AccountType  
       {  
           Saving,  
           Current  
       }  
  
       [Required(ErrorMessage="First Name Required")]  
       [MaxLength(15,ErrorMessage="First Name should not more than 1`5 character")]  
       [MinLength(3,ErrorMessage="First Name should be more than 3 character")]  
       public string AccountHolderFirstName { get; set; }  
  
       [Required(ErrorMessage="Last Name Required")]  
       [MaxLength(15,ErrorMessage="Last Name should not more than 1`5 character")]  
       [MinLength(3,ErrorMessage="Last Name should be more than 3 character")]  
       public string AccountHolderLastName { get; set; }  
  
       [Required]  
       [RegularExpression("^[0-9]+$", ErrorMessage = "Only Number allowed in AccountNumber")]  
       public string AccountNumber { get; set; }  
  
       public AccountType AcType { get; set; }  
    }

Define a validator:

    public class GenericValidator   
    {  
        public static bool TryValidate(object obj, out ICollection<ValidationResult> results)  
        {  
            var context = new ValidationContext(obj, serviceProvider: null, items: null);  
            results = new List<ValidationResult>();  
            return Validator.TryValidateObject(  
                obj, context, results,  
                validateAllProperties: true  
            );  
        }  
    }

use the validator:

    var bankAccount = new BankAccount();  
    ICollection<ValidationResult> lstvalidationResult;  
  
    bool valid = GenericValidator.TryValidate(bankAccount, out lstvalidationResult);  
    if (!valid)  
    {  
        foreach (ValidationResult res in lstvalidationResult)  
        {  
            Console.WriteLine(res.MemberNames +":"+ res.ErrorMessage);  
        }  
          
    }  
    Console.ReadLine();

Output generated:

    First Name Required
    Last Name Required
    The AccountNumber field is required.

