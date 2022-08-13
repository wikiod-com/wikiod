---
title: "Models"
slug: "models"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Model Validation with Validation Attrributes
Validation attributes can be used to easily configure model validation.

    public class MyModel
    {
        public int id { get; set; }
    
        //sets the FirstName to be required, and no longer than 100 characters
        [Required]
        [StringLength(100)]
        public string FirstName { get; set; }
    }

The built in attributes are:

 - `[CreditCard]`: Validates the property has a credit card format.
 - `[Compare]`: Validates two properties in a model match.
 - `[EmailAddress]`: Validates the property has an email format.
 - `[Phone]`: Validates the property has a telephone format.
 - `[Range]`: Validates the property value falls within the given range.
 - `[RegularExpression]`: Validates that the data matches the specified regular expression.
 - `[Required]`: Makes a property required.
 - `[StringLength]`: Validates that a string property has at most the given maximum length.
 - `[Url]`: Validates the property has a URL format.


## Model Validation with Custom Attribute
If the built in attributes are not sufficient to validate your model data, then you can place your validation logic in a class derived from ValidationAttribute. In this example only odd numbers are valid values for a model member.

**Custom Validation Attribute**

    public class OddNumberAttribute : ValidationAttribute
    {
        protected override ValidationResult IsValid(object value, ValidationContext validationContext)
        {
            try
            {
                var number = (int) value;
                if (number % 2 == 1)
                    return ValidationResult.Success;
                else
                    return new ValidationResult("Only odd numbers are valid.");
            }
            catch (Exception)
            {
                return new ValidationResult("Not a number.");
            }            
        }
    }

**Model Class**

    public class MyModel
    {
        [OddNumber]
        public int Number { get; set; }
    }

