---
title: "Propriétés"
slug: "proprietes"
draft: false
images: []
weight: 9930
type: docs
toc: true
---

Les propriétés combinent le stockage de données de classe des champs avec l'accessibilité des méthodes. Parfois, il peut être difficile de décider s'il faut utiliser une propriété, une propriété référençant un champ ou une méthode référençant un champ. En règle générale :

- Les propriétés doivent être utilisées sans champ interne si elles obtiennent et/ou définissent uniquement des valeurs ; sans qu'aucune autre logique ne se produise. Dans de tels cas, l'ajout d'un champ interne reviendrait à ajouter du code sans aucun avantage.

- Les propriétés doivent être utilisées avec des champs internes lorsque vous devez manipuler ou valider les données. Un exemple peut être de supprimer les espaces de début et de fin des chaînes ou de s'assurer qu'une date n'est pas dans le passé.

En ce qui concerne Méthodes vs Propriétés, où vous pouvez à la fois récupérer (`get`) et mettre à jour (`set`) une valeur, une propriété est le meilleur choix.
En outre, .Net fournit de nombreuses fonctionnalités qui utilisent la structure d'une classe ; par exemple. en ajoutant une grille à un formulaire, .Net listera par défaut toutes les propriétés de la classe sur ce formulaire ; ainsi, pour tirer le meilleur parti de ces conventions, prévoyez d'utiliser des propriétés lorsque ce comportement serait généralement souhaitable, et des méthodes où vous préféreriez que les types ne soient pas automatiquement ajoutés.

## Propriétés implémentées automatiquement
Les [propriétés implémentées automatiquement][1] ont été introduites dans C# 3.
Une propriété auto-implémentée est déclarée avec un getter et un setter vides (accesseurs) :

    public bool IsValid { get; set; }

Lorsqu'une propriété implémentée automatiquement est écrite dans votre code, le compilateur crée un champ anonyme privé accessible uniquement via les accesseurs de la propriété.

La déclaration de propriété auto-implémentée ci-dessus équivaut à écrire ce long code :

    private bool _isValid;
    public bool IsValid
    {
        get { return _isValid; }
        set { _isValid = value; }
    }

---

Les propriétés implémentées automatiquement ne peuvent pas avoir de logique dans leurs accesseurs, par exemple :

    public bool IsValid { get; set { PropertyChanged("IsValid"); } } // Invalid code

Une propriété auto-implémentée *peut* cependant avoir des modificateurs d'accès différents pour ses accesseurs :

    public bool IsValid { get; private set; }    

C# 6 permet aux propriétés auto-implémentées de n'avoir aucun setter (ce qui la rend immuable, puisque sa valeur ne peut être définie qu'à l'intérieur du constructeur ou codée en dur) :

    public bool IsValid { get; }    
    public bool IsValid { get; } = true;

Pour plus d'informations sur l'initialisation des propriétés implémentées automatiquement, lisez la documentation [Auto-property initializers][2].


[1] : https://msdn.microsoft.com/en-us/library/bb384054.aspx
[2] : https://www.wikiod.com/fr/docs/c%23/24/c-sharp-6-0-features/47/auto-property-initializers#t=201607211254385249419

## Obtention publique
Les getters sont utilisés pour exposer les valeurs des classes.

    string name;
    public string Name
    {
        get { return this.name; }
    }

## Ensemble public
Les setters sont utilisés pour attribuer des valeurs aux propriétés.

    string name;
    public string Name 
    {
        set { this.name = value; }
    }

## Accéder aux propriétés

    class Program 
    {
        public static void Main(string[] args)
        {
            Person aPerson = new Person("Ann Xena Sample", new DateTime(1984, 10, 22));
            //example of accessing properties (Id, Name & DOB)
            Console.WriteLine("Id is:  \t{0}\nName is:\t'{1}'.\nDOB is: \t{2:yyyy-MM-dd}.\nAge is: \t{3}", aPerson.Id, aPerson.Name, aPerson.DOB, aPerson.GetAgeInYears());
            //example of setting properties

            aPerson.Name = "   Hans Trimmer  ";
            aPerson.DOB = new DateTime(1961, 11, 11);
            //aPerson.Id = 5; //this won't compile as Id's SET method is private; so only accessible within the Person class.
            //aPerson.DOB = DateTime.UtcNow.AddYears(1); //this would throw a runtime error as there's validation to ensure the DOB is in past. 

            //see how our changes above take effect; note that the Name has been trimmed
            Console.WriteLine("Id is:  \t{0}\nName is:\t'{1}'.\nDOB is: \t{2:yyyy-MM-dd}.\nAge is: \t{3}", aPerson.Id, aPerson.Name, aPerson.DOB, aPerson.GetAgeInYears());

            Console.WriteLine("Press any key to continue");
            Console.Read();
        }
    }

    public class Person
    {
        private static int nextId = 0;
        private string name;
        private DateTime dob; //dates are held in UTC; i.e. we disregard timezones
        public Person(string name, DateTime dob)
        {
            this.Id = ++Person.nextId;
            this.Name = name;
            this.DOB = dob;
        }
        public int Id
        {
            get;
            private set;
        }
        public string Name
        {
            get { return this.name; }
            set
            {
                if (string.IsNullOrWhiteSpace(value)) throw new InvalidNameException(value);
                this.name = value.Trim();
            }
        }
        public DateTime DOB
        {
            get { return this.dob; }
            set 
            {
                if (value < DateTime.UtcNow.AddYears(-200) || value > DateTime.UtcNow) throw new InvalidDobException(value);
                this.dob = value; 
            }
        }
        public int GetAgeInYears()
        {
            DateTime today = DateTime.UtcNow;
            int offset = HasHadBirthdayThisYear() ? 0 : -1;
            return today.Year - this.dob.Year + offset;
        }
        private bool HasHadBirthdayThisYear()
        {
            bool hasHadBirthdayThisYear = true;
            DateTime today = DateTime.UtcNow;
            if (today.Month > this.dob.Month)
            {
                hasHadBirthdayThisYear = true;
            }
            else
            {
                if (today.Month == this.dob.Month)
                {
                    hasHadBirthdayThisYear = today.Day > this.dob.Day;
                }
                else
                {
                    hasHadBirthdayThisYear = false;
                }
            }
            return hasHadBirthdayThisYear;
        }
    }

    public class InvalidNameException : ApplicationException
    {
        const string InvalidNameExceptionMessage = "'{0}' is an invalid name.";
        public InvalidNameException(string value): base(string.Format(InvalidNameExceptionMessage,value)){}
    }
    public class InvalidDobException : ApplicationException
    { 
        const string InvalidDobExceptionMessage = "'{0:yyyy-MM-dd}' is an invalid DOB.  The date must not be in the future, or over 200 years in the past.";
        public InvalidDobException(DateTime value): base(string.Format(InvalidDobExceptionMessage,value)){}
    }

## Valeurs par défaut pour les propriétés
La définition d'une valeur par défaut peut être effectuée à l'aide d'initialiseurs (C#6)

    public class Name 
    {
        public string First { get; set; } = "James";
        public string Last { get; set; } = "Smith";
    }

S'il est en lecture seule, vous pouvez renvoyer des valeurs comme celle-ci :

      public class Name 
      {
          public string First => "James";
          public string Last => "Smith";
      }



## Diverses propriétés en contexte
    public class Person 
    {
        //Id property can be read by other classes, but only set by the Person class
        public int Id {get; private set;}
        //Name property can be retrieved or assigned 
        public string Name {get; set;}
        
        private DateTime dob;
        //Date of Birth property is stored in a private variable, but retrieved or assigned through the public property.
        public DateTime DOB
        {
            get { return this.dob; }
            set { this.dob = value; }
        }
        //Age property can only be retrieved; it's value is derived from the date of birth 
        public int Age 
        {
            get 
            {
                int offset = HasHadBirthdayThisYear() ? 0 : -1;
                return DateTime.UtcNow.Year - this.dob.Year + offset;
            }
        }

        //this is not a property but a method; though it could be rewritten as a property if desired.
        private bool HasHadBirthdayThisYear() 
        {
            bool hasHadBirthdayThisYear = true;
            DateTime today = DateTime.UtcNow;
            if (today.Month > this.dob.Month)
            {
                hasHadBirthdayThisYear = true;
            }
            else 
            {
                if (today.Month == this.dob.Month)
                {
                    hasHadBirthdayThisYear = today.Day > this.dob.Day;
                }
                else
                {
                    hasHadBirthdayThisYear = false;
                }
            }
            return hasHadBirthdayThisYear;
        }
    }


## Propriétés en lecture seule
# Déclaration

Un malentendu courant, en particulier chez les débutants, est que la propriété en lecture seule est celle marquée du mot-clé `readonly`. Ce n'est pas correct et en fait *ce qui suit est une erreur de compilation* :

    public readonly string SomeProp { get; set; }

Une propriété est en lecture seule lorsqu'elle n'a qu'un getter.

    public string SomeProp { get; }

# Utiliser des propriétés en lecture seule pour créer des classes immuables

    public Address
    {
        public string ZipCode { get; }
        public string City { get; }
        public string StreetAddress { get; }

        public Address(
            string zipCode,
            string city,
            string streetAddress)
        {
            if (zipCode == null)
                throw new ArgumentNullException(nameof(zipCode));
            if (city == null)
                throw new ArgumentNullException(nameof(city));
            if (streetAddress == null)
                throw new ArgumentNullException(nameof(streetAddress));

            ZipCode = zipCode;
            City = city;
            StreetAddress = streetAddress;
        }
    }

