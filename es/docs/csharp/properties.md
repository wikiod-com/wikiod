---
title: "Propiedades"
slug: "propiedades"
draft: false
images: []
weight: 9930
type: docs
toc: true
---

Las propiedades combinan el almacenamiento de datos de clase de los campos con la accesibilidad de los métodos. A veces puede ser difícil decidir si usar una propiedad, una propiedad que hace referencia a un campo o un método que hace referencia a un campo. Como una regla de oro:

- Las propiedades deben usarse sin un campo interno si solo obtienen y/o establecen valores; sin que ocurra otra lógica. En tales casos, agregar un campo interno sería agregar código sin ningún beneficio.

- Las propiedades deben usarse con campos internos cuando necesite manipular o validar los datos. Un ejemplo puede ser eliminar los espacios iniciales y finales de las cadenas o asegurarse de que una fecha no sea del pasado.

Con respecto a Métodos vs Propiedades, donde puede recuperar (`obtener`) y actualizar (`establecer`) un valor, una propiedad es la mejor opción.
Además, .Net proporciona mucha funcionalidad que hace uso de la estructura de una clase; p.ej. al agregar una cuadrícula a un formulario, .Net listará por defecto todas las propiedades de la clase en ese formulario; por lo tanto, para hacer el mejor uso de tales convenciones, planee usar propiedades cuando este comportamiento sea típicamente deseable, y métodos donde prefiera que los tipos no se agreguen automáticamente.

## Propiedades implementadas automáticamente
[Propiedades implementadas automáticamente][1] se introdujeron en C# 3.
Una propiedad implementada automáticamente se declara con un getter y setter vacíos (accesorios):

    public bool IsValid { get; set; }

Cuando se escribe una propiedad implementada automáticamente en su código, el compilador crea un campo anónimo privado al que solo se puede acceder a través de los accesores de la propiedad.

La declaración de propiedad implementada automáticamente anterior es equivalente a escribir este extenso código:

    private bool _isValid;
    public bool IsValid
    {
        get { return _isValid; }
        set { _isValid = value; }
    }

---

Las propiedades implementadas automáticamente no pueden tener ninguna lógica en sus accesores, por ejemplo:

    public bool IsValid { get; set { PropertyChanged("IsValid"); } } // Invalid code

Sin embargo, una propiedad implementada automáticamente *puede* tener diferentes modificadores de acceso para sus accesores:

    public bool IsValid { get; private set; }    

C# 6 permite que las propiedades implementadas automáticamente no tengan setter (lo que las hace inmutables, ya que su valor solo se puede establecer dentro del constructor o codificarlo de forma rígida):

    public bool IsValid { get; }    
    public bool IsValid { get; } = true;

Para obtener más información sobre la inicialización de propiedades implementadas automáticamente, lea la documentación [Inicializadores de propiedades automáticas][2].


[1]: https://msdn.microsoft.com/en-us/library/bb384054.aspx
[2]: https://www.wikiod.com/es/docs/c%23/24/c-sharp-6-0-features/47/auto-property-initializers#t=201607211254385249419

## Obtener público
Los captadores se utilizan para exponer valores de clases.

    string name;
    public string Name
    {
        get { return this.name; }
    }

## Conjunto público
Los establecedores se utilizan para asignar valores a las propiedades.

    string name;
    public string Name 
    {
        set { this.name = value; }
    }

## Acceso a las propiedades

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

## Valores predeterminados para las propiedades
Se puede establecer un valor predeterminado mediante el uso de inicializadores (C#6)

    public class Name 
    {
        public string First { get; set; } = "James";
        public string Last { get; set; } = "Smith";
    }

Si es de solo lectura, puede devolver valores como este:

      public class Name 
      {
          public string First => "James";
          public string Last => "Smith";
      }



## Varias propiedades en contexto
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


## Propiedades de solo lectura
# Declaración

Un malentendido común, especialmente los principiantes, es que la propiedad de solo lectura es la que está marcada con la palabra clave `solo lectura`. Eso no es correcto y, de hecho, *el siguiente es un error de tiempo de compilación*:

    public readonly string SomeProp { get; set; }

Una propiedad es de solo lectura cuando solo tiene un captador.

    public string SomeProp { get; }

# Uso de propiedades de solo lectura para crear clases inmutables

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

