---
title: "Özellikleri"
slug: "ozellikleri"
draft: false
images: []
weight: 9930
type: docs
toc: true
---

Özellikler, alanların sınıf veri depolamasını yöntemlerin erişilebilirliği ile birleştirir. Bazen bir özelliğin mi, bir alana referans veren bir özelliğin mi yoksa bir alana referans veren bir yöntemin mi kullanılacağına karar vermek zor olabilir. Kural olarak:

- Özellikler, yalnızca değerler alıyorsa ve/veya ayarlarsa dahili alan olmadan kullanılmalıdır; başka bir mantık oluşmadan. Bu gibi durumlarda, dahili bir alan eklemek, hiçbir faydası olmayacak şekilde kod eklemek olacaktır.

- Verileri manipüle etmeniz veya doğrulamanız gerektiğinde, özellikler dahili alanlarla birlikte kullanılmalıdır. Bir örnek, dizelerin başındaki ve sonundaki boşlukları kaldırmak veya bir tarihin geçmişte olmamasını sağlamak olabilir.

Bir değeri hem alabileceğiniz ('get') hem de güncelleyebileceğiniz ('ayarlayın') Yöntemler ve Özellikler ile ilgili olarak, bir özellik daha iyi bir seçimdir.
Ayrıca .Net, bir sınıfın yapısını kullanan birçok işlevsellik sağlar; Örneğin. bir forma bir kılavuz ekleyerek, .Net varsayılan olarak o formdaki sınıfın tüm özelliklerini listeler; bu nedenle, bu tür sözleşmelerden en iyi şekilde yararlanmak için, bu davranış tipik olarak istendiğinde özellikleri ve türlerin otomatik olarak eklenmemesini tercih ettiğiniz yöntemleri kullanmayı planlayın.

## Otomatik uygulanan özellikler
[Otomatik olarak uygulanan özellikler][1] C# 3'te tanıtıldı.
Otomatik olarak uygulanan bir özellik, boş bir alıcı ve ayarlayıcı (erişimciler) ile bildirilir:

    public bool IsValid { get; set; }

Kodunuza otomatik olarak uygulanan bir özellik yazıldığında, derleyici yalnızca özelliğin erişimcileri aracılığıyla erişilebilen özel bir anonim alan oluşturur.

Yukarıdaki otomatik uygulanan özellik ifadesi, bu uzun kodu yazmaya eşdeğerdir:

    private bool _isValid;
    public bool IsValid
    {
        get { return _isValid; }
        set { _isValid = value; }
    }

---

Otomatik uygulanan özelliklerin erişimcilerinde herhangi bir mantık olamaz, örneğin:

    public bool IsValid { get; set { PropertyChanged("IsValid"); } } // Invalid code

Bununla birlikte, otomatik olarak uygulanan bir özellik, erişimcileri için farklı erişim değiştiricilerine * sahip olabilir*:

    public bool IsValid { get; private set; }    

C# 6, otomatik olarak uygulanan özelliklerin hiçbir ayarlayıcıya sahip olmamasına izin verir (değeri yalnızca yapıcı içinde ayarlanabileceği veya sabit kodlanabileceği için onu değişmez kılar):

    public bool IsValid { get; }    
    public bool IsValid { get; } = true;

Otomatik olarak uygulanan özellikleri başlatma hakkında daha fazla bilgi için [Otomatik özellik başlatıcılar][2] belgelerini okuyun.


[1]: https://msdn.microsoft.com/en-us/library/bb384054.aspx
[2]: https://www.wikiod.com/tr/docs/c%23/24/c-sharp-6-0-features/47/auto-property-initializers#t=201607211254385249419

## Herkese Açık
Getters, sınıflardan değerleri ortaya çıkarmak için kullanılır.

    string name;
    public string Name
    {
        get { return this.name; }
    }

## Genel Set
Ayarlayıcılar, özelliklere değer atamak için kullanılır.

    string name;
    public string Name 
    {
        set { this.name = value; }
    }

## Mülklere Erişme

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

## Özellikler için Varsayılan Değerler
Varsayılan bir değerin ayarlanması, Başlatıcılar (C#6) kullanılarak yapılabilir.

    public class Name 
    {
        public string First { get; set; } = "James";
        public string Last { get; set; } = "Smith";
    }

Salt okunursa, aşağıdaki gibi değerler döndürebilirsiniz:

      public class Name 
      {
          public string First => "James";
          public string Last => "Smith";
      }



## Bağlamdaki Çeşitli Özellikler
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


## Salt okunur özellikler
# Beyanname

Özellikle yeni başlayanlar için yaygın bir yanlış anlama, salt okunur özelliğinin "salt okunur" anahtar kelimesiyle işaretlenmiş olmasıdır. Bu doğru değil ve aslında *aşağıdaki bir derleme zamanı hatasıdır*:

    public readonly string SomeProp { get; set; }

Bir özellik, yalnızca bir alıcıya sahip olduğunda salt okunurdur.

    public string SomeProp { get; }

# Değişmez sınıflar oluşturmak için salt okunur özellikleri kullanma

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

