---
title: "Propriedades"
slug: "propriedades"
draft: false
images: []
weight: 9930
type: docs
toc: true
---

As propriedades combinam o armazenamento de dados de classe dos campos com a acessibilidade dos métodos. Às vezes, pode ser difícil decidir se deve usar uma propriedade, uma propriedade que faz referência a um campo ou um método que faz referência a um campo. Como um princípio básico:

- As propriedades devem ser utilizadas sem campo interno se apenas obtiverem e/ou definirem valores; sem nenhuma outra lógica ocorrendo. Nesses casos, adicionar um campo interno seria adicionar código sem nenhum benefício.

- As propriedades devem ser usadas com campos internos quando for necessário manipular ou validar os dados. Um exemplo pode ser remover espaços iniciais e finais de strings ou garantir que uma data não esteja no passado.

Com relação a Métodos vs Propriedades, onde você pode tanto recuperar (`get`) quanto atualizar (`set`) um valor, uma propriedade é a melhor escolha.
Além disso, o .Net oferece muitas funcionalidades que fazem uso da estrutura de uma classe; por exemplo. adicionando uma grade a um formulário, .Net listará, por padrão, todas as propriedades da classe nesse formulário; portanto, para fazer o melhor uso de tais convenções, planeje usar propriedades quando esse comportamento for normalmente desejável e métodos onde você preferir que os tipos não sejam adicionados automaticamente.

## Propriedades implementadas automaticamente
[Propriedades implementadas automaticamente][1] foram introduzidas no C# 3.
Uma propriedade autoimplementada é declarada com um getter e setter vazios (acessadores):

    public bool IsValid { get; set; }

Quando uma propriedade autoimplementada é escrita em seu código, o compilador cria um campo anônimo privado que só pode ser acessado por meio dos acessadores da propriedade.

A declaração de propriedade autoimplementada acima é equivalente a escrever este código longo:

    private bool _isValid;
    public bool IsValid
    {
        get { return _isValid; }
        set { _isValid = value; }
    }

---

Propriedades autoimplementadas não podem ter lógica em seus acessadores, por exemplo:

    public bool IsValid { get; set { PropertyChanged("IsValid"); } } // Invalid code

Uma propriedade autoimplementada *pode*, no entanto, ter diferentes modificadores de acesso para seus acessadores:

    public bool IsValid { get; private set; }    

O C# 6 permite que as propriedades autoimplementadas não tenham nenhum setter (tornando-o imutável, pois seu valor pode ser definido apenas dentro do construtor ou codificado):

    public bool IsValid { get; }    
    public bool IsValid { get; } = true;

Para obter mais informações sobre como inicializar propriedades autoimplementadas, leia a documentação [Auto-property initializers][2].


[1]: https://msdn.microsoft.com/en-us/library/bb384054.aspx
[2]: https://www.wikiod.com/pt/docs/c%23/24/c-sharp-6-0-features/47/auto-property-initializers#t=201607211254385249419

## Obtenção Pública
Getters são usados ​​para expor valores de classes.

    string name;
    public string Name
    {
        get { return this.name; }
    }

## Conjunto público
Setters são usados ​​para atribuir valores a propriedades.

    string name;
    public string Name 
    {
        set { this.name = value; }
    }

## Acessando Propriedades

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

## Valores padrão para propriedades
Definir um valor padrão pode ser feito usando inicializadores (C#6)

    public class Name 
    {
        public string First { get; set; } = "James";
        public string Last { get; set; } = "Smith";
    }

Se for somente leitura, você pode retornar valores como este:

      public class Name 
      {
          public string First => "James";
          public string Last => "Smith";
      }



## Várias propriedades no contexto
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


## Propriedades somente leitura
# Declaração

Um mal-entendido comum, especialmente para iniciantes, é que a propriedade read-only é aquela marcada com a palavra-chave `readonly`. Isso não está correto e, de fato, *seguir é um erro de tempo de compilação*:

    public readonly string SomeProp { get; set; }

Uma propriedade é somente leitura quando possui apenas um getter.

    public string SomeProp { get; }

# Usando propriedades somente leitura para criar classes imutáveis

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

