---
title: "Arayüzler"
slug: "arayuzler"
draft: false
images: []
weight: 9819
type: docs
toc: true
---

## Bir arayüz uygulama
Herhangi bir sınıfta, onu 'uygulayan' bir yöntemin varlığını zorlamak için bir arabirim kullanılır. Arayüz 'arayüz' anahtar kelimesi ile tanımlanır ve bir sınıf, sınıf adından sonra ': InterfaceName' ekleyerek onu 'uygulayabilir'. Bir sınıf, her arabirimi bir virgülle ayırarak birden çok arabirim uygulayabilir.
` : ArayüzAdı, ISecondInterface`
    
    public interface INoiseMaker
    {
        string MakeNoise();
    }

    public class Cat : INoiseMaker
    {
        public string MakeNoise()
        {
            return "Nyan";
        }
    }

    public class Dog : INoiseMaker
    {
        public string MakeNoise()
        {
            return "Woof";
        }
    }

"INoiseMaker"ı uyguladıkları için, hem "cat" hem de "dog"un "string MakeNoise()" yöntemini içermesi gerekir ve bu olmadan derleme başarısız olur.

## Açık arayüz uygulaması
Ortak bir yöntem tanımlayan birden çok arabirim uyguladığınızda açık arabirim uygulaması gereklidir, ancak yöntemi çağırmak için hangi arabirimin kullanıldığına bağlı olarak farklı uygulamalar gerekir (birden çok arabirim aynı yöntemi paylaşıyorsa açık uygulamalara ihtiyacınız olmadığını unutmayın ve ortak bir uygulama mümkündür).

    interface IChauffeur 
    {
        string Drive();
    }

    interface IGolfPlayer
    {
        string Drive();
    }

    class GolfingChauffeur : IChauffeur, IGolfPlayer 
    {
        public string Drive()
        {
            return "Vroom!";
        }

        string IGolfPlayer.Drive()
        {
            return "Took a swing...";
        }
    }


    GolfingChauffeur obj = new GolfingChauffeur();
    IChauffeur chauffeur = obj;
    IGolfPlayer golfer = obj;
    
    Console.WriteLine(obj.Drive()); // Vroom!
    Console.WriteLine(chauffeur.Drive()); // Vroom!
    Console.WriteLine(golfer.Drive()); // Took a swing...

Uygulama, arayüzü kullanmak dışında başka hiçbir yerden çağrılamaz:

    public class Golfer : IGolfPlayer
    {
        string IGolfPlayer.Drive()
        {
            return "Swinging hard...";
        }
        public void Swing()
        {
            Drive(); // Compiler error: No such method
        }
    }

Bu nedenle, açıkça uygulanan bir arabirimin karmaşık uygulama kodunu ayrı, özel bir yönteme koymak avantajlı olabilir.

Açık bir arabirim uygulaması elbette yalnızca o arabirim için gerçekten var olan yöntemler için kullanılabilir:

    public class ProGolfer : IGolfPlayer
    {
        string IGolfPlayer.Swear() // Error
        {
            return "The ball is in the pit";
        }
    }

Benzer şekilde, sınıf üzerinde bu arabirimi bildirmeden açık bir arabirim uygulaması kullanmak da bir hataya neden olur.

# İpucu:
Arayüzleri açıkça uygulamak, ölü kodu önlemek için de kullanılabilir. Bir yönteme artık ihtiyaç duyulmadığında ve arayüzden kaldırıldığında, derleyici halen var olan her uygulama hakkında şikayette bulunacaktır.

# Not:

Programcılar, tür bağlamından bağımsız olarak sözleşmenin aynı olmasını bekler ve açık uygulama çağrıldığında farklı davranışlar sergilememelidir.
Dolayısıyla, yukarıdaki örnekten farklı olarak, `IGolfPlayer.Drive` ve `Drive` mümkün olduğunda aynı şeyi yapmalıdır.



## Birden çok arayüz uygulama
    public interface IAnimal 
    {
        string Name { get; set; }
    }
    
    public interface INoiseMaker
    {
        string MakeNoise();
    }
    
    public class Cat : IAnimal, INoiseMaker
    {
        public Cat() 
        {
            Name = "Cat";
        }
    
        public string Name { get; set; }

        public string MakeNoise()
        {
            return "Nyan";
        }
    }

## Arayüz Temelleri
Bir Arayüzün işlevsellik "sözleşme" olarak bilinen işlevi. Bu, özellikleri ve yöntemleri bildirdiği, ancak bunları uygulamadığı anlamına gelir.

Yani sınıflardan farklı olarak Arayüzler:

- Örneklenemez
- Herhangi bir işlevselliğe sahip olamaz
- Yalnızca yöntemleri içerebilir * *(Özellikler ve Olaylar dahili yöntemlerdir)*
- Bir arabirimi devralmaya "Uygulama" denir
- 1 sınıftan miras alabilirsiniz, ancak birden çok Arabirimi "Uygulayabilirsiniz"


    public interface ICanDoThis{
        void TheThingICanDo();
        int SomeValueProperty { get; set; }
    }

Dikkat edilmesi gerekenler:
- "I" öneki, arayüzler için kullanılan bir adlandırma kuralıdır.
- İşlev gövdesi noktalı virgül ";" ile değiştirilir.
- Özellikler aynı zamanda dahili olarak yöntem oldukları için de izin verilir.


    public class MyClass : ICanDoThis {
        public void TheThingICanDo(){
            // do the thing
        }

        public int SomeValueProperty { get; set; }
        public int SomeValueNotImplemtingAnything { get; set; }
    }

.

    ICanDoThis obj = new MyClass();

    // ok
    obj.TheThingICanDo();

    // ok
    obj.SomeValueProperty = 5;

    // Error, this member doesn't exist in the interface
    obj.SomeValueNotImplemtingAnything = 5;

    // in order to access the property in the class you must "down cast" it
    ((MyClass)obj).SomeValueNotImplemtingAnything = 5; // ok

Bu, özellikle WinForms veya WPF gibi UI çerçeveleriyle çalışırken kullanışlıdır, çünkü kullanıcı denetimi oluşturmak için bir temel sınıftan devralmak zorunludur ve farklı denetim türleri üzerinde soyutlama oluşturma yeteneğini kaybedersiniz. Bir örnek? geliyor:

    public class MyTextBlock : TextBlock {
        public void SetText(string str){
            this.Text = str;
        }
    }

    public class MyButton : Button {
        public void SetText(string str){
            this.Content = str;
        }
    }

Önerilen sorun, her ikisinin de bir miktar "Metin" kavramı içermesi ancak özellik adlarının farklı olmasıdır. Ve 2 farklı sınıfa zorunlu bir mirasa sahip oldukları için *soyut bir temel sınıf* oluşturamazsınız. Bir arayüz bunu hafifletebilir

    public interface ITextControl{
        void SetText(string str);
    }

    public class MyTextBlock : TextBlock, ITextControl {
        public void SetText(string str){
            this.Text = str;
        }
    }

    public class MyButton : Button, ITextControl {
        public void SetText(string str){
            this.Content = str;
        }

        public int Clicks { get; set; }
    }

Artık MyButton ve MyTextBlock birbirinin yerine kullanılabilir.

    var controls = new List<ITextControls>{
        new MyTextBlock(),
        new MyButton()
    };

    foreach(var ctrl in controls){
        ctrl.SetText("This text will be applied to both controls despite them being different");


        // Compiler Error, no such member in interface
        ctrl.Clicks = 0;

        // Runtime Error because 1 class is in fact not a button which makes this cast invalid
        ((MyButton)ctrl).Clicks = 0;


        /* the solution is to check the type first.
        This is usually considered bad practice since
        it's a symptom of poor abstraction */
        var button = ctrl as MyButton;
        if(button != null)
            button.Clicks = 0; // no errors

       
    }

## Arayüz Uygulama Örneği Olarak IComparable<T>
Arayüzler, pratikte görünene kadar soyut görünebilir. "IComparable" ve "IComparable<T>", arayüzlerin bize neden yardımcı olabileceğine dair harika örneklerdir.

Diyelim ki bir çevrimiçi mağaza programında satın alabileceğiniz çeşitli öğeler var. Her öğenin bir adı, kimlik numarası ve fiyatı vardır.

    public class Item {
        
        public string name; // though public variables are generally bad practice,
        public int idNumber; // to keep this example simple we will use them instead
        public decimal price; // of a property.

        // body omitted for brevity        

    }

Bir `Liste<Item>` içinde `Öğe`lerimiz var ve programımızda bir yerde, listemizi kimlik numarasına göre küçükten büyüğe sıralamak istiyoruz. Kendi sıralama algoritmamızı yazmak yerine, 'List<T>'nin zaten sahip olduğu 'Sort()' yöntemini kullanabiliriz. Ancak, 'Item' sınıfımız şu anda olduğu için, 'List<T>'nin listeyi hangi sırayla sıralayacağını anlamasının bir yolu yoktur. İşte burada "IComparable" arayüzü devreye giriyor.

"CompareTo" yöntemini doğru bir şekilde uygulamak için, "CompareTo", parametre geçerli olandan "küçükse" pozitif bir sayı, eşitlerse sıfır ve parametre "büyüktür" ise negatif bir sayı döndürmelidir.

    Item apple = new Item();
    apple.idNumber = 15;
    Item banana = new Item();
    banana.idNumber = 4;
    Item cow = new Item();
    cow.idNumber = 15;
    Item diamond = new Item();
    diamond.idNumber = 18;

    Console.WriteLine(apple.CompareTo(banana)); // 11
    Console.WriteLine(apple.CompareTo(cow)); // 0
    Console.WriteLine(apple.CompareTo(diamond)); // -3

İşte "Öğe"nin arayüzün uygulanması örneği:

    public class Item : IComparable<Item> {
        
        private string name;
        private int idNumber;
        private decimal price;

        public int CompareTo(Item otherItem) {

            return (this.idNumber - otherItem.idNumber);

        }

        // rest of code omitted for brevity    

    }

Yüzey düzeyinde, öğemizdeki 'CompareTo' yöntemi basitçe kimlik numaralarındaki farkı döndürür, ancak yukarıdakiler pratikte ne yapar?

Şimdi, bir 'List<Item>' nesnesi üzerinde 'Sort()'u çağırdığımızda, 'Liste', nesneleri hangi sıraya koyacağını belirlemesi gerektiğinde otomatik olarak 'Item'in 'CompareTo' yöntemini çağıracaktır. 'Liste<T>' dışında, iki nesneyi karşılaştırma yeteneğine ihtiyaç duyan diğer nesneler 'Öğe' ile çalışacaktır, çünkü iki farklı 'Öğe'nin birbiriyle karşılaştırılabilmesini tanımladık.

## Arayüzleri neden kullanırız
Arayüz, arayüzün kullanıcısı ile onu uygulayan sınıf arasındaki bir sözleşmenin tanımıdır. Bir arabirimi düşünmenin bir yolu, bir nesnenin belirli işlevleri yerine getirebileceğinin bir bildirimidir.

Diyelim ki, farklı türde şekilleri temsil etmek için bir "IShape" arayüzü tanımladık, bir şeklin bir alanı olmasını bekliyoruz, bu yüzden arayüz uygulamalarını kendi alanlarını döndürmeye zorlamak için bir yöntem tanımlayacağız:

    public interface IShape
    {
        double ComputeArea();
    }

Şu iki şekle sahip olalım: "Dikdörtgen" ve "Daire"

    public class Rectangle : IShape
    {
        private double length;
        private double width;

        public Rectangle(double length, double width)
        {
            this.length = length;
            this.width = width;
        }

        public double ComputeArea()
        {
            return length * width;
        }
    }

    public class Circle : IShape
    {
        private double radius;

        public Circle(double radius)
        {
            this.radius = radius;
        }

        public double ComputeArea()
        {
            return Math.Pow(radius, 2.0) * Math.PI;
        }
    }

Her birinin kendi alanı tanımı vardır, ancak ikisi de şekildir. Bu yüzden onları programımızda 'IShape' olarak görmek sadece mantıklı:

    private static void Main(string[] args)
    {
        var shapes = new List<IShape>() { new Rectangle(5, 10), new Circle(5) };
        ComputeArea(shapes);

        Console.ReadKey();
    }

    private static void ComputeArea(IEnumerable<IShape> shapes) 
    {
        foreach (shape in shapes)
        {
            Console.WriteLine("Area: {0:N}, shape.ComputeArea());
        }
    }

    // Output:
    // Area : 50.00
    // Area : 78.54



## Açık Uygulama ile üyeleri "gizleme"
Arayüzlerin sınıfınızı, umurunuzda bile olmayan çok sayıda üyeyle kirletmesinden nefret etmiyor musunuz? Peki bir çözüm buldum! Açık Uygulamalar

    public interface IMessageService {
        void OnMessageRecieve();
        void SendMessage();
        string Result { get; set; }
        int Encoding { get; set; }
        // yadda yadda
    }

Normalde sınıfı böyle uygularsınız.

    public class MyObjectWithMessages : IMessageService {
         public void OnMessageRecieve(){

         }

         public void SendMessage(){

         }

         public string Result { get; set; }
         public int Encoding { get; set; }
    }
Her üye halka açıktır.

    var obj = new MyObjectWithMessages();

    // why would i want to call this function?
    obj.OnMessageRecieve();

Cevap: İstemiyorum. Yani kamuya açıklanmamalı
ancak üyelerin özel olarak ilan edilmesi, derleyicinin bir hata atmasına neden olur.



Çözüm, açık uygulamayı kullanmaktır:

    public class MyObjectWithMessages : IMessageService{
        void IMessageService.OnMessageRecieve() {
            
        }

        void IMessageService.SendMessage() {
            
        }

        string IMessageService.Result { get; set; }
        int IMessageService.Encoding { get; set; }
    }

Artık üyeleri gerektiği gibi uyguladınız ve hiçbir üyeyi herkese açık olarak göstermeyecekler.

    var obj = new MyObjectWithMessages();

    /* error member does not exist on type MyObjectWithMessages. 
     * We've succesfully made it "private" */
    obj.OnMessageRecieve();

Açıkça uygulanmasına rağmen hala üyeye gerçekten erişmek istiyorsanız, yapmanız gereken tek şey nesneyi arayüze göndermek ve gitmekte fayda var.

    ((IMessageService)obj).OnMessageRecieve();

