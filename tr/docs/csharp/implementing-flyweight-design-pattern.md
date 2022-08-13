---
title: "Flyweight Tasarım Modelinin Uygulanması"
slug: "flyweight-tasarm-modelinin-uygulanmas"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## RPG oyununda harita uygulama
Flyweight, yapısal tasarım modellerinden biridir. Benzer nesnelerle mümkün olduğunca fazla veri paylaşarak kullanılan bellek miktarını azaltmak için kullanılır. Bu belge size Flyweight DP'yi nasıl doğru şekilde kullanacağınızı öğretecektir.

Bunun fikrini size basit bir örnekle açıklayayım. Bir RPG oyunu üzerinde çalıştığınızı ve bazı karakterleri içeren büyük bir dosya yüklemeniz gerektiğini düşünün. Örneğin:

- `#` çimendir. Üzerinde yürüyebilirsin.
- `$` başlangıç ​​noktasıdır
- `@` kayadır. Üzerinde yürüyemezsin.
- `%` hazine sandığıdır

Harita örneği:

`@@@@@@@@@@@@@@@@@@@@@@@@@@@@@`

`@###########@@@@######@#$@@@`

`@#############@@######@###@@@`

`@#######%######@###########@@@`

`@##########################@`

`@@@@@@@@@@@@@@@@@@@@@@@@@@@@@`

Bu objeler benzer özelliklere sahip olduğu için her harita alanı için ayrı obje oluşturmanıza gerek yoktur. Size flyweight'i nasıl kullanacağınızı göstereceğim.

Alanlarımızın uygulayacağı bir interface tanımlayalım:

    public interface IField
    {
        string Name { get; }
        char Mark { get; }
        bool CanWalk { get; }
        FieldType Type { get; }
    }
Artık alanlarımızı temsil eden sınıflar oluşturabiliriz. Ayrıca onları bir şekilde tanımlamamız gerekiyor (bir numaralandırma kullandım):

    public enum FieldType
    {
        GRASS,
        ROCK,
        START,
        CHEST
    }
    public class Grass : IField
    {
        public string Name { get { return "Grass"; } }
        public char Mark { get { return '#'; } }
        public bool CanWalk { get { return true; } }
        public FieldType Type { get { return FieldType.GRASS; } }
    }
    public class StartingPoint : IField
    {
        public string Name { get { return "Starting Point"; } }
        public char Mark { get { return '$'; } }
        public bool CanWalk { get { return true; } }
        public FieldType Type { get { return FieldType.START; } }
    }
    public class Rock : IField
    {
        public string Name { get { return "Rock"; } }
        public char Mark { get { return '@'; } }
        public bool CanWalk { get { return false; } }
        public FieldType Type { get { return FieldType.ROCK; } }
    }
    public class TreasureChest : IField
    {
        public string Name { get { return "Treasure Chest"; } }
        public char Mark { get { return '%'; } }
        public bool CanWalk { get { return true; } } // you can approach it
        public FieldType Type { get { return FieldType.CHEST; } }
    }
    
Dediğim gibi her alan için ayrı bir instance oluşturmamıza gerek yok. Bir __repository__ alan oluşturmalıyız. Flyweight DP'nin özü, bir nesneyi yalnızca ihtiyacımız olduğunda ve henüz depomuzda yoksa dinamik olarak oluşturmamız veya zaten varsa geri döndürmemizdir. Bunu bizim için halledecek basit bir sınıf yazalım:

    public class FieldRepository
    {
        private List<IField> lstFields = new List<IField>();
 
        private IField AddField(FieldType type)
        {
            IField f;
            switch(type)
            {
                case FieldType.GRASS: f = new Grass(); break;
                case FieldType.ROCK: f = new Rock(); break;
                case FieldType.START: f = new StartingPoint(); break;
                case FieldType.CHEST:
                default: f = new TreasureChest(); break;
            }
            lstFields.Add(f); //add it to repository
            Console.WriteLine("Created new instance of {0}", f.Name);
            return f;
        }
        public IField GetField(FieldType type)
        {
            IField f = lstFields.Find(x => x.Type == type);
            if (f != null) return f;
            else return AddField(type);
        }
    }
Harika! Şimdi kodumuzu test edebiliriz:

    public class Program
    {
        public static void Main(string[] args)
        {
            FieldRepository f = new FieldRepository();
            IField grass = f.GetField(FieldType.GRASS);
            grass = f.GetField(FieldType.ROCK);
            grass = f.GetField(FieldType.GRASS);       
        }
    }
Konsoldaki sonuç şöyle olmalıdır:
> Yeni bir Grass örneği oluşturuldu
>
> Yeni bir Rock örneği yarattı

Ama iki kez almak istiyorsak neden çim sadece bir kez ortaya çıkıyor? Bunun nedeni, ilk kez `GetField` olarak adlandırdığımız çimen örneği __repository__'mizde mevcut değil, bu yüzden yaratıldı, ancak bir sonraki otu ihtiyacımız olduğunda zaten var, bu yüzden sadece onu iade ediyoruz.



