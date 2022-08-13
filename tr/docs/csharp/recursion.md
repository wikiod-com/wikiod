---
title: "özyineleme"
slug: "ozyineleme"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

Her özyinelemeli işlev çağrısı yığına ekleneceğinden özyineleme kullanmanın kodunuz üzerinde ciddi bir etkisi olabileceğini unutmayın. Çok fazla çağrı varsa, bu bir **StackOverflow**İstisnasına yol açabilir. Çoğu "doğal özyinelemeli işlev" "for", "while" veya "foreach" döngü yapısı olarak yazılabilir ve öyle görünmese de **şık** veya **akıllı** daha verimli olacaktır.

Her zaman iki kez düşünün ve özyinelemeyi dikkatli kullanın - neden kullandığınızı bilin:

- yinelemeli aramaların sayısının *aşırı* olmadığını bildiğinizde özyineleme kullanılmalıdır
- * aşırı *, ne kadar belleğin kullanılabilir olduğuna bağlıdır
- Özyineleme daha net ve daha temiz kod sürümü olduğu için kullanılır, yinelemeli veya döngü tabanlı bir işlevden daha okunabilir. Genellikle durum böyledir, çünkü daha temiz ve daha kompakt kod (aka daha az kod satırları) verir.
- ama farkında olun, daha az verimli olabilir! Örneğin Fibonacci özyinelemesinde, dizideki *nth* sayısını hesaplamak için hesaplama süresi katlanarak büyüyecektir!

Daha fazla teori istiyorsanız, lütfen okuyun:
- https://www.cs.umd.edu/class/fall2002/cmsc214/Tutorial/recursion2.html
- https://en.wikipedia.org/wiki/Recursion#In_computer_science


## sade İngilizce'de özyineleme
Özyineleme şu şekilde tanımlanabilir:

> Belirli bir koşul karşılanana kadar kendini çağıran bir yöntem.

Mükemmel ve basit bir özyineleme örneği, belirli bir sayının faktöriyelini alacak bir yöntemdir:

    public int Factorial(int number)
    {
        return number == 0 ? 1 : n * Factorial(number - 1);
    }

Bu yöntemde, yöntemin bir 'sayı' argümanı alacağını görebiliriz.

Adım adım:

Örnek göz önüne alındığında, `faktöriyel (4)` `

1. 'sayı (4) == 1' mi?
2. Hayır? `4 * Faktöriyel (numara-1)` (3)
3. Yöntem bir kez daha çağrıldığı için, yeni argüman olarak `Factory(3)` kullanarak ilk adımı tekrarlar.
4. Bu, `` faktöriyel (1) 'yürütülene ve `sayı (1) == 1' döndürünceye kadar devam eder.
5. Genel olarak, hesaplama "4 * 3 * 2 * 1` hesaplaması ve son olarak 24 döner.

Özyinelemeyi anlamanın anahtarı, yöntemin kendisinin *yeni bir örneğini* çağırmasıdır. Döndükten sonra, çağıran örneğin yürütülmesi devam eder.

## Fibonacci Dizisi
Özyinelemeyi kullanarak Fibonacci dizisindeki bir sayıyı hesaplayabilirsiniz.

Herhangi bir i > 0 için F(n) = F(n-2) + F(n-1) matematik teorisini izleyerek,

    // Returns the i'th Fibonacci number
    public int fib(int i) {
        if(i <= 2) {
            // Base case of the recursive function.
            // i is either 1 or 2, whose associated Fibonacci sequence numbers are 1 and 1.
            return 1;
        }
        // Recursive case. Return the sum of the two previous Fibonacci numbers.
        // This works because the definition of the Fibonacci sequence specifies
        // that the sum of two adjacent elements equals the next element.
        return  fib(i - 2) + fib(i - 1);
        
    }

    fib(10); // Returns 55

## Bir nesne yapısını yinelemeli olarak tanımlayın


## Dizin Ağacı Almak için Özyinelemeyi Kullanma
Özyinelemenin kullanımlarından biri, ağacın kaç düzeyi olduğunu veya her düzeydeki nesne sayısını bilmeden, bir dosya sistemi dizin ağacı gibi hiyerarşik bir veri yapısında gezinmektir. Bu örnekte, belirli bir dizinin tüm alt dizinlerini bulmak ve tüm ağacı konsola yazdırmak için bir dizin ağacında özyinelemenin nasıl kullanılacağını göreceksiniz.

    internal class Program
    {
        internal const int RootLevel = 0;
        internal const char Tab = '\t';

        internal static void Main()
        {
            Console.WriteLine("Enter the path of the root directory:");
            var rootDirectorypath = Console.ReadLine();

            Console.WriteLine(
                $"Getting directory tree of '{rootDirectorypath}'");

            PrintDirectoryTree(rootDirectorypath);
            Console.WriteLine("Press 'Enter' to quit...");
            Console.ReadLine();
        }

        internal static void PrintDirectoryTree(string rootDirectoryPath)
        {
            try
            {
                if (!Directory.Exists(rootDirectoryPath))
                {
                    throw new DirectoryNotFoundException(
                        $"Directory '{rootDirectoryPath}' not found.");
                }

                var rootDirectory = new DirectoryInfo(rootDirectoryPath);
                PrintDirectoryTree(rootDirectory, RootLevel);
            }
            catch (DirectoryNotFoundException e)
            {
                Console.WriteLine(e.Message);
            }
        }

        private static void PrintDirectoryTree(
            DirectoryInfo directory, int currentLevel)
        {
            var indentation = string.Empty;
            for (var i = RootLevel; i < currentLevel; i++)
            {
                indentation += Tab;
            }

            Console.WriteLine($"{indentation}-{directory.Name}");
            var nextLevel = currentLevel + 1;
            try
            {
                foreach (var subDirectory in directory.GetDirectories())
                {
                    PrintDirectoryTree(subDirectory, nextLevel);
                }
            }
            catch (UnauthorizedAccessException e)
            {
                Console.WriteLine($"{indentation}-{e.Message}");
            }
        }
    }

Bu kod, bu görevi tamamlamak için çıplak minimumdan biraz daha karmaşıktır, çünkü dizinleri almakla ilgili herhangi bir sorunla ilgilenmek için istisna kontrolü içerir. Aşağıda, kodun her birinin açıklamaları ile daha küçük segmentlere ayrılacaksınız.

"Ana":

Ana yöntem, bir kullanıcıdan kök dizine giden yol olarak kullanılacak bir dize olarak bir girdi alır. Daha sonra parametre olarak bu dizeyle `PrintDirectoryTree` yöntemini çağırır.

`PrintDirectoryTree(dize)`:

Bu, gerçek dizin ağacı yazdırmayı işleyen iki yöntemden ilkidir. Bu yöntem, parametre olarak kök dizine giden yolu temsil eden bir dize alır. Yolun gerçek bir dizin olup olmadığını kontrol eder ve değilse, daha sonra catch bloğunda işlenen bir 'DirectoryNotFoundException' atar. Yol gerçek bir dizin ise, yoldan bir "DirectoryInfo" nesnesi "rootDirectory" oluşturulur ve ikinci "PrintDirectoryTree" yöntemi, "rootDirectory" nesnesi ve bir değer içeren bir tamsayı sabiti olan "RootLevel" ile çağrılır. sıfır.

`PrintDirectoryTree(DirectoryInfo, int)`:

Bu ikinci yöntem, işin ağırlığını ele alır. Parametre olarak bir "DirectoryInfo" ve bir tamsayı alır. 'DirectoryInfo' geçerli dizindir ve tam sayı, dizinin köke göre derinliğidir. Okuma kolaylığı için, çıktı, geçerli dizinin derinliklerindeki her seviye için girintilidir, böylece çıktı şöyle görünür:

    -Root
        -Child 1
        -Child 2
            -Grandchild 2.1
        -Child 3

Geçerli dizin yazdırıldıktan sonra, alt dizinleri alınır ve bu yöntem daha sonra her biri için geçerli olandan bir fazla derinlik düzeyi değeriyle çağrılır. Bu kısım özyinelemedir: kendini çağıran yöntem. Program, ağaçtaki her dizini ziyaret edene kadar bu şekilde çalışacaktır. Alt dizinleri olmayan bir dizine ulaştığında, yöntem otomatik olarak geri dönecektir.

Bu yöntem ayrıca, geçerli dizinin alt dizinlerinden herhangi biri sistem tarafından korunuyorsa oluşturulan bir 'UnauthorizedAccessException'ı da yakalar. Hata mesajı, tutarlılık için geçerli girinti düzeyinde yazdırılır.

Aşağıdaki yöntem, bu soruna daha temel bir yaklaşım sağlar:

    internal static void PrintDirectoryTree(string directoryName)
    {
        try
        {
            if (!Directory.Exists(directoryName)) return;
            Console.WriteLine(directoryName);
            foreach (var d in Directory.GetDirectories(directoryName))
            {
                PrintDirectoryTree(d);
            }
        }
        catch (Exception e)
        {
            Console.WriteLine(e.Message);
        }
    }

Bu, ilk yaklaşımın belirli hata kontrolünü veya çıktı biçimlendirmesini içermez, ancak aynı şeyi etkili bir şekilde yapar. 'DirectoryInfo' yerine yalnızca dizeleri kullandığından, izinler gibi diğer dizin özelliklerine erişim sağlayamaz.

## Powerof Hesaplama
Belirli bir sayının gücünün hesaplanması da özyinelemeli olarak yapılabilir.
Bir taban numarası `n` ve üs` `e` göz önüne alındığında, üssünü azaltarak sorunu parçalara böldüğünden emin olmalıyız.

Teorik Örnek:

- 2² = 2x2
- 2³ = 2x2x2
veya, 2³ = 2² x 2<br/>özyinelemeli algoritmamızın sırrı burada yatıyor (aşağıdaki koda bakın). Bu, sorunu almak ve onu daha küçük ve çözülmesi daha basit parçalara ayırmakla ilgilidir.
- **Notlar**
- Temel numara 0 olduğunda, 0 0 = 0 x 0 x 0 olarak dönmenin farkında olmalıyız
- üs 0 olduğunda, bu matematiksel bir kural olduğu için her zaman 1 döndürmenin farkında olmalıyız.

Kod Örneği:

    public int CalcPowerOf(int b, int e) {
        if (b == 0) { return 0; } // when base is 0, it doesn't matter, it will always return 0
        if (e == 0) { return 1; } // math rule, exponent 0 always returns 1
        return b * CalcPowerOf(b, e - 1); // actual recursive logic, where we split the problem, aka: 2³ = 2 * 2² etc..
    }

Mantığı doğrulamak için xUnit'te testler:<br/>
Bu gerekli olmasa da, mantığınızı doğrulamak için testler yazmak her zaman iyidir. Burada [Xunit çerçevesi] [1] 'e yazılmış olanları dahil ediyorum.

        [Theory]
        [MemberData(nameof(PowerOfTestData))]
        public void PowerOfTest(int @base, int exponent, int expected) {
            Assert.Equal(expected, CalcPowerOf(@base, exponent));
        }

        public static IEnumerable<object[]> PowerOfTestData() {
            yield return new object[] { 0, 0, 0 };
            yield return new object[] { 0, 1, 0 };
            yield return new object[] { 2, 0, 1 };
            yield return new object[] { 2, 1, 2 };
            yield return new object[] { 2, 2, 4 };
            yield return new object[] { 5, 2, 25 };
            yield return new object[] { 5, 3, 125 };
            yield return new object[] { 5, 4, 625 };
    }


[1]: https://xunit.github.io/

## Faktöriyel hesaplama
Bir sayının faktöriyeli (! ile gösterilir, örneğin 9!), bu sayının bir düşük faktöriyel ile çarpımıdır. Örneğin, 9! = 9x8! = 9x8x7! = 9 x 8 x 7 x 6 x 5 x 4 x 3 x 2 x 1.

Yani, özyinelemeyi kullanarak kod olarak:

    long Factorial(long x)
    {
        if (x < 1)
        {
            throw new OutOfRangeException("Factorial can only be used with positive numbers.");
        }
    
        if (x == 1)
        {
            return 1;
        } else {
            return x * Factorial(x - 1);
        }
    }



