---
title: "System.IO.File sınıfı"
slug: "systemiofile-snf"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Sözdizimi
- dize kaynağı;
- dize hedefi;

## Parametreler
| parametre | Ayrıntılar |
| ---------------- | ------- |
| 'kaynak' | Başka bir konuma taşınacak dosya. |
| 'hedef' | 'Kaynak' öğesini taşımak istediğiniz dizin (bu değişken ayrıca dosyanın adını (ve dosya uzantısını) da içermelidir.

## Bir dosyayı sil
Bir dosyayı silmek (gerekli izinlere sahipseniz) şu kadar basittir:

    File.Delete(path);

Ancak birçok şey ters gidebilir:

* Gerekli izinlere sahip değilsiniz (`UnauthorizedAccessException` atılır).
* Dosya başka biri tarafından kullanılıyor olabilir (`IOException` atılır).
* Düşük seviyeli hata veya medya salt okunur olduğundan (`IOException` atılır) dosya silinemez.
* Dosya artık mevcut değil (`IOException` atılır).

Son noktanın (dosya mevcut değil) genellikle bunun gibi bir kod parçacığıyla _circumvented_ olduğunu unutmayın:

    if (File.Exists(path))
        File.Delete(path);

Ancak bu atomik bir işlem değildir ve dosya `File.Exists()` çağrısı ile `File.Delete()` çağrısı arasında başka biri tarafından silinebilir. G/Ç işlemini ele almak için doğru yaklaşım, istisna işlemeyi gerektirir (işlem başarısız olduğunda alternatif bir eylem planının yapılabileceğini varsayarsak):

    if (File.Exists(path))
    {
        try
        {
            File.Delete(path);
        }
        catch (IOException exception)
        {
            if (!File.Exists(path))
                return; // Someone else deleted this file
    
            // Something went wrong...
        }
        catch (UnauthorizedAccessException exception)
        {
            // I do not have required permissions
        }
    }

Bu G/Ç hatalarının bazen geçici olduğunu (örneğin kullanımdaki dosya) ve bir ağ bağlantısı söz konusuysa, bizim tarafımızdan herhangi bir işlem yapmadan otomatik olarak düzelebileceğini unutmayın. Daha sonra, her deneme arasında küçük bir gecikmeyle bir G/Ç işlemini birkaç kez _retry_ yapmak yaygındır:

    public static void Delete(string path)
    {
        if (!File.Exists(path))
            return;
    
        for (int i=1; ; ++i)
        {
            try
            {
                File.Delete(path);
                return;
            }
            catch (IOException e)
            {
                if (!File.Exists(path))
                    return;
    
                if (i == NumberOfAttempts)
                    throw;
    
                Thread.Sleep(DelayBetweenEachAttempt);
            }
    
            // You may handle UnauthorizedAccessException but this issue
            // will probably won't be fixed in few seconds...
        }
    }
    
    private const int NumberOfAttempts = 3;
    private const int DelayBetweenEachAttempt = 1000; // ms

Not: Windows ortamında, bu işlevi çağırdığınızda dosya gerçekten silinmeyecektir, eğer bir başkası dosyayı `FileShare.Delete' kullanarak açarsa dosya silinebilir, ancak bu sadece dosya sahibi dosyayı kapattığında etkili olur.

## Bir metin dosyasından istenmeyen satırları çıkarın
Bir metin dosyasını değiştirmek kolay değildir çünkü içeriğinin hareket ettirilmesi gerekir. _small_ dosyaları için en kolay yöntem, içeriğini bellekte okumak ve sonra değiştirilmiş metni geri yazmaktır.

Bu örnekte, bir dosyadaki tüm satırları okuduk ve tüm boş satırları bıraktıktan sonra orijinal yola geri yazıyoruz:

    File.WriteAllLines(path,
        File.ReadAllLines(path).Where(x => !String.IsNullOrWhiteSpace(x)));

Dosya belleğe yüklenemeyecek kadar büyükse ve çıkış yolu giriş yolundan farklıysa:

    File.WriteAllLines(outputPath,
        File.ReadLines(inputPath).Where(x => !String.IsNullOrWhiteSpace(x)));
    

## Metin dosyası kodlamasını dönüştürün
Metin kodlanmış olarak kaydedilir (ayrıca [Dizeler][1] konusuna bakın), bazen kodlamasını değiştirmeniz gerekebilir, bu örnek (basitlik için) dosyanın çok büyük olmadığını ve tamamen bellekte okunabileceğini varsayar:

    public static void ConvertEncoding(string path, Encoding from, Encoding to)
    {
        File.WriteAllText(path, File.ReadAllText(path, from), to);
    }

Dönüştürme gerçekleştirirken dosyanın BOM (Byte Order Mark) içerebileceğini unutmayın, nasıl yönetildiğini daha iyi anlamak için [Encoding.UTF8.GetString Önsöz/BOM'u [2] dikkate almaz] bölümüne bakın.


[1]: https://www.wikiod.com/tr/dotnet/teller
[2]: http://stackoverflow.com/q/11701341/1207195

## Belirtilen miktardan daha eski dosyaları numaralandır
Bu snippet, belirli bir yaştan daha eski tüm dosyaları numaralandırmak için yardımcı bir işlevdir; örneğin, eski günlük dosyalarını veya eski önbelleğe alınmış verileri silmeniz gerektiğinde yararlıdır.

    static IEnumerable<string> EnumerateAllFilesOlderThan(
                                   TimeSpan maximumAge,
                                   string path,
                                   string searchPattern = "*.*",
                                   SearchOption options = SearchOption.TopDirectoryOnly)
    {
        DateTime oldestWriteTime = DateTime.Now - maximumAge;

        return Directory.EnumerateFiles(path, searchPattern, options)
            .Where(x => Directory.GetLastWriteTime(x) < oldestWriteTime);
    }

Bu şekilde kullanılır:

    var oldFiles = EnumerateAllFilesOlderThan(TimeSpan.FromDays(7), @"c:\log", "*.log");

Dikkat edilmesi gereken birkaç şey:

* Arama, `Directory.GetFiles()` yerine `Directory.EnumerateFiles()` kullanılarak yapılır. Numaralandırma _alive_ ise, tüm dosya sistemi girişleri getirilene kadar beklemeniz gerekmez.
* Son yazma zamanını kontrol ediyoruz ancak oluşturma zamanını veya son erişim zamanını kullanabilirsiniz (örneğin, _kullanılmayan_ önbelleğe alınmış dosyaları silmek için, erişim zamanının devre dışı bırakılabileceğini unutmayın).
* Tüm bu özellikler (yazma süresi, erişim süresi, oluşturma süresi) için ayrıntı düzeyi tek tip değildir, bununla ilgili ayrıntılar için MSDN'ye bakın.


## Dosyayı bir konumdan diğerine taşıma
<h1>Dosya.Taşı</h1>
Bir dosyayı bir konumdan diğerine taşımak için basit bir kod satırı şunu başarabilir:

`File.Move(@"C:\TemporaryFile.txt", @"C:\TemporaryFiles\TemporaryFile.txt");`

Ancak, bu basit operasyonda yanlış gidebilecek birçok şey var. Örneğin, programınızı çalıştıran kullanıcının 'C' etiketli bir Sürücüsü yoksa ne olur? Ya yaptılarsa - ama adını 'B' veya 'M' olarak değiştirmeye karar verdilerse?

Ya Kaynak dosya (içine taşımak istediğiniz dosya) sizin haberiniz olmadan taşınmışsa - ya da sadece mevcut değilse.

Bu, ilk önce kaynak dosyanın var olup olmadığını kontrol ederek aşılabilir:

    string source = @"C:\TemporaryFile.txt", destination = @"C:\TemporaryFiles\TemporaryFile.txt";
    if(File.Exists("C:\TemporaryFile.txt"))
    {
        File.Move(source, destination);
    }

Bu, o anda dosyanın var olmasını ve başka bir konuma taşınabilmesini sağlayacaktır. `File.Exists` için basit bir çağrının yeterli olmayacağı zamanlar olabilir. Değilse, tekrar kontrol edin, kullanıcıya işlemin başarısız olduğunu iletin - veya istisnayı ele alın.

Karşılaşabileceğiniz tek istisna bir "FileNotFoundException" değildir.

Olası istisnalar için aşağıya bakın:

| İstisna Türü | Açıklama |
| ------ | ------ |
| 'IOİstisna' | Dosya zaten var veya kaynak dosya bulunamadı.
| `ArgumentNullException`| Kaynak ve/veya Hedef parametrelerinin değeri boş. |
| 'ArgumentException' | Source ve/veya Destination parametrelerinin değeri boş veya geçersiz karakterler içeriyor. |
| 'YetkisizErişim Özel Durumu' | Bu eylemi gerçekleştirmek için gerekli izinlere sahip değilsiniz. |
| `PathTooLongException` | Kaynak, Hedef veya belirtilen yol(lar) maksimum uzunluğu aşıyor. Windows'ta bir Yolun uzunluğu 248 karakterden az, Dosya adları ise 260 karakterden az olmalıdır.
| `DirectoryNotFoundException` | Belirtilen dizin bulunamadı. |
| `NotSupportedException` | Kaynak veya Hedef yolları veya dosya adları geçersiz bir biçimde.

## Çok sayıda dosyaya "dokunun" (son yazma zamanını güncellemek için)
Bu örnek, çok sayıda dosyanın son yazma zamanını günceller ("System.IO.Directory.GetFiles()" yerine "System.IO.Directory.EnumerateFiles" kullanarak). İsteğe bağlı olarak bir arama modeli belirtebilirsiniz (varsayılan `"*.*"` şeklindedir ve sonunda bir dizin ağacında arama yapabilirsiniz (yalnızca belirtilen dizinde değil):

    public static void Touch(string path,
                             string searchPattern = "*.*",
                             SearchOptions options = SearchOptions.None)
    {
        var now = DateTime.Now;
    
        foreach (var filePath in Directory.EnumerateFiles(path, searchPattern, options))
        {
            File.SetLastWriteTime(filePath, now);
        }
    }

