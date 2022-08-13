---
title: "ES de archivos y secuencias"
slug: "es-de-archivos-y-secuencias"
draft: false
images: []
weight: 9903
type: docs
toc: true
---

Administra archivos.

## Sintaxis
- `nuevo System.IO.StreamWriter (ruta de cadena)`
- `nuevo System.IO.StreamWriter (ruta de la cadena, anexo bool)`
- `System.IO.StreamWriter.WriteLine(cadena de texto)`
- `System.IO.StreamWriter.WriteAsync(cadena de texto)`
- `Sistema.IO.Stream.Close()`
- `System.IO.File.ReadAllText (ruta de cadena)`
- `System.IO.File.ReadAllLines(ruta de la cadena)`
- `System.IO.File.ReadLines(ruta de cadena)`
- `System.IO.File.WriteAllText(ruta de cadena, texto de cadena)`
- `System.IO.File.WriteAllLines(ruta de cadena, contenido de IEnumerable<cadena>)`
- `System.IO.File.Copy(fuente de cadena, destino de cadena)`
- `System.IO.File.Create (ruta de cadena)`
- `System.IO.File.Delete(ruta de cadena)`
- `System.IO.File.Move(fuente de cadena, destino de cadena)`
- `System.IO.Directory.GetFiles(ruta de cadena)`

## Parámetros
| Parámetro | Detalles |
| ------ | ------ |
| camino | La ubicación del archivo. |
| añadir | Si el archivo existe, true agregará datos al final del archivo (adjuntar), false sobrescribirá el archivo. |
| texto | Texto a escribir o almacenar. |
|contenidos | Una colección de cadenas para ser escritas. |
| fuente | La ubicación del archivo que desea utilizar. |
| destino | La ubicación a la que desea que vaya un archivo. |

- Siempre asegúrese de cerrar los objetos `Stream`. Esto se puede hacer con un bloque `using` como se muestra arriba o llamando manualmente a `myStream.Close()`.
- Asegúrese de que el usuario actual tenga los permisos necesarios en la ruta en la que intenta crear el archivo.
- [Cadenas textuales](https://www.wikiod.com/es/docs/c%23/16/verbatim-strings) deben usarse al declarar una cadena de ruta que incluye barras invertidas, así: `@"C:\MyFolder\MyFile .txt"`

## Lectura de un archivo usando la clase System.IO.File
Puede usar **[System.IO.File.ReadAllText](https://msdn.microsoft.com/en-us/library/system.io.file.readalltext(v=vs.110).aspx)* * función para leer todo el contenido de un archivo en una cadena.

    string text = System.IO.File.ReadAllText(@"C:\MyFolder\MyTextFile.txt");

También puede leer un archivo como una matriz de líneas usando **[System.IO.File.ReadAllLines](https://msdn.microsoft.com/en-us/library/system.io.file.readlines(v =vs.110).aspx)** función:
    
    string[] lines = System.IO.File.ReadAllLines(@"C:\MyFolder\MyTextFile.txt");

## Leyendo perezosamente un archivo línea por línea a través de un IEnumerable
Cuando trabaje con archivos grandes, puede usar el método `System.IO.File.ReadLines` para leer todas las líneas de un archivo en `IEnumerable<string>`. Esto es similar a `System.IO.File.ReadAllLines`, excepto que no carga todo el archivo en la memoria a la vez, lo que lo hace más eficiente cuando se trabaja con archivos grandes.

    IEnumerable<string> AllLines = File.ReadLines("file_name.txt", Encoding.Default);

_El segundo parámetro de File.ReadLines es opcional. Puede usarlo cuando sea necesario para especificar la codificación._

Es importante tener en cuenta que llamar a `ToArray`, `ToList` u otra función similar obligará a cargar todas las líneas a la vez, lo que significa que el beneficio de usar `ReadLines` queda anulado. Es mejor enumerar sobre `IEnumerable` usando un bucle `foreach` o LINQ si usa este método.


## Escribir líneas en un archivo usando la clase System.IO.StreamWriter
La clase **[System.IO.StreamWriter](https://msdn.microsoft.com/en-us/library/system.io.streamwriter(v=vs.110).aspx)**:
>Implementa un TextWriter para escribir caracteres en un flujo en una codificación particular.

Con el método `WriteLine`, puede escribir contenido línea por línea en un archivo.

Observe el uso de la palabra clave `using` que garantiza que el objeto StreamWriter se elimine tan pronto como quede fuera del alcance y, por lo tanto, el archivo se cierre.

    string[] lines = { "My first string", "My second string", "and even a third string" };
    using (System.IO.StreamWriter sw = new System.IO.StreamWriter(@"C:\MyFolder\OutputText.txt"))
    {
        foreach (string line in lines)
        {
            sw.WriteLine(line);
        }
    }

Tenga en cuenta que StreamWriter puede recibir un segundo parámetro `bool` en su constructor, lo que permite `Anexar` a un archivo en lugar de sobrescribir el archivo:

    bool appendExistingFile = true;
    using (System.IO.StreamWriter sw = new System.IO.StreamWriter(@"C:\MyFolder\OutputText.txt", appendExistingFile ))
    {
        sw.WriteLine("This line will be appended to the existing file");
    }

## Escribiendo en un archivo usando la clase System.IO.File
Puede usar **[System.IO.File.WriteAllText](https://msdn.microsoft.com/en-us/library/system.io.file.writealltext(v=vs.110).aspx)* * función para escribir una cadena en un archivo.

    string text = "String that will be stored in the file";
    System.IO.File.WriteAllText(@"C:\MyFolder\OutputFile.txt", text);

También puede usar **[System.IO.File.WriteAllLines](https://msdn.microsoft.com/en-us/library/system.io.file.writealllines(v=vs.110).aspx) ** función que recibe un `IEnumerable<String>` como segundo parámetro (a diferencia de una sola cadena en el ejemplo anterior). Esto le permite escribir contenido a partir de una matriz de líneas.

    string[] lines = { "My first string", "My second string", "and even a third string" };
    System.IO.File.WriteAllLines(@"C:\MyFolder\OutputFile.txt", lines);

## Copiar archivo
**Archivo de clase estática**

La clase estática `File` se puede usar fácilmente para este propósito.

    File.Copy(@"sourcePath\abc.txt", @"destinationPath\abc.txt");
    File.Copy(@"sourcePath\abc.txt", @"destinationPath\xyz.txt");

**Observación:** Con este método, el archivo se copia, lo que significa que se leerá desde el origen y luego se escribirá en la ruta de destino. Este es un proceso que consume recursos, tomaría un tiempo relativo al tamaño del archivo y puede hacer que su programa se congele si no utiliza subprocesos.

## Escritura asíncrona de texto en un archivo usando StreamWriter


## Crea un archivo
**Archivo de clase estática**

Al usar el método `Crear` de la clase estática `Archivo` podemos crear archivos. Method crea el archivo en la ruta dada, al mismo tiempo abre el archivo y nos da el `FileStream` del archivo. Asegúrese de cerrar el archivo una vez que haya terminado con él.

ex1:

    var fileStream1 = File.Create("samplePath");
    /// you can write to the fileStream1
    fileStream1.Close();

ex2:

    using(var fileStream1 = File.Create("samplePath"))
    {
        /// you can write to the fileStream1
    }

ex3:

    File.Create("samplePath").Close();

**Clase FileStream**

Hay muchas sobrecargas de este constructor de clases que en realidad está bien documentado [aquí][1]. El siguiente ejemplo es para el que cubre las funcionalidades más utilizadas de esta clase.

    var fileStream2 = new FileStream("samplePath", FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None);

Puede verificar las enumeraciones para [FileMode][2], [FileAccess][3] y [FileShare][4] desde esos enlaces. Lo que básicamente significan es lo siguiente:

*FileMode:* responde "¿Debería crearse un archivo? ¿Abrirse? ¿Crearse si no existe y luego abrirse?" tipo de preguntas

*FileAccess:* responde "¿Debería poder leer el archivo, escribir en el archivo o ambos?" tipo de preguntas

*FileShare:* responde "¿Deberían otros usuarios poder leer, escribir, etc. en el archivo mientras lo estoy usando simultáneamente?" tipo de preguntas


[1]: https://msdn.microsoft.com/en-us/library/system.io.filestream(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.io.filemode(v=vs.110).aspx
[3]: https://msdn.microsoft.com/en-us/library/4z36sx0f(v=vs.110).aspx
[4]: https://msdn.microsoft.com/en-us/library/system.io.fileshare(v=vs.110).aspx

## Mover archivo
**Archivo de clase estática**

La clase estática de archivo se puede usar fácilmente para este propósito.

    File.Move(@"sourcePath\abc.txt", @"destinationPath\xyz.txt");

**Observación1:** Solo cambia el índice del archivo (si el archivo se mueve en el mismo volumen). Esta operación no lleva tiempo relativo al tamaño del archivo.

**Observación 2:** No se puede anular un archivo existente en la ruta de destino.

## Borrar archivo
    string path = @"c:\path\to\file.txt";
    File.Delete(path);

Si bien `Delete` no arroja una excepción si el archivo no existe, arrojará una excepción, p. si la ruta especificada no es válida o la persona que llama no tiene los permisos necesarios. Siempre debe envolver las llamadas a `Delete` dentro del [bloque try-catch][1] y manejar todas las excepciones esperadas. En caso de posibles condiciones de carrera, ajuste la lógica dentro de [instrucción de bloqueo][2].


[1]: https://www.wikiod.com/es/docs/c%23/26/keywords/148/try-catch-finally-throw#t=201608021340222162938
[2]: https://www.wikiod.com/es/docs/c%23/1495/lock-statement/4865/simple-usage#t=201608021343504970522

## Archivos y Directorios
**Obtener todos los archivos en el Directorio**
    
     var FileSearchRes = Directory.GetFiles(@Path, "*.*", SearchOption.AllDirectories);

Devuelve una matriz de `FileInfo`, que representa todos los archivos en el directorio especificado.

**Obtener archivos con extensión específica**

     var FileSearchRes = Directory.GetFiles(@Path, "*.pdf", SearchOption.AllDirectories);

Devuelve una matriz de `FileInfo`, que representa todos los archivos en el directorio especificado con la extensión especificada.

