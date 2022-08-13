---
title: "Clase System.IO.File"
slug: "clase-systemiofile"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Sintaxis
- fuente de cadena;
- cadena de destino;

## Parámetros
| Parámetro | Detalles |
| ---------------- | ------- |
| `fuente` | El archivo que se va a mover a otra ubicación. |
| `destino` | El directorio al que le gustaría mover la `fuente` (esta variable también debe contener el nombre (y la extensión del archivo) del archivo. |

## Eliminar un archivo
Eliminar un archivo (si tiene los permisos requeridos) es tan simple como:

    File.Delete(path);

Sin embargo, muchas cosas pueden salir mal:

* No tiene los permisos requeridos (se lanza `UnauthorizedAccessException`).
* El archivo puede estar en uso por otra persona (se lanza `IOException`).
* El archivo no se puede eliminar debido a un error de bajo nivel o el medio es de solo lectura (se lanza una `IOException`).
* El archivo ya no existe (se lanza `IOException`).

Tenga en cuenta que el último punto (el archivo no existe) generalmente se _elude_ con un fragmento de código como este:

    if (File.Exists(path))
        File.Delete(path);

Sin embargo, no es una operación atómica y el archivo puede ser eliminado por otra persona entre la llamada a `File.Exists()` y antes de `File.Delete()`. El enfoque correcto para manejar la operación de E/S requiere el manejo de excepciones (suponiendo que se puede tomar un curso de acción alternativo cuando falla la operación):

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

Tenga en cuenta que estos errores de E/S a veces son transitorios (archivo en uso, por ejemplo) y si hay una conexión de red involucrada, puede recuperarse automáticamente sin ninguna acción de nuestra parte. Entonces es común _reintentar_ una operación de E/S varias veces con un pequeño retraso entre cada intento:

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

Nota: en el entorno de Windows, el archivo no se eliminará realmente cuando llame a esta función, si alguien más abre el archivo usando `FileShare.Delete`, entonces el archivo se puede eliminar, pero solo sucederá efectivamente cuando el propietario cierre el archivo.

## Elimina las líneas no deseadas de un archivo de texto
Cambiar un archivo de texto no es fácil porque su contenido debe moverse. Para archivos _pequeños_, el método más fácil es leer su contenido en la memoria y luego volver a escribir el texto modificado.

En este ejemplo, leemos todas las líneas de un archivo y eliminamos todas las líneas en blanco, luego volvemos a escribir en la ruta original:

    File.WriteAllLines(path,
        File.ReadAllLines(path).Where(x => !String.IsNullOrWhiteSpace(x)));

Si el archivo es demasiado grande para cargarlo en la memoria y la ruta de salida es diferente de la ruta de entrada:

    File.WriteAllLines(outputPath,
        File.ReadLines(inputPath).Where(x => !String.IsNullOrWhiteSpace(x)));
    

## Convertir codificación de archivos de texto
El texto se guarda codificado (consulte también el tema [Strings][1]) y, a veces, es posible que deba cambiar su codificación; este ejemplo asume (para simplificar) que el archivo no es demasiado grande y se puede leer por completo en la memoria:

    public static void ConvertEncoding(string path, Encoding from, Encoding to)
    {
        File.WriteAllText(path, File.ReadAllText(path, from), to);
    }

Al realizar conversiones, no olvide que el archivo puede contener BOM (marca de orden de bytes), para comprender mejor cómo se administra, consulte [Encoding.UTF8.GetString no tiene en cuenta el preámbulo/BOM][2].


[1]: https://www.wikiod.com/es/dotnet/instrumentos-de-cuerda
[2]: http://stackoverflow.com/q/11701341/1207195

## Enumerar archivos anteriores a una cantidad especificada
Este fragmento es una función de ayuda para enumerar todos los archivos anteriores a una edad específica, es útil, por ejemplo, cuando tiene que eliminar archivos de registro antiguos o datos almacenados en caché antiguos.

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

Usado así:

    var oldFiles = EnumerateAllFilesOlderThan(TimeSpan.FromDays(7), @"c:\log", "*.log");

Algunas cosas a tener en cuenta:

* La búsqueda se realiza utilizando `Directory.EnumerateFiles()` en lugar de `Directory.GetFiles()`. La enumeración está _viva_, entonces no necesitará esperar hasta que se hayan obtenido todas las entradas del sistema de archivos.
* Estamos comprobando la hora de la última escritura, pero puede usar la hora de creación o la hora del último acceso (por ejemplo, para eliminar archivos almacenados en caché _no utilizados_, tenga en cuenta que la hora de acceso puede estar deshabilitada).
* La granularidad no es uniforme para todas esas propiedades (tiempo de escritura, tiempo de acceso, tiempo de creación), consulte MSDN para obtener detalles al respecto.


## Mover un archivo de una ubicación a otra
<h1>Archivo.Mover</h1>
Para mover un archivo de una ubicación a otra, una simple línea de código puede lograr esto:

`File.Move(@"C:\TemporaryFile.txt", @"C:\TemporaryFiles\TemporaryFile.txt");`

Sin embargo, hay muchas cosas que podrían salir mal con esta simple operación. Por ejemplo, ¿qué sucede si el usuario que ejecuta su programa no tiene una unidad con la etiqueta 'C'? ¿Qué pasa si lo hicieron, pero decidieron cambiarle el nombre a 'B' o 'M'?

¿Qué sucede si el archivo de origen (el archivo en el que le gustaría mover) se ha movido sin su conocimiento, o qué sucede si simplemente no existe?

Esto se puede eludir comprobando primero si el archivo fuente existe:

    string source = @"C:\TemporaryFile.txt", destination = @"C:\TemporaryFiles\TemporaryFile.txt";
    if(File.Exists("C:\TemporaryFile.txt"))
    {
        File.Move(source, destination);
    }

Esto garantizará que, en ese mismo momento, el archivo exista y se pueda mover a otra ubicación. Puede haber ocasiones en las que una simple llamada a `File.Exists` no sea suficiente. Si no es así, verifique nuevamente, transmita al usuario que la operación falló o maneje la excepción.

Una `FileNotFoundException` no es la única excepción que probablemente encuentre.

Consulte a continuación las posibles excepciones:

| Tipo de excepción | Descripción |
| ------ | ------ |
| `ExcepciónIO` | El archivo ya existe o no se pudo encontrar el archivo de origen.
| `ArgumentNullException`| El valor de los parámetros Origen y/o Destino es nulo. |
| `ArgumentoExcepción` | El valor de los parámetros Origen y/o Destino está vacío o contiene caracteres no válidos. |
| `Excepción de acceso no autorizado` | No tiene los permisos necesarios para realizar esta acción. |
| `Excepción PathTooLong` | El origen, el destino o las rutas especificadas superan la longitud máxima. En Windows, la longitud de una ruta debe tener menos de 248 caracteres, mientras que los nombres de archivo deben tener menos de 260 caracteres.
| `DirectoryNotFoundException` | No se pudo encontrar el directorio especificado. |
| `Excepción no admitida` | Las rutas de origen o destino o los nombres de archivo tienen un formato no válido.

## "Toca" una gran cantidad de archivos (para actualizar la última hora de escritura)
Este ejemplo actualiza la última hora de escritura de una gran cantidad de archivos (usando `System.IO.Directory.EnumerateFiles` en lugar de `System.IO.Directory.GetFiles()`). Opcionalmente, puede especificar un patrón de búsqueda (el valor predeterminado es `"*.*"` y eventualmente buscar a través de un árbol de directorios (no solo el directorio especificado):

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

