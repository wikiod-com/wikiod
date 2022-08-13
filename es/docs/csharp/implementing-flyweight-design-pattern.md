---
title: "Implementación del patrón de diseño Flyweight"
slug: "implementacion-del-patron-de-diseno-flyweight"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Implementando el mapa en el juego RPG
Flyweight es uno de los patrones de diseño estructural. Se utiliza para disminuir la cantidad de memoria utilizada compartiendo tantos datos como sea posible con objetos similares. Este documento le enseñará cómo usar Flyweight DP correctamente.

Déjame explicarte la idea de esto con un ejemplo simple. Imagina que estás trabajando en un juego de rol y necesitas cargar un archivo enorme que contiene algunos personajes. Por ejemplo:

- `#` es hierba. Puedes caminar sobre él.
- `$` es el punto de partida
- `@` es roca. No puedes caminar sobre él.
- `%` es el cofre del tesoro

Ejemplo de un mapa:

`@@@@@@@@@@@@@@@@@@@@@@@@@@@@@`

`@############@@@@@######@#$@@@`

`@############@@@######@###@@@`

`@#######%######@###########@@@`

`@##########################@`

`@@@@@@@@@@@@@@@@@@@@@@@@@@@@@`

Dado que esos objetos tienen características similares, no necesita crear objetos separados para cada campo del mapa. Te mostraré cómo usar el peso mosca.

Definamos una interfaz que implementarán nuestros campos:

    public interface IField
    {
        string Name { get; }
        char Mark { get; }
        bool CanWalk { get; }
        FieldType Type { get; }
    }
Ahora podemos crear clases que representen nuestros campos. También tenemos que identificarlos de alguna manera (utilicé una enumeración):

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
    
Como dije, no necesitamos crear instancias separadas para cada campo. Tenemos que crear un __repositorio__ de campos. La esencia de Flyweight DP es que creamos dinámicamente un objeto solo si lo necesitamos y aún no existe en nuestro repositorio, o lo devolvemos si ya existe. Escribamos una clase simple que manejará esto por nosotros:

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
¡Excelente! Ahora podemos probar nuestro código:

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
El resultado en la consola debería ser:
> Creé una nueva instancia de Grass
>
> Creé una nueva instancia de Rock

Pero, ¿por qué la hierba aparece solo una vez si queremos obtenerla dos veces? Esto se debe a que la primera vez que llamamos a `GetField`, la instancia de grass no existe en nuestro __repository__, por lo que se crea, pero la próxima vez que necesitamos grass ya existe, por lo que solo la devolvemos.



