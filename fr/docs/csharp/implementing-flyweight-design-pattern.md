---
title: "Implémentation du modèle de conception poids mouche"
slug: "implementation-du-modele-de-conception-poids-mouche"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Implémentation de la carte dans le jeu RPG
Le poids mouche est l'un des modèles de conception structurelle. Il est utilisé pour diminuer la quantité de mémoire utilisée en partageant autant de données que possible avec des objets similaires. Ce document vous apprendra comment utiliser correctement Flyweight DP.

Laissez-moi vous en expliquer l'idée sur un exemple simple. Imaginez que vous travaillez sur un jeu RPG et que vous devez charger un énorme fichier contenant des personnages. Par exemple:

- `#` est de l'herbe. Vous pouvez marcher dessus.
- `$` est le point de départ
- `@` c'est du rock. Vous ne pouvez pas marcher dessus.
- `%` est un coffre au trésor

Exemple de carte :

`@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@`

`@############@@@@@######@#$@@@`

`@#############@@@######@###@@@`

`@#######%######@###########@@@`

`@############################@`

`@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@`

Étant donné que ces objets ont des caractéristiques similaires, vous n'avez pas besoin de créer un objet séparé pour chaque champ de la carte. Je vais vous montrer comment utiliser le poids mouche.

Définissons une interface que nos champs vont implémenter :

    public interface IField
    {
        string Name { get; }
        char Mark { get; }
        bool CanWalk { get; }
        FieldType Type { get; }
    }
Nous pouvons maintenant créer des classes qui représentent nos champs. Nous devons également les identifier d'une manière ou d'une autre (j'ai utilisé une énumération):

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
    
Comme je l'ai dit, nous n'avons pas besoin de créer une instance distincte pour chaque champ. Nous devons créer un __repository__ de champs. L'essence de Flyweight DP est que nous créons dynamiquement un objet uniquement si nous en avons besoin et qu'il n'existe pas encore dans notre référentiel, ou le renvoyons s'il existe déjà. Écrivons une classe simple qui gérera cela pour nous :

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
Super! Nous pouvons maintenant tester notre code :

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
Le résultat dans la console devrait être :
> Création d'une nouvelle instance de Grass
>
> Création d'une nouvelle instance de Rock

Mais pourquoi l'herbe n'apparaît qu'une seule fois si on voulait l'avoir deux fois ? C'est parce que la première fois que nous appelons l'instance d'herbe `GetField` n'existe pas dans notre __repository__, elle est donc créée, mais la prochaine fois que nous avons besoin d'herbe, elle existe déjà, nous la renvoyons donc seulement.



