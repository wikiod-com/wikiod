---
title: "Implementando o padrão de design Flyweight"
slug: "implementando-o-padrao-de-design-flyweight"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Implementando mapa em jogo de RPG
Flyweight é um dos padrões de projeto estrutural. Ele é usado para diminuir a quantidade de memória usada compartilhando o máximo de dados possível com objetos semelhantes. Este documento irá ensiná-lo a usar o Flyweight DP corretamente.

Deixe-me explicar a idéia disso para você em um exemplo simples. Imagine que você está trabalhando em um jogo de RPG e precisa carregar um arquivo enorme que contenha alguns personagens. Por exemplo:

- `#` é grama. Você pode andar nele.
- `$` é o ponto de partida
- `@` é rock. Você não pode andar nele.
- `%` é o baú do tesouro

Exemplo de um mapa:

`@@@@@@@@@@@@@@@@@@@@@@@@@@@@@`

`@############@@@@######@#$@@@`

`@#############@@######@###@@@@

`@#######%######@###########@@@@

`@############################@`

`@@@@@@@@@@@@@@@@@@@@@@@@@@@@@`

Como esses objetos possuem características semelhantes, você não precisa criar objetos separados para cada campo do mapa. Eu vou te mostrar como usar o peso-mosca.

Vamos definir uma interface que nossos campos irão implementar:

    public interface IField
    {
        string Name { get; }
        char Mark { get; }
        bool CanWalk { get; }
        FieldType Type { get; }
    }
Agora podemos criar classes que representam nossos campos. Também temos que identificá-los de alguma forma (usei uma enumeração):

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
    
Como eu disse, não precisamos criar instâncias separadas para cada campo. Temos que criar um __repositório__ de campos. A essência do Flyweight DP é que criamos dinamicamente um objeto somente se precisarmos dele e ele ainda não existe em nosso repositório, ou o retornamos se já existir. Vamos escrever uma classe simples que irá lidar com isso para nós:

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
Excelente! Agora podemos testar nosso código:

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
O resultado no console deve ser:
> Criei uma nova instância do Grass
>
> Criei uma nova instância do Rock

Mas por que a grama aparece apenas uma vez se quiséssemos pegá-la duas vezes? Isso porque a primeira vez que chamamos a instância de grama `GetField` não existe em nosso __repositório__, então ela está criada, mas na próxima vez que precisarmos de grama ela já existe, então apenas a retornamos.



