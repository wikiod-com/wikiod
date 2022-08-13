---
title: "Fonderie"
slug: "fonderie"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

*Diffusion* n'est pas la même chose que *Conversion*. Il est possible de convertir la valeur de chaîne `"-1"` en une valeur entière (`-1`), mais cela doit être fait via des méthodes de bibliothèque comme `Convert.ToInt32()` ou `Int32.Parse()`. Cela ne peut pas être fait en utilisant directement la syntaxe de casting.

## Convertir un objet en type de base


## Vérification de la compatibilité sans diffusion
Si vous avez besoin de savoir si le type d'une valeur étend ou implémente un type donné, mais que vous ne voulez pas le convertir en ce type, vous pouvez utiliser l'opérateur `is`.

    if(value is int)
    {
       Console.WriteLine(value + "is an int");
    }

## Casting explicite
Si vous savez qu'une valeur est d'un type spécifique, vous pouvez la convertir explicitement en ce type afin de l'utiliser dans un contexte où ce type est nécessaire.

    object value = -1;
    int number = (int) value;
    Console.WriteLine(Math.Abs(number));

Si nous essayions de passer `value` directement à `Math.Abs()`, nous obtiendrions une exception au moment de la compilation car `Math.Abs()` n'a pas de surcharge qui prend un `object` comme paramètre.

Si `value` ne pouvait pas être converti en `int`, alors la deuxième ligne de cet exemple lèverait une `InvalidCastException`

## Casting explicite sécurisé (opérateur `as`)
Si vous n'êtes pas sûr qu'une valeur soit du type que vous pensez qu'elle est, vous pouvez la transtyper en toute sécurité en utilisant l'opérateur `as`. Si la valeur n'est pas de ce type, la valeur résultante sera "null".

    object value = "-1";
    int? number = value as int?;
    if(number != null)
    {
        Console.WriteLine(Math.Abs(number.Value));
    }

Notez que les valeurs `null` n'ont pas de type, donc le mot-clé `as` donnera en toute sécurité `null` lors de la conversion de toute valeur `null`.

## Casting implicite
Une valeur sera automatiquement convertie dans le type approprié si le compilateur sait qu'elle peut toujours être convertie dans ce type.

    int number = -1;
    object value = number;
    Console.WriteLine(value);

Dans cet exemple, nous n'avons pas eu besoin d'utiliser la syntaxe typique de transtypage explicite car le compilateur sait que tous les "int" peuvent être transtypés en "object". En fait, nous pourrions éviter de créer des variables et passer `-1` directement comme argument de `Console.WriteLine()` qui attend un `object`.

    Console.WriteLine(-1);

## Conversions numériques explicites
Les opérateurs de transtypage explicites peuvent être utilisés pour effectuer des conversions de types numériques, même s'ils ne s'étendent pas ou ne s'implémentent pas les uns les autres.

    double value = -1.1;
    int number = (int) value;

Notez que dans les cas où le type de destination a moins de précision que le type d'origine, la précision sera perdue. Par exemple, `-1.1` en tant que valeur double dans l'exemple ci-dessus devient `-1` en tant que valeur entière.

De plus, les conversions numériques reposent sur des types au moment de la compilation, elles ne fonctionneront donc pas si les types numériques ont été "encadrés" dans des objets.

    object value = -1.1;
    int number = (int) value; // throws InvalidCastException


## Opérateurs de conversion
En C#, les types peuvent définir des *opérateurs de conversion* personnalisés, qui permettent de convertir des valeurs vers et depuis d'autres types à l'aide de transtypages explicites ou implicites. Par exemple, considérons une classe censée représenter une expression JavaScript :

    public class JsExpression
    {
        private readonly string expression;
        public JsExpression(string rawExpression)
        {
            this.expression = rawExpression;
        }
        public override string ToString()
        {
            return this.expression;
        }
        public JsExpression IsEqualTo(JsExpression other)
        {
            return new JsExpression("(" + this + " == " + other + ")");
        }
    }

Si nous voulions créer une JsExpression représentant une comparaison de deux valeurs JavaScript, nous pourrions faire quelque chose comme ceci :

    JsExpression intExpression = new JsExpression("-1");
    JsExpression doubleExpression = new JsExpression("-1.0");
    Console.WriteLine(intExpression.IsEqualTo(doubleExpression)); // (-1 == -1.0)

Mais nous pouvons ajouter des *opérateurs de conversion explicites* à `JsExpression`, pour permettre une conversion simple lors de l'utilisation de la conversion explicite.

    public static explicit operator JsExpression(int value)
    {
        return new JsExpression(value.ToString());
    }
    public static explicit operator JsExpression(double value)
    {
        return new JsExpression(value.ToString());
    }

    // Usage:
    JsExpression intExpression = (JsExpression)(-1);
    JsExpression doubleExpression = (JsExpression)(-1.0);
    Console.WriteLine(intExpression.IsEqualTo(doubleExpression)); // (-1 == -1.0)

Ou, nous pourrions changer ces opérateurs en *implicite* pour rendre la syntaxe beaucoup plus simple.

    public static implicit operator JsExpression(int value)
    {
        return new JsExpression(value.ToString());
    }
    public static implicit operator JsExpression(double value)
    {
        return new JsExpression(value.ToString());
    }

    // Usage:
    JsExpression intExpression = -1;
    Console.WriteLine(intExpression.IsEqualTo(-1.0)); // (-1 == -1.0)



## Opérations de diffusion LINQ
Supposons que vous ayez des types comme celui-ci :

    interface IThing {  }
    class Thing : IThing {  }

LINQ vous permet de créer une projection qui modifie le type générique au moment de la compilation d'un `IEnumerable<>` via les méthodes d'extension `Enumerable.Cast<>()` et `Enumerable.OfType<>()`.

    IEnumerable<IThing> things = new IThing[] {new Thing()};
    IEnumerable<Thing> things2 = things.Cast<Thing>();
    IEnumerable<Thing> things3 = things.OfType<Thing>();

Lorsque `things2` est évalué, la méthode `Cast<>()` essaiera de convertir toutes les valeurs de `things` en `Thing`s. S'il rencontre une valeur qui ne peut pas être transtypée, une `InvalidCastException` sera levée.

Lorsque `things3` est évalué, la méthode `OfType<>()` fera de même, sauf que si elle rencontre une valeur qui ne peut pas être castée, elle omettra simplement cette valeur plutôt que de lancer une exception.

En raison du type générique de ces méthodes, elles ne peuvent pas invoquer d'opérateurs de conversion ni effectuer de conversions numériques.

    double[] doubles = new[]{1,2,3}.Cast<double>().ToArray(); // Throws InvalidCastException

Vous pouvez simplement effectuer un cast à l'intérieur d'un `.Select()` comme solution de contournement :

    double[] doubles = new[]{1,2,3}.Select(i => (double)i).ToArray();

