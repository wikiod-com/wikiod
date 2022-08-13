---
title: "Chronomètres"
slug: "chronometres"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Syntaxe
- stopWatch.Start() - Démarre le chronomètre.
- stopWatch.Stop() - Arrête le chronomètre.
- stopWatch.Elapsed - Obtient le temps total écoulé mesuré par l'intervalle actuel.

Les chronomètres sont souvent utilisés dans les programmes d'analyse comparative pour chronométrer le code et voir à quel point différents segments de code sont optimaux pour s'exécuter.

## EstHauteRésolution
    

- La propriété IsHighResolution indique si le minuteur est basé sur un compteur de performances haute résolution ou sur la classe DateTime.
- Ce champ est en lecture seule.


    // Display the timer frequency and resolution.
    if (Stopwatch.IsHighResolution)
    {
        Console.WriteLine("Operations timed using the system's high-resolution performance counter.");
    }
    else 
    {
        Console.WriteLine("Operations timed using the DateTime class.");
    }

    long frequency = Stopwatch.Frequency;
    Console.WriteLine("  Timer frequency in ticks per second = {0}",
        frequency);
    long nanosecPerTick = (1000L*1000L*1000L) / frequency;
    Console.WriteLine("  Timer is accurate within {0} nanoseconds", 
        nanosecPerTick);
    }
https://dotnetfiddle.net/ckrWUo

Le minuteur utilisé par la classe Stopwatch dépend du matériel système et du système d'exploitation. IsHighResolution a la valeur true si le chronomètre du chronomètre est basé sur un compteur de performances haute résolution. Sinon, IsHighResolution est faux, ce qui indique que le chronomètre du chronomètre est basé sur le chronomètre système.

Les ticks dans Stopwatch dépendent de la machine / du système d'exploitation, vous ne devez donc jamais compter sur le rapport des ticks du chronomètre aux secondes pour être le même entre deux systèmes, et peut-être même sur le même système après un redémarrage. Ainsi, vous ne pouvez jamais compter sur les ticks du chronomètre pour avoir le même intervalle que les ticks DateTime/TimeSpan.

Pour obtenir une heure indépendante du système, assurez-vous d'utiliser les propriétés Elapsed ou ElapsedMilliseconds du chronomètre, qui prennent déjà en compte le chronomètre.Fréquence (ticks par seconde).

Le chronomètre doit toujours être utilisé sur Datetime pour les processus de chronométrage car il est plus léger et utilise Dateime s'il ne peut pas utiliser un compteur de performances haute résolution.

[Source] (http://geekswithblogs.net/BlackRabbitCoder/archive/2012/01/12/c.net-little-pitfalls-stopwatch-ticks-are-not-timespan-ticks.aspx)

## Création d'une instance d'un chronomètre
Une instance de chronomètre peut mesurer le temps écoulé sur plusieurs intervalles, le temps écoulé total étant tous les intervalles individuels additionnés. Cela donne une méthode fiable pour mesurer le temps écoulé entre deux événements ou plus.


    Stopwatch stopWatch = new Stopwatch();
    stopWatch.Start();

    double d = 0;
    for (int i = 0; i < 1000 * 1000 * 1000; i++)
    {
        d += 1;
    }

    stopWatch.Stop();
    Console.WriteLine("Time elapsed: {0:hh\\:mm\\:ss\\.fffffff}", stopWatch.Elapsed);


`Stopwach` est dans `System.Diagnostics` vous devez donc ajouter `using System.Diagnostics;` à votre fichier.

