---
title: "Finding and collecting GameObjects"
slug: "finding-and-collecting-gameobjects"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## Syntax
 - public static GameObject Find(string name);
 - public static GameObject FindGameObjectWithTag(string tag); 
 - public static GameObject[] FindGameObjectsWithTag(string tag); 
 - public static Object FindObjectOfType(Type type); 
 - public static Object[] FindObjectsOfType(Type type); 

## Which method to use ##

Be careful while looking for GameObjects at runtime, as this can be resource consuming. Especially : don't run FindObjectOfType or Find in Update, FixedUpdate or more generally in a method called one or more time per frame.

 - Call runtime methods `FindObjectOfType` and `Find` only when necessary
 - `FindGameObjectWithTag` has very good performance compared to other string based methods. Unity keeps separate tabs on tagged objects and queries those instead of the entire scene.
 - For "static" GameObjects (such as UI elements and prefabs) created in the editor use [serializable GameObject reference][1] in the editor
 - Keep your lists of GameObjects in List or Arrays that you manage yourself
 - In general, if you instantiate a lot of GameObjects of the same type take a look at [Object Pooling][2]
 - Cache your search results to avoid running the expensive search methods again and again.

## Going deeper ##
Besides the methods that come with Unity, it's relatively easy to design your own search and collection methods.

- In case of `FindObjectsOfType()`, you could have your scripts keep a list of themselves in a `static` collection. It is far faster to iterate a ready list of objects than to search and inspect objects from the scene.

- Or make a script that stores their instances in a string based `Dictionary`, and you have a simple tagging system you can expand upon.


  [1]: https://www.wikiod.com/unity3d/finding-and-collecting-gameobjects#Inserted to scripts in Edit Mode
  [2]: https://www.wikiod.com/unity3d/object-pooling#Object Pool

## Searching by GameObject's name
<!-- language-all: c# -->

    var go = GameObject.Find("NameOfTheObject");

Pros        | Cons
------------|-----------------
Easy to use | Performance degrades along the number of gameobjects in scene
  | Strings are *weak references* and suspect to user errors

## Searching by GameObject's tags
<!-- language: c# -->

    var go = GameObject.FindGameObjectWithTag("Player");

Pros | Cons
---|---
Possible to search both single objects and entire groups | Strings are weak references and suspect to user errors.
Relatively fast and efficient | Code is not portable as tags are hard coded in scripts.

## Finding GameObjects by MonoBehaviour scripts
<!-- language: c# -->

    ExampleScript script = GameObject.FindObjectOfType<ExampleScript>();
    GameObject go = script.gameObject;

> `FindObjectOfType()` returns `null` if none is found.

Pros | Cons
---|---
Strongly typed | Performance degrades along the number of gameobjects needed to evaluate
Possible to search both single objects and entire groups |

## Inserted to scripts in Edit Mode
<!-- language: c# -->

    [SerializeField]
    GameObject[] gameObjects;

Pros | Cons
---|---
Great performance | Object collection is static
Portable code | Can only refer to GameObjects from the same scene

## Find GameObjects by name from child objects
<!-- language: c# -->
    Transform tr = GetComponent<Transform>().Find("NameOfTheObject");
    GameObject go = tr.gameObject;

> `Find` returns `null` if none is found

Pros | Cons
---|---
Limited, well defined search scope | Strings are weak references


