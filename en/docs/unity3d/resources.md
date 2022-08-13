---
title: "Resources"
slug: "resources"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Resources 101


Introduction
============

  
Unity has a few 'specially named' folders that allows for a variety of uses. One of these folders is called 'Resources'

The 'Resources' folder is one of only TWO ways of loading assets at runtime in Unity (The other being [AssetBundles (Unity Docs)][1]

The 'Resources' folder can reside anywhere inside your Assets folder, and you can have multiple folders named Resources. The contents of all 'Resources' folders are merged during compile time.


The primary way to load an asset from a Resources folder is to use the [Resources.Load][2] function. This function takes a string parameter which allows you to specify the path of the file **relative** to the Resources folder. Note that you do NOT need to specify file extensions while loading an asset

<!-- language: c# -->

    public class ResourcesSample : MonoBehaviour {  
        
        void Start () {
            //The following line will load a TextAsset named 'foobar' which was previously place under 'Assets/Resources/Stackoverflow/foobar.txt'
            //Note the absence of the '.txt' extension! This is important!

            var text = Resources.Load<TextAsset>("Stackoverflow/foobar").text;
            Debug.Log(string.Format("The text file had this in it :: {0}", text));
        }
    }


Objects which are comprised of multiple objects can also be loaded from Resources. Examples are such objects are 3D models with textures baked in, or a multiple sprite.

<!-- language: c# -->

    //This example will load a multiple sprite texture from Resources named "A_Multiple_Sprite"
    var sprites = Resources.LoadAll("A_Multiple_Sprite") as Sprite[];
<br>
<br>

Putting it all together
=======================

Here's one of my helper classes which I use to load all sounds for any game. You can attach this to any GameObject in a scene and it will load the specified audio files from the 'Resources/Sounds' folder

<!-- language: c# -->

    public class SoundManager : MonoBehaviour {
        
        void Start () {
    
            //An array of all sounds you want to load
            var filesToLoad = new string[] { "Foo", "Bar" };
    
            //Loop over the array, attach an Audio source for each sound clip and assign the 
            //clip property.
            foreach(var file in filesToLoad) {
                var soundClip = Resources.Load<AudioClip>("Sounds/" + file);
                var audioSource = gameObject.AddComponent<AudioSource>();
                audioSource.clip = soundClip;
            }
        }
    }

<br>
<br>

Final Notes
===========
1. Unity is smart when it comes to including assets into your build. Any asset that is not serialized (i.e. used in a scene that's included in a build) is excluded from a build. HOWEVER this DOES NOT apply to any asset inside the Resources folder. Therefore, do not go overboard on adding assets to this folder

2. Assets that are loaded using Resources.Load or Resources.LoadAll can be unloaded in the future by using [Resources.UnloadUnusedAssets][3] or [Resources.UnloadAsset][4]


  [1]: https://docs.unity3d.com/Manual/AssetBundlesIntro.html
  [2]: https://docs.unity3d.com/ScriptReference/Resources.Load.html
  [3]: https://docs.unity3d.com/ScriptReference/Resources.UnloadUnusedAssets.html
  [4]: https://docs.unity3d.com/ScriptReference/Resources.UnloadAsset.html

## Introduction
With Resources class it's possible to dynamically load assets that are not part of the scene.
It's very usefull when you have to use on demand assets, for example localize multi language audios, texts, etc..

Assets must be placed in a folder named **Resources**. It's possible to have multiple Resources folders spread across the project's hierarchy. `Resources` class will inspect all Resources folders you may have.

Every asset placed within Resources will be included in the build even it's not referenced in your code. Thus don't insert assets in Resources indiscriminately.

<!-- language: c# -->

    //Example of how to load language specific audio from Resources

    [RequireComponent(typeof(AudioSource))]
    public class loadIntroAudio : MonoBehaviour {
        void Start () {
            string language = Application.systemLanguage.ToString();
            AudioClip ac = Resources.Load(language + "/intro") as AudioClip; //loading intro.mp3 specific for user's language (note the file file extension should not be used)
            if (ac==null)
            {
                ac = Resources.Load("English/intro") as AudioClip; //fallback to the english version for any unsupported language
            }
            transform.GetComponent<AudioSource>().clip = ac;
            transform.GetComponent<AudioSource>().Play();
        }
    }

