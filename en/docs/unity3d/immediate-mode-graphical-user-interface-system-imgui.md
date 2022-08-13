---
title: "Immediate Mode Graphical User Interface System (IMGUI)"
slug: "immediate-mode-graphical-user-interface-system-imgui"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Syntax
- public static void GUILayout.Label(string text, params GUILayoutOption[] options)
- public static bool GUILayout.Button(string text, params GUILayoutOption[] options)
- public static string GUILayout.TextArea(string text, params GUILayoutOption[] options)

## GUILayout
 Old UI system tool, now used for fast and simple prototyping or debugging in game.

<!-- language: c# -->

    void OnGUI ()
    {
        GUILayout.Label ("I'm a simple label text displayed in game.");

        if ( GUILayout.Button("CLICK ME") )
        {
            GUILayout.TextArea ("This is a \n
                                 multiline comment.")
        }
    }

**GUILayout** function works inside the **OnGUI** function. 

