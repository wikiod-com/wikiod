---
title: "TStringList class"
slug: "tstringlist-class"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Introduction
[TStringList][1] is a descendant of the TStrings class of the VCL. TStringList can be used for  storing and manipulating of list of Strings. Although originally intended for Strings, any type of objects can also be manipulated using this class. 

TStringList is widely used in VCL when the the purpose is there for maintaining a list of Strings. TStringList supports a rich set of methods which offer high level of customization and ease of manipulation.

The following example demonstrates the creation, adding of strings, sorting, retrieving and freeing of a TStringList object.

    procedure StringListDemo;
    var 
       MyStringList: TStringList;
       i: Integer;
    
    Begin
    
       //Create the object
       MyStringList := TStringList.Create();
       try
          //Add items
          MyStringList.Add('Zebra');
          MyStringList.Add('Elephant');
          MyStringList.Add('Tiger');
    
          //Sort in the ascending order
          MyStringList.Sort;
    
          //Output 
          for i:=0 to MyStringList.Count - 1 do
            WriteLn(MyStringList[i]);
       finally
          //Destroy the object
          MyStringList.Free;
       end;
    end;


TStringList has a variety of user cases including string manipulation, sorting, indexing, key-value pairing and delimiter separation among them. 


  [1]: http://docwiki.embarcadero.com/Libraries/Berlin/en/System.Classes.TStringList

## Key-Value Pairing
You can use a TStringList to store Key-Value pairs. This can be useful if you want to store settings, for example. A settings consists of a Key (The Identifier of the setting) and the value.
Each Key-Value pair is stored in one line of the StringList in Key=Value format.

    procedure Demo(const FileName: string = '');
    var
       SL: TStringList;
       i: Integer;
    begin
         SL:= TStringList.Create;
         try
            //Adding a Key-Value pair can be done this way
            SL.Values['FirstName']:= 'John';   //Key is 'FirstName', Value is 'John'
            SL.Values['LastName']:= 'Doe';   //Key is 'LastName', Value is 'Doe'
    
            //or this way
            SL.Add('City=Berlin');  //Key ist 'City', Value is 'Berlin'
    
            //you can get the key of a given Index
            IF SL.Names[0] = 'FirstName' THEN
             begin
                  //and change the key at an index
                  SL.Names[0]:= '1stName';  //Key is now "1stName", Value remains "John"
             end; 
          
            //you can get the value of a key
            s:= SL.Values['City']; //s now is set to 'Berlin'
    
            //and overwrite a value 
            SL.Values['City']:= 'New York';

            //if desired, it can be saved to an file
            IF (FileName <> '') THEN
             begin
                  SL.SaveToFile(FileName);
             end;
         finally
            SL.Free;
         end;
    end;

In this example, the Stringlist has the following content before it is destroyed:

    1stName=John
    LastName=Doe
    City=New York

**Note on performance**

Under the hood `TStringList` performs key search by straight looping through all items, searching for separator inside every item and comparing the name part against the given key. No need to say it does huge impact on performance so this mechanism should only be used in non-critical, rarely repeated places. In cases where performance matters, one should use `TDictionary<TKey,TValue>` from `System.Generics.Collections` that implements hash table search or to keep keys in **sorted** `TStringList` with values stored as `Object`-s thus utilizing binary find algorithm.

