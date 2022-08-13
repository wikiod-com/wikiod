---
title: "Generics"
slug: "generics"
draft: false
images: []
weight: 9956
type: docs
toc: true
---

## Sort a dynamic array via generic TArray.Sort<T>
    uses
      System.Generics.Collections, { TArray }
      System.Generics.Defaults; { TComparer<T> }

    var StringArray: TArray<string>; { Also works with "array of string" }

    ...

    { Sorts the array case insensitive }
    TArray.Sort<string>(StringArray, TComparer<string>.Construct(
      function (const A, B: string): Integer
      begin
        Result := string.CompareText(A, B);
      end
    ));

## Simple usage of TList<T>
    var List: TList<Integer>;

    ...

    List := TList<Integer>.Create; { Create List }
    try
      List.Add(100); { Add Items }
      List.Add(200);

      WriteLn(List[1]); { 200 }
    finally
      List.Free;
    end;

## Descending from TList<T> making it specific
    type
      TIntegerList = class(TList<Integer>)
      public
        function Sum: Integer;
      end;

    ...

    function TIntegerList.Sum: Integer;
    var
      Item: Integer;
    begin
      Result := 0;
      for Item in Self do
         Result := Result + Item;
    end;

## Sort a TList<T>
    var List: TList<TDateTime>;

    ...

    List.Sort(
      TComparer<TDateTime>.Construct(
        function(const A, B: TDateTime): Integer
        begin
          Result := CompareDateTime(A, B);
        end
      )
    );


