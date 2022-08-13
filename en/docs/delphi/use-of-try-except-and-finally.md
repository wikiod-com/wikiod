---
title: "Use of try, except, and finally"
slug: "use-of-try-except-and-finally"
draft: false
images: []
weight: 9793
type: docs
toc: true
---

## Syntax
 1. Try-except: try [statements] except [[[on E:ExceptionType do
    statement]] [else statement] | [statements] end;
    
    Try-finally: try [statements] finally [statements] end;

## Exception-safe return of a new object
When a function *returns* an object (as opposed to *using* one that's passed in by the caller), be careful an exception doesn't cause the object to leak.

    function MakeStrings: TStrings;
    begin
      // Create a new object before entering the try-block.
      Result := TStringList.Create;
      try
        // Execute code that uses the new object and prepares it for the caller.
        Result.Add('One');
        MightThrow;
      except
        // If execution reaches this point, then an exception has occurred. We cannot
        // know how to handle all possible exceptions, so we merely clean up the resources
        // allocated by this function and then re-raise the exception so the caller can
        // choose what to do with it.
        Result.Free;
        raise;
      end;
      // If execution reaches this point, then no exception has occurred, so the
      // function will return Result normally.
    end;

Naive programmers might attempt to catch all exception types and return `nil` from such a function, but that's just a special case of the general discouraged practice of catching all exception types without handling them.

## Try-except nested inside try-finally
A `try`-`except` block may be nested inside a `try`-`finally` block.

    AcquireResource;
    try
      UseResource1;
      try
        UseResource2;
      except
        on E: EResourceUsageError do begin
          HandleResourceErrors;
        end;
      end;
      UseResource3;
    finally
      ReleaseResource;
    end;

If an `EResourceUsageError` occurs in `UseResource2`, then execution will jump to the exception handler and call `HandleResourceError`. The exception will be considered handled, so execution will continue to `UseResource3`, and then `ReleaseResource`.

If an exception of any other type occurs in `UseResource2`, then the exception handler show here will not apply, so execution will jump over the `UseResource3` call and go directly to the `finally` block, where `ReleaseResource` will be called. After that, execution will jump to the next applicable exception handler as the exception bubbles up the call stack.

If an exception occurs in any other call in the above example, then `HandleResourceErrors` will *not* be called. This is because none of the other calls occur inside the `try` block corresponding to that exception handler.

## Simple try..finally example to avoid memory leaks
Use `try`-`finally` to avoid leaking resources (such as memory) in case an exception occurs during execution.

The procedure below saves a string in a file and prevents the `TStringList` from leaking.

    procedure SaveStringToFile(const aFilename: TFilename; const aString: string);
    var
      SL: TStringList;
    begin
      SL := TStringList.Create; // call outside the try 
      try
        SL.Text := aString;
        SL.SaveToFile(aFilename);
      finally
        SL.Free // will be called no matter what happens above
      end;
    end;

Regardless of whether an exception occurs while saving the file, `SL` will be freed. Any exception will go to the caller.

## Try-finally with 2 or more objects
    Object1 := nil;
    Object2 := nil;
    try
      Object1 := TMyObject.Create;
      Object2 := TMyObject.Create;
    finally
      Object1.Free;
      Object2.Free;
    end;

If you do not initialize the objects with `nil` outside the `try-finally` block, if one of them fails to be created an AV will occur on the finally block, because the object won't be nil (as it wasn't initialized) and will cause an exception.

The `Free` method checks if the object is nil, so initializing both objects with `nil` avoids errors when freeing them if they weren't created.

## Try-finally nested inside try-except
A `try`-`finally` block may be nested inside a `try`-`except` block.

    try
      AcquireResources;
      try
        UseResource;
      finally
        ReleaseResource;
      end;
    except
      on E: EResourceUsageError do begin
        HandleResourceErrors;
      end;
    end;

If an exception occurs inside `UseResource`, then execution will jump to `ReleaseResource`. If the exception is an `EResourceUsageError`, then execution will jump to the exception handler and call `HandleResourceErrors`. Exceptions of any other type will skip the exception handler above and bubble up to the next `try`-`except` block up the call stack.

Exceptions in `AcquireResource` or `ReleaseResource` will cause execution to go to the exception handler, skipping the `finally` block, either because the corresponding `try` block has not been entered yet or because the `finally` block has *already* been entered.

