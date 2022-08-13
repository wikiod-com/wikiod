---
title: "How to use variables inside a script component"
slug: "how-to-use-variables-inside-a-script-component"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

This post provides steps to use variables (User Variable, Package Parameter and Project Parameter) in the script component and viewing the updated value using Breakpoint and Watch window.

## Parameters
| Parameter | Details |
| ------ | ------ |
| UserVar | It is like a local variable used inside a package. Its value can be read and modified in script task|
| Result  | It is a local variable which will hold the concatenated result. Its value can be read and modified in script task|
| PackageVar | It is package parameter, which can be shared between packages. Its value is read only inside script component|
| ProjectParm | It is project parameter, which is available after deployment as a configuration. Its value is read only inside the script component|

## Steps to achieve the objective.
**SSIS tasks required.**
--

1. **Data Flow Task:** As the script component is only available in the data flow.
2. **Script Component:** Inside this we will use variables and play with there values.

**Steps**
--

*There are two methods to access variables inside of script component*

**First Method - Using `this.Variables`**

1. Create two user variable Result (String), UserVar (String value: UserVar), also create a package parameter PackageVar (String value: PackageVariable) and a project parameter ProjectParam (String value: ProjectParameter).
2. Drag and drop a *Data Flow Task* from the SSIS Toolbox under favorites section.
[![enter image description here][1]][1]
3. Double click on the *Data flow task* which will take you into the Data Flow. Now from the SSIS Toolbox drag and drop *Script Component* present under *Common* section. It will prompt with a window having three choices Source, Destination and Transformation. Select Source and click OK. 
[![enter image description here][2]][2]
4. Double click on the *Script component*, on the right hand side for **ReadOnlyVariables** click on the three dots it will open a *Select Variable* window. Now select User::UserVar, $Package::PackageVar and $Project::ProjectParm. Click OK. Similarly Click on the three dots corresponding to the **ReadWriteVariables** and select User::Result. Click OK.
[![enter image description here][3]][3]
5. On the left hand side of Script Transformation Editor select *Inputs and Outputs*, On center Expand **Output O** -> expand Output Columns at bottom Click *Add Column*. When we use script component as Source, it is required to have output column, that is why we have created this output column. 
[![enter image description here][4]][4]
6. On the left Click on the *Script*, on the right bottom of the editor click *Edit Scripts*, it will open a new window, in this window find **PostExecute()** method and write ` this.Variables.Result = this.Variables.UserVar + this.Variables.PackageVar + this.Variables.ProjectParm;` through this.Variables we are accessing variable. Click Ctrl+S to save and close the window. Click OK.
[![enter image description here][5]][5]
7. Go to the Control Flow and right click on the *Data Flow Task* and select **Edit Breakpoints**. Now in the new *Set Breakpoints* window *Select Break when the container receives the OnPostExecute event*. Click OK.
[![enter image description here][6]][6]    
8. Now in the *Solution Explorer* right click on the package name and click Execute Package. From the menu bar click on the Debug -> Windows -> Watch -> Watch 1. Now at the bottom Watch window will be visible. Under *Name* type User::Result and click enter. Under *value* concatenated values {UserVarPackageVariableProjectParameter} can be seen. 
[![enter image description here][7]][7]

**Second Method - Using VariableDispenser**

When the variable dispenser is used adding the variables to the `ReadOnlyVariables` and `ReadWriteVariables` is not needed. You can use the following codes to Read and write variables values: *(Code is for ssis 2008)*

*Write*

    private void WriteVariable(String varName, Object varValue)
    {
    
      IDTSVariables100 vars = null;
      VariableDispenser.LockForWrite(varName);
      VariableDispenser.GetVariables(out vars);
      vars[varName].Value = varValue;
      vars.Unlock();
     
    }

*Read*

    private Object ReadVariable(String varName)
    {
    
      Object varValue;
      IDTSVariables100 vars = null;
      VariableDispenser.LockForRead(varName);
      VariableDispenser.GetVariables(out vars);
      varValue = vars[varName].Value;
      vars.Unlock();
     
      return varValue;

    }

  [1]: https://i.stack.imgur.com/s1b64.png
  [2]: https://i.stack.imgur.com/SnsDU.png
  [3]: https://i.stack.imgur.com/n1tMJ.png
  [4]: https://i.stack.imgur.com/ClNsv.png
  [5]: https://i.stack.imgur.com/tOshU.png
  [6]: https://i.stack.imgur.com/sCVt1.png
  [7]: https://i.stack.imgur.com/TOlgC.png

