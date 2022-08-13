---
title: "Working with angular-cli Generating components, directives, pipes, services, etc."
slug: "working-with-angular-cli-generating-components-directives-pipes-services-etc"
draft: false
images: []
weight: 9719
type: docs
toc: true
---

The angular-cli tool can help you to scaffold different parts of an angular application (components, directives, pipes, services, classes, guards, interfaces, enums and modules).

## Syntax
- ng generate [component | directive | service | pipe | class | enum | interface | guard | module] [name] [flags...]
- ng g [c | d | s | p | cl | e | i | g | m] [name] [flags...]

## Parameters
Parameter                         | Description
---                               |---          
`component` or `c`                | Used to generate component
`directive` or `d`                | Used to generate directives
`service` or `s`                  | Used to generate services
`pipe` or `p`                     | Used to generate pipes
`class` or `cl`                   | Used to generate classes
`enum` or `e`                     | Used to generate enums
`interfaces` or `i`               | Used to generate interfaces
`guard` or `g`                    | Used to generate guards
`module` or `m`                   | Used to generate modules
`--flat` or `-f`                  | Used to enable/disable directory creation
`--inline-template` or `-it`      | Used to enable/disable inline html templates in components
`--inline-style` or `-is`         | Used to enable/disable inline styles in components
`--prefix` or `-p`                | Used to disable or change prefix
`--spec` or `-s`                  | Used to enable/disable `.spec` files creation
`--skip-import`                   | Used to skip the module import
`--app` or `-a`                   | Used to specify app name to use
`--module` or `-m`                | Used to specify the declaring module
`--view-encapsulation` or `-ve`   | Used to specify the view encapsulation strategy in components
`--change-detection` or `-cd`     | Used to specify the change detection strategy in components
`--routing ` or `-r`              | Used to specify if routing module file should be generated


## "Generate command" usage
You can use the `ng generate` or `ng g` command to generate Angular building blocks (components, services, pipes, etc.).

You can find all possible **blueprints** in the table below:

Scaffold  | Usage                                   | Shortened
---       | ---                                     | ---   
Component | `ng generate component component-name`  | `ng g c component-name`
Directive | `ng generate directive directive-name`  | `ng g d directive-name`
Pipe      | `ng generate pipe pipe-name`            | `ng g p pipe-name`
Service   | `ng generate service service-name`      | `ng g s service-name`
Class     | `ng generate class class-name`          | `ng g cl class-name`
Guard     | `ng generate guard guard-name`          | `ng g g guard-name`
Interface | `ng generate interface interface-name`  | `ng g i interface-name`
Enum      | `ng generate enum enum-name`            | `ng g e enum-name`
Module    | `ng generate module module-name`        | `ng g m module-name`

So, for example, if you run `ng generate component user-list` - angular-cli will: 
 - create `user-list` directory in `src/app` folder or folder where you have run the command.
 - inside that directory generate 4 files (`user-list.component.ts`, `user-list.component.html`, `user-list.component.css` and `user-list.component.spec.ts`)
 - add `user-list` as a declaration in the `@NgModule` decorator of the nearest module.


## Generating services
To add a service with a name `UserService`, run:

```bash
  $ ng g s user

  installing service
    create src/app/user.service.spec.ts
    create src/app/user.service.ts
```

- To prevent `.spec` files creation add `--spec false` or `-sp false` flag

```bash
  $ ng g s user --spec false
  
  installing service
    create src/app/user.service.ts
```

- To enable folder creation add `--flat false` or `-f false` flag

```bash
  $ ng g s user --flat false

  installing service
    create src/app/user/user.service.spec.ts
    create src/app/user/user.service.ts
```

You can also combine flags listed above. For example, to create only `user.service.ts` file inside `user` folder without `.spec` file use the following command.

```bash
  $ ng g s user -f false -sp false

  installing service
    create src/app/user/user.service.ts
```

All `generate service` flags:

Description                     | Flag                                    | Shortened     | Default Value
---                             | ---                                     | ---           | ---
Enable folder creation          | `--flat false`                          | `-f false`    | `true`
Prevent `.spec` files creation  | `--spec false`                          | `-sp false`   | `true`


## Generating modules
To add a module called `GuestModule`, run:

```bash
  $ ng g m guest

  installing module
    create src/app/guest/guest.module.ts
```

- To enable `.spec` files creation add `--spec` or `-sp` flag

```bash
  $ ng g m guest --spec

  installing module
    create src/app/guest/guest.module.spec.ts
    create src/app/guest/guest.module.ts
```

- To enable routing add `--routing` or `-r` flag

```bash
  $ ng g m guest --routing

  installing module
    create src/app/guest/guest-routing.module.ts
    create src/app/guest/guest.module.ts
```

You can also combine flags listed above. For example, to create module with routing and specs use the following command.

```bash
  $ ng g m guest -sp -r
  
  installing module
    create src/app/guest/guest-routing.module.ts
    create src/app/guest/guest.module.spec.ts
    create src/app/guest/guest.module.ts
```

All `generate module` flags:

Description                     | Flag                                    | Shortened     | Default Value
---                             | ---                                     | ---           | ---
Enable `.spec` files creation   | `--spec`                                |`-sp`          | `false`
Enable routing                  | `--routing`                             |`-r`           | `false`


## Generating components
To add a component with a selector `[prefix]-user-list`, run:

```bash
  $ ng g c user-list
  
  installing component
    create src/app/user-list/user-list.component.css
    create src/app/user-list/user-list.component.html
    create src/app/user-list/user-list.component.spec.ts
    create src/app/user-list/user-list.component.ts
    update src/app/app.module.ts
```

> **prefix** prevents element name collisions with components in other apps and with native HTML elements.
So, for example, if prefix is `app` - generated component will have `app-user-list` selector.

- To prevent prefix usage add `--prefix false` or `-p false` flag

```bash
  $ ng g c user-list --prefix false
```

```typescript
import { Component } from '@angular/core';

@Component({
  selector: 'user-list',
  templateUrl: './user-list.component.html',
  styleUrls: ['./user-list.component.css']
})
export class UserListComponent {}
```

- To prevent `.spec` files creation add `--spec false` or `-sp false` flag

```bash
  $ ng g c user-list --spec false

  installing component
    create src/app/user-list/user-list.component.css
    create src/app/user-list/user-list.component.html
    create src/app/user-list/user-list.component.ts
    update src/app/app.module.ts
```

- To use inline html templates instead of external templates add `--inline-template` or `-it` flag

```bash
  $ ng g c user-list --inline-template

  installing component
    create src/app/user-list/user-list.component.css
    create src/app/user-list/user-list.component.spec.ts
    create src/app/user-list/user-list.component.ts
    update src/app/app.module.ts
```

- To use inline styles instead of external styles add `--inline-style` or `-is` flag

```bash
  $ ng g c user-list --inline-style

  installing component
    create src/app/user-list/user-list.component.html
    create src/app/user-list/user-list.component.spec.ts
    create src/app/user-list/user-list.component.ts
    update src/app/app.module.ts
```

- To prevent folder creation add `--flat` or `-f` flag

```bash
  $ ng g c user-list --flat
  
  installing component
    create src/app/user-list.component.css
    create src/app/user-list.component.html
    create src/app/user-list.component.spec.ts
    create src/app/user-list.component.ts
    update src/app/app.module.ts
```

You can also combine flags listed above. For example, to create only `.component.ts` file without `.css`, `.html`, `.spec` files and folder use the following command.

```bash
  $ ng g c user-list -f -it -is -sp false
  
  installing component
    create src/app/user-list.component.ts
    update src/app/app.module.ts  
```

All `generate component` flags:

Description                     | Flag                                    | Shortened     | Default Value
---                             | ---                                     | ---           | ---
Prevent folder creation         | `--flat`                                | `-f`          | `false`
Prevent prefix usage            | `--prefix false`                        | `-p false`    | `true`
Prevent `.spec` files creation  | `--spec false`                          | `-sp false`   | `true`
Enable  inline html templates   | `--inline-template`                     | `-it`         | `false`
Enable  inline styles           | `--inline-style`                        | `-is`         | `false`


## Generating directives
To add a directive with a selector `[prefix]Highlight`, run:

```bash
  $ ng g d highlight

  installing directive
    create src/app/highlight.directive.spec.ts
    create src/app/highlight.directive.ts
    update src/app/app.module.ts
```

- To prevent prefix usage add `--prefix false` or `-p false` flag

```bash
  $ ng g d highlight --prefix false
```

```typescript
import { Directive } from '@angular/core';

@Directive({
  selector: '[highlight]'
})
export class HighlightDirective {}
```

- To prevent `.spec` files creation add `--spec false` or `-sp false` flag

```bash
  $ ng g d highlight --spec false

  installing directive
    create src/app/highlight.directive.ts
    update src/app/app.module.ts
```

- To enable folder creation add `--flat false` or `-f false` flag

```bash
  $ ng g d highlight --flat false
  
  installing directive
    create src/app/highlight/highlight.directive.spec.ts
    create src/app/highlight/highlight.directive.ts
    update src/app/app.module.ts
```

You can also combine flags listed above. For example, to create only `highlight.directive.ts` file inside `highlight` folder without `.spec` file use the following command.

```bash
  $ ng g d highlight -f false -sp false

  installing directive
    create src/app/highlight/highlight.directive.ts
    update src/app/app.module.ts
```

All `generate directive` flags:

Description                     | Flag                                    | Shortened     | Default Value
---                             | ---                                     | ---           | ---
Enable folder creation          | `--flat false`                          | `-f false`    | `true`
Prevent prefix usage            | `--prefix false`                        | `-p false`    | `true`
Prevent `.spec` files creation  | `--spec false`                          | `-sp false`   | `true`


## Generating pipes
To add a pipe with a name `searchByName`, run:

```bash
  $ ng g p search-by-name

  installing pipe
    create src/app/search-by-name.pipe.spec.ts
    create src/app/search-by-name.pipe.ts
    update src/app/app.module.ts
```

- To prevent `.spec` files creation add `--spec false` or `-sp false` flag

```bash
  $ ng g p search-by-name --spec false
  
  installing pipe
    create src/app/search-by-name.pipe.ts
    update src/app/app.module.ts
```

- To enable folder creation add `--flat false` or `-f false` flag

```bash
  $ ng g p search-by-name --flat false
  
  installing pipe
    create src/app/search-by-name/search-by-name.pipe.spec.ts
    create src/app/search-by-name/search-by-name.pipe.ts
    update src/app/app.module.ts
```

You can also combine flags listed above. For example, to create only `search-by-name.pipe.ts` file inside folder `search-by-name` folder without `.spec` file use the following command.

```bash
  $ ng g p search-by-name -f false -sp false

  installing pipe
    create src/app/search-by-name/search-by-name.pipe.ts
    update src/app/app.module.ts
```

All `generate pipe` flags:

Description                     | Flag                                    | Shortened     | Default Value
---                             | ---                                     | ---           | ---
Enable folder creation          | `--flat false`                          | `-f false`    | `true`
Prevent `.spec` files creation  | `--spec false`                          | `-sp false`   | `true`


