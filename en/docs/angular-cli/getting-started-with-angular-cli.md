---
title: "Getting started with angular-cli"
slug: "getting-started-with-angular-cli"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Generating and serving an Angular project via a development server
To create a new project `ng new [project-name]`  which initializes git.Install packages for tooling via npm. It creates the project successfully.

`cd [project-name]` and execute either `npm start` or `ng serve`

It'll run the server in the given default port.

Application will refresh according to the changes made in the directory. The default HTTP port and the one used by the LiveReload server can be changed using below command

    ng serve --host 0.0.0.0 --port 4201 --live-reload-port 49153

## Installation or Setup
**Note:** Angular CLI versions are under rapid development. This documentation targets for the latest version.

----------

**Prerequisites**

To execute and work with the latest version, Angular CLI and the project have dependencies that require node v6.9.0 or higher.


----------


**Setup**

Make sure that a node version is installed which is compatible with the CLI

Install the Angular CLI globally. It installs the latest version.

`npm i @angular/cli -g` or `yarn global add @angular/cli`, depending on the package manager in use.


----------
`ng help` command will provide the executable commands

## Angular CLI - The Basic Steps
 1. You will need to install node.js - https://nodejs.org/en/
 2. npm install -g @angular/cli - install the CLI by executing this command in the terminal 
 3. ng new projectname - after executing this command in the terminal, you will create a new sub folder titled projectname in your current folder. 
 4. cd projectname - go to the sub folder, where your project is
 5. ng serve - run the project. It will be available at http://localhost:4200

