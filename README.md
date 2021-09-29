# Formally#

![GitHub release (semver)](https://img.shields.io/github/v/release/baioc/FormallySharp)
![GitHub top language](https://img.shields.io/github/languages/top/baioc/FormallySharp?color=%2330b9db)
![GitHub commit activity](https://img.shields.io/github/commit-activity/m/baioc/FormallySharp?label=commits)

Formally# is an online formal language designer.
**Check it out at [formallysharp.azurewebsites.net](https://formallysharp.azurewebsites.net/)**


## SAFE Stack

This is a full-stack web application using the [SAFE Stack](https://safe-stack.github.io/docs/overview/).

![SAFE](https://www.compositional-it.com/wp-content/uploads/2019/09/safe-1.png)

Other than the prototypical SAFE components, we also depend on:
* [Fable.Remoting](https://zaid-ajaj.github.io/Fable.Remoting/) for our RPC communication layer
* [Feliz](https://zaid-ajaj.github.io/Feliz/) for client-side rendering, styled with [Feliz.Bulma](https://dzoukr.github.io/Feliz.Bulma/#/api-description)
* [LiteDB](https://www.litedb.org/) for simple data persistency in the cloud


## Dev Setup

### Install pre-requisites

You'll need to install the following pre-requisites in order to build, test and run the application in your local system:

* [.NET Core SDK](https://www.microsoft.com/net/download) 5.0 or higher
* [ASP.NET Core Runtime](https://dotnet.microsoft.com/apps/aspnet)
* [Node LTS](https://nodejs.org/en/download/), including NPM

Before running the project for the first time (**and after updates**), you must also restore local dotnet tools:

```sh
$ dotnet tool restore
```

### Commands

All dev commands are implemented in a console app using [FAKE](https://fake.build/) build rules.

#### Run

Locally runs both server and client in watch mode (i.e. **hot-reload enabled**):

```sh
$ dotnet run
```

Then, browse to [localhost:8080](http://localhost:8080)

#### Test

Concurrently runs server and client tests in watch mode:

```sh
$ dotnet run tests
```

Client test results can be seen at [localhost:8008](http://localhost:8008) and server results are printed directly to the console.

#### Deploy

Builds the project in release mode, packaging it in the `deploy/` folder:

```sh
$ dotnet run bundle
```
