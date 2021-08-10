# Formally#

Formally# is an online formal language designer.<br/>
**Check it out at [www.formallysharp.codes](http://www.formallysharp.codes)**


## SAFE Stack

This is a full-stack web application using the [SAFE Stack](https://safe-stack.github.io/docs/overview/).

![SAFE](https://www.compositional-it.com/wp-content/uploads/2019/09/safe-1.png)

Other than the prototypical SAFE components (**S**aturn running on an **A**zure server and **F**able **E**lmish to compile a SPA client), we also depend on:
* [Fable.Remoting](https://zaid-ajaj.github.io/Fable.Remoting/) for our RPC communication layer
* [Feliz](https://zaid-ajaj.github.io/Feliz/) for client-side rendering, styled with [Feliz.Bulma](https://dzoukr.github.io/Feliz.Bulma/#/api-description)


## Dev Setup

### Install pre-requisites

You'll need to install the following pre-requisites in order to build, test and run the application in your local system:

* [.NET Core SDK](https://www.microsoft.com/net/download) 5.0 or higher
* [ASP.NET Core Runtime](https://dotnet.microsoft.com/apps/aspnet)
* [Node LTS](https://nodejs.org/en/download/), including NPM

Before running the project **for the first time**, you must also restore dotnet project-local tools:

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

Client test results can be seen at [localhost:8008](http://localhost:8008)<br/>
Server results are printed directly to the console.

#### Deploy

Builds the project in release mode, packaging it in the `deploy/` folder:

```sh
$ dotnet run bundle
```

If you have Azure CLI installed in your system, this rule deploys the project to **your account**:

```sh
$ dotnet run azure
```
