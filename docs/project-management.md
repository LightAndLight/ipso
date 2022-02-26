# Project Management

We use [GitHub issues](https://github.com/features/issues) to track project work.

<!-- omit in toc -->
## Contents

* [Issues](#issues)
  * [Issue Content](#issue-content)
  * [Branching](#branching)
* [Projects](#projects)
  * [Design](#design)
  * [Features](#features)

## Issues

Issues belong to one of 2 projects:

* [*Design*](#design)
* [*Features*](#features)

### Issue Content

Every issue should provide enough context as to be self-contained. We can work more efficiently when we don't need to ask person who filed the issue for more details.

For example, bug reports should provide replication instructions and features should provide acceptance criteria.

Each [Project](#projects) has a more specific description of how its issues should be written.

### Branching

When an issue requires changes to the repository, the changes are tracked in a branch
named `issue/<ISSUE NUMBER>`.

## Projects

### Design

*Design* tracks work that requires planning, investigation, discussion, and design.

A *Design* issue's title must be the topic of investigation/discussion. For example, "Array index syntax" or "Faster import resolution". This should be a [noun phrase](https://en.wikipedia.org/wiki/Noun_phrase).

A *Design* issue's content must explain the problem to be solved. Once the problem has been explained, proposed solutions or a request for comments may follow.

### Features

*Features* tracks the implementation progress of features. 

A *Feature* issue's title must be the name of the feature. For example, "Function composition operator", "String interner", or "Syntax-highlighted error reporting". This should be a [noun phrase](https://en.wikipedia.org/wiki/Noun_phrase).

A *Feature* issue's content must contain the following acceptance criteria as tasks:

* Use cases
  
  This includes well-formed code samples and their associated output, malformed code samples and their associated error messages, test cases that must pass, and user workflows.

* Documentation

  This includes updates to user-facing documentation, as well as internal API documentation.