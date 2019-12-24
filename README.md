# Working with APIs


## Contents
- [Purpose](#purpose)
- [Getting Started](#getting-started)
- [Security](#security)
- [Resources](#resources)

## Purpose
This repository contains code for working with the Smartsheet and Quick Base APIs. These scripts intend to link automate workflows and analysis or add functionality that would be cumbersome to do manually.

## Getting Started
APIs (application programming interfaces) are communication protocols that allow computers to exchange information. You can find a great overview of APIs [here](https://zapier.com/learn/apis/).

Before you can work with APIs, you'll need to prove that you are who your computer says you are. This involves creating an account and authenticating your identity. You can register for Smartsheet's developer portal [here](https://developers.smartsheet.com/), and you can use your Quickbase credentials to access the Quickbase API.

## Security
Most of the code in this repository uses environment variables stored on a local machine to avoid storing passwords directly in scripts. You can create environment variables on Windows machines by clicking on the Start menu and searching for "environment variables". This takes you to advanced system properties menu, where you can add environment variables that are stored on the local machine in the user's registry.

## Resources
The following resources are helpful for working with the Smartsheet and Quick Base APIs.
- [Quick Base API Guide](https://help.quickbase.com/api-guide/intro.html)
- [Smartsheet API 2.0 Documentation](http://smartsheet-platform.github.io/api-docs/)
- [QuNect ODBC](https://www.qunect.com/)