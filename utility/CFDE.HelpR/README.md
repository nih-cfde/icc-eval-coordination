
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CFDE.HelpR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

### What is CFDE?

The Common Fund Data Ecosystem (CFDE) is an initiative aimed at
integrating knowledge across NIH Common Fund programs into a cohesive
resource. App Description

This GitHub App allows, in conjunction with a Shiny Web Application
allows users to efficiently add CFDE Core Project Numbers as topics to
GitHub repositories within this ecosystem. By tagging repositories, you
can share additional information about your Common Fund project, aiding
the NIH in gaining a comprehensive understanding of your work.

#### Key Features

- CFDE Core Project Lookup: Search for your project number by name,
  institution or keyword
- Bulk Topic Addition: Add core project numbers as topics to several
  repositories in a single action, saving time and effort compared to
  adding topics individually.

## Installation Instructions

This application includes an RPackage, a web app, developed using R
Shiny, and a GitHub App. The GitHub App is used to authenticate users,
enabling the web app to perform actions on their behalf.

## RPackage

You can install the development version of CFDE.HelpR from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("nih-cfde/icc-eval-coordination/utility/CFDE.HelpR")
```

## Web App:

*No Installation Required*: This means you can access and use the app
directly through your web browser. There’s no need to download or
install any software on your computer. GitHub App:

*Installation Required*: Unlike the web app, the GitHub App needs to be
installed on your GitHub account or organization. This installation
process involves granting the app specific permissions to interact with
your repositories.

Permissions: When you install the GitHub App, you need to authorize it
to perform certain actions, such as tagging repositories. This is done
to ensure the app has the necessary access to manage topics and other
repository settings.

## Permissions

The app needs to be able to administer user/organization repositories.
These are the permissions requested and how we use them:

### Repository

*Repository/Organization*

- *Read Access*: Allows the app to view repository content and metadata.
  This is used to populate a list of projects that you already own
- *Write Access*: Allows the app to add a topic to the repository

In this context, “administering repositories” means the app needs admin
access to manage topics effectively. However, it respects the existing
permissions of the user, ensuring it only operates within the scope of
access the user already has.

## Usage Instructions

1.  After installing this GitHub App, run `CFDE.HelpR::run_app()` or
    head to: <https://cu-dbmi.shinyapps.io/CFDE-All_Aboard>
2.  Locate your core project.
3.  Locate the repository (or multiple repositories) you want to tag.
4.  Click “Add Topic” to tag the repositories with the CFDE Core Project
    Number.

## Support and Contact Information

For support, please visit the [GitHub
repository](https://github.com/nih-cfde/icc-eval-coordination).

## Code of Conduct

Please note that the CFDE.HelpR project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
