
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pacta.sit.rep <img src="assets/images/logo.png" align="right" width="120" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

This repository generates a simple situation report of the overall code
health of the PACTA ecosystem.

Code health is reported at the level of the R package, and at the level
of the Transition Monitor Docker image.

## R Packages

### PACTA for Banks

| Repo                                                          | Lifecycle                                                                                                                        | Status                                                                                                                                                                                     | Test_Coverage                                                                                                                                                          | Latest_SHA                                                            | Maintainer                                |
|:--------------------------------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------|:----------------------------------------------------------------------|:------------------------------------------|
| [r2dii.data](https://github.com/RMI-PACTA/r2dii.data)         | [![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)  | [![R-CMD-check](https://github.com/RMI-PACTA/r2dii.data/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RMI-PACTA/r2dii.data/actions/workflows/R-CMD-check.yaml)         | [![Codecov test coverage](https://codecov.io/gh/RMI-PACTA/r2dii.data/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RMI-PACTA/r2dii.data?branch=main)         | [`a5da570`](https://github.com/RMI-PACTA/r2dii.data/commits/main)     | [@jacobvjk](https://github.com/jacobvjk/) |
| [r2dii.match](https://github.com/RMI-PACTA/r2dii.match)       | [![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)  | [![R-CMD-check](https://github.com/RMI-PACTA/r2dii.match/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RMI-PACTA/r2dii.match/actions/workflows/R-CMD-check.yaml)       | [![Codecov test coverage](https://codecov.io/gh/RMI-PACTA/r2dii.match/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RMI-PACTA/r2dii.match?branch=main)       | [`addb0cd`](https://github.com/RMI-PACTA/r2dii.match/commits/main)    | [@jacobvjk](https://github.com/jacobvjk/) |
| [r2dii.analysis](https://github.com/RMI-PACTA/r2dii.analysis) | [![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)  | [![R-CMD-check](https://github.com/RMI-PACTA/r2dii.analysis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RMI-PACTA/r2dii.analysis/actions/workflows/R-CMD-check.yaml) | [![Codecov test coverage](https://codecov.io/gh/RMI-PACTA/r2dii.analysis/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RMI-PACTA/r2dii.analysis?branch=main) | [`ee8246d`](https://github.com/RMI-PACTA/r2dii.analysis/commits/main) | [@jacobvjk](https://github.com/jacobvjk/) |
| [r2dii.plot](https://github.com/RMI-PACTA/r2dii.plot)         | [![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html) | [![R-CMD-check](https://github.com/RMI-PACTA/r2dii.plot/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RMI-PACTA/r2dii.plot/actions/workflows/R-CMD-check.yaml)         | [![Codecov test coverage](https://codecov.io/gh/RMI-PACTA/r2dii.plot/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RMI-PACTA/r2dii.plot?branch=main)         | [`f0eeda9`](https://github.com/RMI-PACTA/r2dii.plot/commits/main)     | [@MonikaFu](https://github.com/MonikaFu/) |

### Transition Monitor

| Repo                                                                                            | Lifecycle                                                                                                                        | Status                                                                                                                                                                                                                       | Test_Coverage                                                                                                                                                                                            | Latest_SHA                                                                             | Maintainer                                |
|:------------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------|:------------------------------------------|
| [pacta.data.validation](https://github.com/RMI-PACTA/pacta.data.validation)                     | [![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html) | [![R-CMD-check](https://github.com/RMI-PACTA/pacta.data.validation/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RMI-PACTA/pacta.data.validation/actions/workflows/R-CMD-check.yaml)                     | [![Codecov test coverage](https://codecov.io/gh/RMI-PACTA/pacta.data.validation/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RMI-PACTA/pacta.data.validation?branch=main)                     | [`8e1e85f`](https://github.com/RMI-PACTA/pacta.data.validation/commits/main)           | [@cjyetman](https://github.com/cjyetman/) |
| [pacta.scenario.data.preparation](https://github.com/RMI-PACTA/pacta.scenario.data.preparation) | [![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)  | [![R-CMD-check](https://github.com/RMI-PACTA/pacta.scenario.data.preparation/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RMI-PACTA/pacta.scenario.data.preparation/actions/workflows/R-CMD-check.yaml) | [![Codecov test coverage](https://codecov.io/gh/RMI-PACTA/pacta.scenario.data.preparation/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RMI-PACTA/pacta.scenario.data.preparation?branch=main) | [`a52eaf3`](https://github.com/RMI-PACTA/pacta.scenario.data.preparation/commits/main) | [@cjyetman](https://github.com/cjyetman/) |
| [pacta.data.scraping](https://github.com/RMI-PACTA/pacta.data.scraping)                         | [![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)  | [![R-CMD-check](https://github.com/RMI-PACTA/pacta.data.scraping/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RMI-PACTA/pacta.data.scraping/actions/workflows/R-CMD-check.yaml)                         | [![Codecov test coverage](https://codecov.io/gh/RMI-PACTA/pacta.data.scraping/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RMI-PACTA/pacta.data.scraping?branch=main)                         | [`f0656a9`](https://github.com/RMI-PACTA/pacta.data.scraping/commits/main)             | [@cjyetman](https://github.com/cjyetman/) |
| [pacta.data.preparation](https://github.com/RMI-PACTA/pacta.data.preparation)                   | [![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)  | [![R-CMD-check](https://github.com/RMI-PACTA/pacta.data.preparation/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RMI-PACTA/pacta.data.preparation/actions/workflows/R-CMD-check.yaml)                   | [![Codecov test coverage](https://codecov.io/gh/RMI-PACTA/pacta.data.preparation/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RMI-PACTA/pacta.data.preparation?branch=main)                   | [`2809a64`](https://github.com/RMI-PACTA/pacta.data.preparation/commits/main)          | [@cjyetman](https://github.com/cjyetman/) |
| [pacta.portfolio.import](https://github.com/RMI-PACTA/pacta.portfolio.import)                   | [![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)  | [![R-CMD-check](https://github.com/RMI-PACTA/pacta.portfolio.import/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RMI-PACTA/pacta.portfolio.import/actions/workflows/R-CMD-check.yaml)                   | [![Codecov test coverage](https://codecov.io/gh/RMI-PACTA/pacta.portfolio.import/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RMI-PACTA/pacta.portfolio.import?branch=main)                   | [`9d3ea19`](https://github.com/RMI-PACTA/pacta.portfolio.import/commits/main)          | [@cjyetman](https://github.com/cjyetman/) |
| [pacta.portfolio.audit](https://github.com/RMI-PACTA/pacta.portfolio.audit)                     | [![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)  | [![R-CMD-check](https://github.com/RMI-PACTA/pacta.portfolio.audit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RMI-PACTA/pacta.portfolio.audit/actions/workflows/R-CMD-check.yaml)                     | [![Codecov test coverage](https://codecov.io/gh/RMI-PACTA/pacta.portfolio.audit/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RMI-PACTA/pacta.portfolio.audit?branch=main)                     | [`2e7bee7`](https://github.com/RMI-PACTA/pacta.portfolio.audit/commits/main)           | [@cjyetman](https://github.com/cjyetman/) |
| [pacta.portfolio.allocate](https://github.com/RMI-PACTA/pacta.portfolio.allocate)               | [![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)  | [![R-CMD-check](https://github.com/RMI-PACTA/pacta.portfolio.allocate/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RMI-PACTA/pacta.portfolio.allocate/actions/workflows/R-CMD-check.yaml)               | [![Codecov test coverage](https://codecov.io/gh/RMI-PACTA/pacta.portfolio.allocate/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RMI-PACTA/pacta.portfolio.allocate?branch=main)               | [`4c96adb`](https://github.com/RMI-PACTA/pacta.portfolio.allocate/commits/main)        | [@cjyetman](https://github.com/cjyetman/) |
| [pacta.portfolio.report](https://github.com/RMI-PACTA/pacta.portfolio.report)                   | [![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html) | [![R-CMD-check](https://github.com/RMI-PACTA/pacta.portfolio.report/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RMI-PACTA/pacta.portfolio.report/actions/workflows/R-CMD-check.yaml)                   | [![Codecov test coverage](https://codecov.io/gh/RMI-PACTA/pacta.portfolio.report/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RMI-PACTA/pacta.portfolio.report?branch=main)                   | [`8330b97`](https://github.com/RMI-PACTA/pacta.portfolio.report/commits/main)          | [@MonikaFu](https://github.com/MonikaFu/) |
| [pacta.executive.summary](https://github.com/RMI-PACTA/pacta.executive.summary)                 | [![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html) | [![R-CMD-check](https://github.com/RMI-PACTA/pacta.executive.summary/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RMI-PACTA/pacta.executive.summary/actions/workflows/R-CMD-check.yaml)                 | [![Codecov test coverage](https://codecov.io/gh/RMI-PACTA/pacta.executive.summary/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RMI-PACTA/pacta.executive.summary?branch=main)                 | [`af49784`](https://github.com/RMI-PACTA/pacta.executive.summary/commits/main)         | [@MonikaFu](https://github.com/MonikaFu/) |
| [pacta.portfolio.utils](https://github.com/RMI-PACTA/pacta.portfolio.utils)                     | [![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)  | [![R-CMD-check](https://github.com/RMI-PACTA/pacta.portfolio.utils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RMI-PACTA/pacta.portfolio.utils/actions/workflows/R-CMD-check.yaml)                     | [![Codecov test coverage](https://codecov.io/gh/RMI-PACTA/pacta.portfolio.utils/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RMI-PACTA/pacta.portfolio.utils?branch=main)                     | [`f7e9e92`](https://github.com/RMI-PACTA/pacta.portfolio.utils/commits/main)           | [@cjyetman](https://github.com/cjyetman/) |

### PACTA for Supervisors

| Repo                                                                                        | Lifecycle                                                                                                                        | Status                                                                                                                                                                                             | Test_Coverage                                                                                                                                                                                        | Latest_SHA                                                                           | Maintainer                                |
|:--------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-------------------------------------------------------------------------------------|:------------------------------------------|
| [pacta.multi.loanbook.analysis](https://github.com/RMI-PACTA/pacta.multi.loanbook.analysis) | [![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html) | [![R-CMD-check](https://github.com/RMI-PACTA/pacta.multi.loanbook.analysis/actions/workflows/R.yml/badge.svg)](https://github.com/RMI-PACTA/pacta.multi.loanbook.analysis/actions/workflows/R.yml) | [![Codecov test coverage](https://codecov.io/gh/RMI-PACTA/pacta.multi.loanbook.analysis/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RMI-PACTA/pacta.multi.loanbook.analysis?branch=main) | [`e59b1e8`](https://github.com/RMI-PACTA/pacta.multi.loanbook.analysis/commits/main) | [@jacobvjk](https://github.com/jacobvjk/) |
| [pacta.multi.loanbook.plot](https://github.com/RMI-PACTA/pacta.multi.loanbook.plot)         | [![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html) | [![R-CMD-check](https://github.com/RMI-PACTA/pacta.multi.loanbook.plot/actions/workflows/R.yml/badge.svg)](https://github.com/RMI-PACTA/pacta.multi.loanbook.plot/actions/workflows/R.yml)         | [![Codecov test coverage](https://codecov.io/gh/RMI-PACTA/pacta.multi.loanbook.plot/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RMI-PACTA/pacta.multi.loanbook.plot?branch=main)         | [`7182557`](https://github.com/RMI-PACTA/pacta.multi.loanbook.plot/commits/main)     | [@MonikaFu](https://github.com/MonikaFu/) |

## Workflows and Docker Images

### Transition Monitor

| Repo                                                                                        | Lifecycle                                                                                                                       | Docker_Status                                                                                                                                                                                                                                                                            | Latest_SHA                                                                           | Maintainer                                      |
|:--------------------------------------------------------------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-------------------------------------------------------------------------------------|:------------------------------------------------|
| [workflow.scenario.preparation](https://github.com/RMI-PACTA/workflow.scenario.preparation) | [![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html) | [![docker](https://github.com/RMI-PACTA/workflow.scenario.preparation/actions/workflows/docker.yml/badge.svg)](https://github.com/RMI-PACTA/workflow.scenario.preparation/actions/workflows/docker.yml)                                                                                  | [`26811fa`](https://github.com/RMI-PACTA/workflow.scenario.preparation/commits/main) | [@cjyetman](https://github.com/cjyetman/)       |
| [workflow.data.preparation](https://github.com/RMI-PACTA/workflow.data.preparation)         | [![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html) | [![docker](https://github.com/RMI-PACTA/workflow.data.preparation/actions/workflows/docker.yml/badge.svg)](https://github.com/RMI-PACTA/workflow.data.preparation/actions/workflows/docker.yml)                                                                                          | [`eb78ff5`](https://github.com/RMI-PACTA/workflow.data.preparation/commits/main)     | [@cjyetman](https://github.com/cjyetman/)       |
| [workflow.transition.monitor](https://github.com/RMI-PACTA/workflow.transition.monitor)     | [![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html) | [![.github/workflows/build-Docker-image-triggers.yml](https://github.com/RMI-PACTA/workflow.transition.monitor/actions/workflows/build-Docker-image-triggers.yml/badge.svg)](https://github.com/RMI-PACTA/workflow.transition.monitor/actions/workflows/build-Docker-image-triggers.yml) | [`bb969d1`](https://github.com/RMI-PACTA/workflow.transition.monitor/commits/main)   | [@AlexAxthelm](https://github.com/AlexAxthelm/) |
