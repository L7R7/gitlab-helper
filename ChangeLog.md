# Changelog for gitlab-helper

## Unreleased changes

nothing so far

## [1.0.0] - 2023-10-04

Initial release.

### Features

* Update merge requests: list, merge, rebase, toggle the draft status
* Show all projects in a group
* Show all branches for the projects in a group
* Show all open merge requests for all projects in a group
* Show all pipeline schedules for all projects in a group
* Set the option to delete the source branch after a merge request is merged for all projects in a group
* Set the option to require all discussions to be resolved for a merge request to be merged for all projects in a group
* Set the option that there must be a successful pipeline for a merge request to be merged for all projects in a group
* Set the merge method to fast-forward for all projects in a group
* Create a list compatible with [meta](https://github.com/mateodelnorte/meta) that includes either all projects of a group or all projects that are visible with the provided API-Token
* Count the successful deployments for a given year in a group

### Other

* Configuration via config files, environment variables, command-line arguments
* Automatic creation of draft releases to facilitate releasing new versions
