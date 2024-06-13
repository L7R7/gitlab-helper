# gitlab-helper

Some utilities for working with the GitLab API to make your life easier.

## Configuration

The app will look at several places for your configuration, in the following order:

1. command-line arguments
1. environment variables
1. `${PWD}/.gitlab-helper.yml` (the directory in which the executable is started)
1. `~/.gitlab-helper.yml`

If an option is set in multiple sources, the first one wins.
My recommendation is to configure the Base URL and the API Token in the file in your home directory, because they will very likely always be the same.
If you work with different groups, and your working directory is different for each group you work with, you can create a config file that sets the group ID (and possibly excludes) in each of these directories.

For the available command-line options, use `--help` to get the full list.

The available environmnent variables are:

* `HB_BASE_URL`: Base URL of the Gitlab instance you're working with
* `HB_GROUP_ID`: ID of the group you're interested in
* `HB_API_TOKEN`: API Token to use for authorizing requests against the Gitlab API. `api` scope is required.
* `HB_EXCLUDE_PROJECTS`: Lists of projects to exclude

The structure of the configuration files looks like this:

```yaml
config:
  baseUrl: 'https://gitlab.breuni.de'
  apiToken: 'apitoken'
  groupId: 123
  excludeProjects: []
```

### Excluding projects

You can configure a list of IDs of projects that should be excluded from processing.
This does not apply to all possible commands and is not supported in all commands where this might make sense.

There are a couple of interesting details to this option:

* If you don't provide a value in any of the config sources, it defaults to an empty list
* Any configured value overrides other configured values, just like the other options (it might be intuitive to expect the concatenation of all configured lists, but that's not the case). This also means that you can disable all exclusions by configuring an empty list (`""`).

## Running

If you don't have a working Haskell toolchain on your machine yet, I recommend using [https://www.haskell.org/ghcup/](https://www.haskell.org/ghcup/) to install a recent version of `stack` (which solves similar problems like `sbt` does for Scala, or Maven for Java).

To build the executable and make it available on your path:

```shell script
stack install
```

Optionally, install autocompletion:

```shell script
source <(gitlab-helper-exe --bash-completion-script `which gitlab-helper-exe`)
```

See what the tool can do for you:

```shell script
gitlab-helper-exe -h
```

The app will start only if all properties are set in one of the sources.

The repository contains a config file that sets reasonable defaults, see [.gitlab-helper.yaml](.gitlab-helper.yml).
