# gitlab-helper

Gets you info from the Gitlab API. Currently listing the branches of all projects inside a Gitlab group

## Configuration and Running

The app will look at several places for your configuration, in the following order:

1. command-line parameters
1. environment variables
1. `${PWD}/.gitlab-helper.yml` (the directory in which the executable is started)
1. `~/.gitlab-helper.yml`

If an option is set in multiple sources, the first one wins.

For the available command-line options, use `--help` to get the full list of options.

The available environmnent variables are:

* `HB_BASE_URL`: Base URL of the Gitlab instance you're working with
* `HB_GROUP_ID`: ID of the group you're interested in
* `HB_API_TOKEN`: API Token to use for authorizing requests against the Gitlab API. `api` scope is required.

The structure of the configuration files looks like this:

```yaml
config:
  baseUrl: 'https://gitlab.breuni.de'
  apiToken: 'apitoken'
  groupId: 123
```

To build the executable and make it available on your path:

```shell script
stack install
```

Install autocompletion:

```shell script
source <(gitlab-helper-exe --bash-completion-script `which gitlab-helper-exe`)
```

See what the tool can do for you:

```shell script
gitlab-helper-exe -h
```

The app will start only if all properties are set in one of the sources.

The repository contains a config file that sets reasonable defaults, see [.gitlab-helper.yaml](.gitlab-helper.yml).
