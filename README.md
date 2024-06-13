# gitlab-helper

Gets you info from the Gitlab API. Currently listing the branches of all projects inside a Gitlab group

## Configuration and Running

The app will look at several places for your configuration, in the following order:

1. `~/.gitlab-helper.yml`
1. `${PWD}/.gitlab-helper.yml` (the directory in which the executable is started)
1. environment variables

If more than one source defines a configuration property, the last one will be used.

The configuration files look like this:

```yaml
config:
  baseUrl: 'https://gitlab.breuni.de'
  apiToken: 'apitoken'
  groupId: 123
```

Or the following env variables:

* `HB_BASE_URL`: Base URL of the Gitlab instance you're working with
* `HB_GROUP_ID`: ID of the group you're interested in
* `HB_API_TOKEN`: API Token to use for authorizing requests against the Gitlab API. `api` scope is required.

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
