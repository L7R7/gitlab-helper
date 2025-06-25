# gitlab-helper

Some utilities for working with GitLab to make your life easier.

## Features

All the features are built for a GitLab user in mind that works in one single group in Gitlab at the time, while the number of projects in the group is high enough that it's hard to keep track of things and/or tedious to repeat the same action over and over again.

### Working with bulks of merge requests (aka "the million dollar feature" of this tool)

When using tools like Scala Steward or renovate-bot in a setting with many projects with a similar tech stack, you often end up with "the same" merge request for each project when one of the core dependencies is updated.  
Because it's tiresome to do that ("click on _auto-merge_ for the merge request that updates Spring Boot to the latest version, then rebase all the other open merge requests, and after that move on to the next project and repeat), the gitlab-helper will assist you with that.

Let's say you have a bunch of projects that use Spring Boot, and there's a new version released which causes your renovate-bot to open a Merge Request in all your projects.
You could list all of them like that:

```sh
gitlab-helper update-merge-requests --user-id <user-id> --search "Spring Boot" list
```

where `<user-id>` is the User ID of the bot user that your renovate-bot instance is using.
`--search` allows to filter the Merge Requests with a string that appears in the MR's title or the description.

If that looks good, you can merge them:

```sh
gitlab-helper update-merge-requests --user-id <user-id> --search "Spring Boot" merge --execute
```

The `--execute` flag actually makes the tool merge the merge requests. If you omit it, it will do a dry run (in this case, this would only list the MRs it would merge, equivalent to using `list`).
Merging will only be done when the Merge Request branch has a successful pipeline.
If there is no pipeline or the pipeline failed, the MR will stay open (consult the help text of the tool if you're brave and want to merge the merge requests anyway).

If there is a bunch of other MRs open on the project (maybe because there was also an update to npm), you can rebase all of them in one go:

```sh
gitlab-helper update-merge-requests --user-id <user-id> rebase --execute
```

After that, continue with the next bunch of similar MRs.
Or with some other meaningful work.

### Showing information

* `show-branches` lists all branches for all projects in the group, grouped by their project. It will also show whether a branch is stale, merged, or has conflicts with the default branch.
* `show-projects` lists all projects in the group, alongside some configuration details like default branch name, merge method, and merge request settings.
* `show-schedules` lists all the pipeline schedules for all projects in the group.
* `show-merge-requests` lists all the merge requests for all projects in the group.
* `list-projects-meta` lists all projects in a format that's compatible to [meta](https://github.com/mateodelnorte/meta). There's an argument to decide whether only the projects of the current group or all projects visible with the configured API token should be included.
* `count-deployments` counts all the deployments for all projects in the group. This assumes that you are doing continuous deployment, i.e. each pipeline that runs on the default branch and is successful can be considered a successful deployment to production. The project exclude list applies here, use this if you want to exclude projects you're not interested in.

### Setting options to all projects in the group

The following features can be used to change the project settings in bulk, so use it with care.
All of these features are also opinionated and try to do "the right thing".  
Ideally, you don't need these because you can set the correct defaults on group level, but if you happen to work with projects in a group that don't have the right settings, changing the settings on group level won't be enough.

* `enable-source-branch-deletion` sets the option that the source branch of a merge request is deleted after the merge request is merged
* `enable-all-discussions-must-be-resolved-for-merge-requirement` sets the option that all discussions must be resolved for a merge request to be merged
* `enable-successful-pipeline-for-merge-requirement` sets the option that a merge request requires a successful pipeline to be merged. Note that this means that merge requests in projects with no CI on branches other than the default branch can't be merged at all anymore.
* `set-merge-method-to-fast-forward` sets the merge method to "Fast-Forward"

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

* `GLH_BASE_URL`: Base URL of the Gitlab instance you're working with
* `GLH_GROUP_ID`: ID of the group you're interested in
* `GLH_API_TOKEN`: API Token to use for authorizing requests against the Gitlab API. `api` scope is required.
* `GLH_USER_AGENT`: User-Agent to use for requests against the Gitlab API.
* `GLH_EXCLUDE_PROJECTS`: Lists of projects to exclude

The structure of the configuration files looks like this:

```yaml
config:
  baseUrl: 'https://my.gitlab.com'
  apiToken: 'apitoken'
  userAgent: 'my-user-agent'
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

### Using pre-built binaries

There are pre-built binaries for most of the current platforms as part of the [releases](https://github.com/L7R7/gitlab-helper/releases/latest).
The fastest way to install the tool is downloading it from there.

If you're on macOS, you'll need to execute the following command after downloading the binary:

```shell script
xattr -r -d com.apple.quarantine gitlab-helper
```

### Building from Source

If you don't have a working Haskell toolchain on your machine yet, I recommend using [https://www.haskell.org/ghcup/](https://www.haskell.org/ghcup/) to install a recent version of `stack` (which solves similar problems like `sbt` does for Scala, or Maven for Java).

#### Install

To build the executable and make it available on your path:

```shell script
stack install
```

When you do that for the first time, it will take a while.

### Optional: Configure autocompletion

If you use `zsh`, put this in your `.zshrc` (see [here](https://stackoverflow.com/a/61861568/5247502)):

```shell script
gitlab-helper --zsh-completion-script $(which gitlab-helper) > ~/.config/zsh/completions/_gitlab-helper
fpath=($HOME/.config/zsh/completions $fpath)
```

If that doesn't work or if you use `bash`:

```shell script
source <(gitlab-helper --bash-completion-script `which gitlab-helper`)
```

### Run

See what the tool can do for you:

```shell script
gitlab-helper -h
```

The tool will start only if all properties are set in one of the sources.

The repository contains a config file that sets reasonable defaults, see [.gitlab-helper.yaml](.gitlab-helper.yml).

## Internals

## Creating a new release

Whenever a tag is pushed, a new release draft is created automatically.
The release notes will be pre-populated, and the artifacts for the release will be built and attached.

```shell script
git tag v0.0.1
git push --atomic origin main v0.0.1
```
