# (build-)hooks-bot

Automatically add a webhook to all projects inside a Gitlab group

## Configuration and Running

Configuration is done via environment variables.
If you try to run it:

```shell script
stack run
```
the app will tell you which configuration options there are and what they mean.

There's more to configure when creating a webhook for a repository, especially the different trigger options.
For now, these are hard coded to "Pipeline events" exclusively.
