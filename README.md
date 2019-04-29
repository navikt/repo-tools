[![CircleCI](https://circleci.com/gh/navikt/repo-tools.svg?style=svg)](https://circleci.com/gh/navikt/repo-tools)

# Repo Tools

This project is a web application that is used by developers in NAV to manage GitHub repositories.

### Technologies used:

- ##### Backend:
    - [Haskell](https://www.haskell.org/) - programming language
    - [Stack](https://haskellstack.org/) - package manager, build tool
    - [Yesod](https://www.yesodweb.com/) - web framework
    - [Haskell OIDC client](https://github.com/krdlab/haskell-oidc-client) for authentication with Azure AD
    - [GitHub API](https://developer.github.com/v3/) for creating git repositories

- ##### Frontend:
    - [Node.js/npm](https://nodejs.org/en/) - for local development and building static assets
    - [ReasonML](https://reasonml.github.io/) - OCaml dialect that compiles to JavaScript
    - [ReasonReact](https://reasonml.github.io/reason-react/) - React bindings for ReasonML
    - [Bulma](https://bulma.io/) - CSS framework

- ##### Build:
    - [Docker](https://www.docker.com/) (for building the application in a container environment)

### GitHub integration

There is a GitHub app, [navikt-repo-tools](https://github.com/apps/navikt-repo-tools/), which
is configured to give us access to manage repositories on [navikt](https://github.com/navikt) on GitHub.

### Setting up a local development environment

1) [Install Haskell and Stack](https://docs.haskellstack.org/en/stable/README/)
2) [Install Node.js and npm](https://nodejs.org/en/)
3) Setup an IDE (for example IntelliJ with the Haskell plugin and ReasonML plugin)
4) [Install Docker](https://www.docker.com/get-started)

### Running locally

1) `git clone` the project
2) Run `./scripts/dev.sh`

The application will run on `http://localhost:3000`.

### Building locally

Build the Docker image: `./scripts/build.sh`
