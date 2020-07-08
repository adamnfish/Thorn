Thorn
=====

Web-based party game.

Work on Thorn is ongoing, it is not yet playable.

## Development

### Project structure

#### core

Scala code containing the game's logic.

#### devserver

Websockets application for running the server on your own development
machine.

#### frontend

Elm Application that provides the Web frontend for Thorn.

#### integration

Integration test suite for the server's game logic.

#### lambda

Provides the core functionality as a handler suitable for execution as
am AWS Lambda Function.

#### project

Build information for the various Scala projects (see also `build.sbt`
in the project root).

#### wat

Web Automation Tests for Thorn.

### Running locally

To run Thorn locally you'll need to make sure both the frontend Elm
App, and the backend development server API are running.

Run the following in separate terminal windows to start both services.

#### Frontend

    cd frontend
    elm-app start

#### Development server

    ./start-devserver.sh

### Web Automation Tests

With the frontend and dev-server running, you can run web automated
tests against Thorn.

    cd wat
    npm install
    npm run wat

As well as checking the game is playable this will produce screenshots
in the `wat/screenshots` directory.
