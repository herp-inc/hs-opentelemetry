# yesod-minimal Example

## How to Run

Run a following command to start Jaeger and PostgreSQL.

```
$ make server.run DOCKER_COMPOSE_OPTS=-d
```

Build and run this example.

```
$ make app.run
```

You can access following end points.

- http://localhost:16686/
  - Jaeger UI
- http://localhost:3000/
  - target app

When you access http://localhost:3000/, Open Telemetry's traces are sent to the Jaeger.
And you can watch the traces at http://localhost:16686.

![Screenshot of Jaeger](./image/jaeger-trace-example.png)
