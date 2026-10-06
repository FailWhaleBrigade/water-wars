#! /usr/bin/env bash

# OpenTelemetry Configuration
export OTEL_LOG_LEVEL="debug"
export OTEL_SERVICE_NAME="water-wars"
export OTEL_RESOURCE_ATTRIBUTES="service.instance.id=water-wars-server"
export OTEL_EXPORTER_OTLP_PROTOCOL="grpc"

# Create a pipe for the eventlog
EVENTLOG_PIPE="/tmp/eventlog.pipe"
rm -f $EVENTLOG_PIPE
mkfifo "${EVENTLOG_PIPE}"

/water-wars/bin/water-wars-server -p 8000 +RTS -l -ol"${EVENTLOG_PIPE}" -hT --eventlog-flush-interval=1 -RTS &
PID=$!

# Define a cleanup function
cleanup() {
    echo "Stopping process $PID..."
    kill "$PID" 2>/dev/null
}

# Trap EXIT or SIGINT (Ctrl+C) to run cleanup
trap cleanup EXIT SIGINT

# Start eventlog-live-otlp
/water-wars/bin/eventlog-live-otlp \
  --eventlog-file="${EVENTLOG_PIPE}" \
  -hT \
  --eventlog-flush-interval=1
