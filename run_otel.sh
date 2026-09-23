#! /usr/bin/env bash

# OpenTelemetry Configuration
export OTEL_LOG_LEVEL="debug"
export OTEL_SERVICE_NAME="water-wars"
export OTEL_RESOURCE_ATTRIBUTES="service.instance.id=$(uuidgen)"
export OTEL_EXPORTER_OTLP_PROTOCOL="grpc"

# Create a pipe for the eventlog
EVENTLOG_PIPE="/tmp/eventlog.pipe"
rm -f $EVENTLOG_PIPE
mkfifo "${EVENTLOG_PIPE}"

cabal run exe:water-wars-server -- -p 8080 +RTS -l -ol"${EVENTLOG_PIPE}" -hT --eventlog-flush-interval=1 -RTS &

# Start eventlog-live-otlp
eventlog-live-otlp \
  --eventlog-file="${EVENTLOG_PIPE}" \
  -hT \
  --eventlog-flush-interval=1
