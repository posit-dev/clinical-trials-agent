# On Posit Connect, route OpenTelemetry traces to this content before any
# package builds the tracer provider: Connect 2026.07.0 injects only its own
# k8s resource attributes (Connect issue #41838), and repairing them after
# ellmer has built the provider at load time has no effect. The TLS setting
# works around otelsdk not honoring OTEL_EXPORTER_OTLP_CERTIFICATE, without
# which spans are silently dropped in export.
if (nzchar(Sys.getenv("CONNECT_CONTENT_GUID"))) {
  Sys.setenv(
    OTEL_RESOURCE_ATTRIBUTES = paste0(
      "content.guid=", Sys.getenv("CONNECT_CONTENT_GUID"),
      ",job.key=", Sys.getenv("CONNECT_CONTENT_JOB_KEY")
    ),
    OTEL_R_EXPORTER_OTLP_SSL_INSECURE_SKIP_VERIFY = "true"
  )
}
