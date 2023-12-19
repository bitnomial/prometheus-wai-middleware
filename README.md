# prometheus-wai-middleware

Use this library to track HTTP status code counts and request handling times in your web application.  We depend on `prometheus` _rather than_ `prometheus-client` (see [here][1] if you use `prometheus-client`).  The metrics are

* `http_requests_total` with codes labeled by `http_response_code`
* `http_request_duration_milliseconds`

Here is an example of the metrics generated:

```
# TYPE http_request_duration_milliseconds histogram
http_request_duration_milliseconds_bucket{app="example_server",le="1.0"} 2.0
...
http_request_duration_milliseconds_sum{app="example_server"} 0.0
http_request_duration_milliseconds_count{app="example_server"} 2
# TYPE http_requests_total counter
http_requests_total{app="example_server",http_response_code="100"} 0
...
http_requests_total{app="example_server",http_response_code="200"} 1
...
```

[1]: http://hackage.haskell.org/package/wai-middleware-prometheus

## Examples

See `exec/Main.hs` for an example of how to instrument a Wai Application.  With a bit more legwork you can also instrument individual endpoints.  For each endpoint you wish to instrument, simply create an `ApplicationMetrics` value using `applicationMetrics` and update it with `countStatusCode` and `observeLatency`.
