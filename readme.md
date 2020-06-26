# prometheus-wai-middleware

Use this library to track HTTP status code counts and request latency in your web application.  We depend on `prometheus` _rather than_ `prometheus-client` (see [here][1] if you use `prometheus-client`).

[1]: http://hackage.haskell.org/package/wai-middleware-prometheus

## Examples

See `exec/Main.hs` for an example of how to instrument a Wai Application.  With a bit more legwork you can also instrument individual endpoints.  For each endpoint you wish to instrument, simply create an `ApplicationMetrics` value using `applicationMetrics` and update it with `countStatusCode` and `observeLatency`.
