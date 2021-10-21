# trace-forward

`trace-forward` is a library allowing to forward tracing items from one process to another one. It is built upon [`typed-protocols`](https://github.com/the-blockchain-company/shardagnostic-network/tree/master/typed-protocols).

The `trace-dispatcher` is using `trace-forward` to forward `TraceObject`s from the node to exernal acceptors (for example, `bcc-tracer`).

## Developers

Benchmarking team is responsible for this library. The prijen developer is [@denisshevchenko](https://github.com/denisshevchenko).
