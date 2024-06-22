# Shrewdish

A Redis client for Standard ML programs.

![shrew](shrew.jpeg)

## Usage

```sml
fun main () =
  let
    val (host, port) = ("localhost", 6379)
    val SOME conn = Shrewdish.connect host port
  in
    case Shrewdish.sendCommand conn ["PING"] of
      SOME (Redis.Value.String "PONG") => Log.info "PONG"
    | SOME v => Log.error ("Unexpected response" ^ Redis.Value.toString v)
    | _ => Log.error "No response"
  end
  handle ex => Log.error ("Error: " ^ General.exnMessage ex)
```

## Not implemented

- Great error handling
- Reconnections
- Client pooling
- Pub/Sub
- Sentinels
- RESP3
