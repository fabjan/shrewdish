local

open Shrewdish

in

fun parseAddr addr =
  case String.fields (fn c => c = #":") addr of
    [host, port] => (
      case Int.fromString port of
        SOME port => (host, port)
      | NONE => raise Fail "Invalid port"
    )
    | _ => raise Fail "Invalid address"

fun sendCommand conn cmd andThen =
  case Shrewdish.sendCommand conn cmd of
    SOME v => andThen v
  | _ => Log.error "No reply from Redis"

fun incrCounter conn key =
  sendCommand conn ("INCR " ^ key) (
  fn Redis.Value.Integer i => Log.info ("Counter " ^ key ^ " is now " ^ Int.toString i)
    | _ => Log.error "Unexpected reply from Redis"
  )

fun ping conn =
  sendCommand conn "PING" (
  fn Redis.Value.String "PONG" => Log.info "PONG"
    | v => Log.error ("Unexpected reply from Redis" ^ Redis.Value.toString v)
  )

fun showConnInfo (
  Redis.Value.BulkString (SOME "server") ::
  Redis.Value.BulkString (SOME "redis") ::
  Redis.Value.BulkString (SOME "version") ::
  Redis.Value.BulkString (SOME version) ::
   _
  ) =
  "Redis server version " ^ version
  | showConnInfo _ = "Unknown Redis server"

fun hello conn =
  sendCommand conn "HELLO" (
  fn Redis.Value.Array connInfo => Log.info ("Connected to Redis " ^ showConnInfo connInfo)
    | v => Log.error ("Unexpected reply from Redis" ^ Redis.Value.toString v)
  )

fun mustConnect host port =
  let
    val conn = Shrewdish.connect host port
  in
    case conn of
      NONE => raise Fail "Failed to connect to Redis"
    | SOME conn => conn
  end

fun main () =
  let
    val subcommand = CommandLine.arguments ()
    val endpoint = Option.getOpt (OS.Process.getEnv "REDIS_ENDPOINT", "localhost:6379")
    val (host, port) = parseAddr endpoint
    val _ = Log.info ("Connecting to Redis at " ^ endpoint ^ " ...")
    val conn = mustConnect host port
  in
    Log.debug "Connected\n";
    case subcommand of
      ["hello"] => hello conn
    | ["ping"] => ping conn
    | ["incr", key] => incrCounter conn key
    | _ => Log.error "Invalid subcommand"
  end
  handle ex => Log.error ("Error: " ^ General.exnMessage ex)

end
