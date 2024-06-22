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
  sendCommand conn ["INCR", key] (
  fn Redis.Value.Integer i => Log.info ("Counter " ^ key ^ " is now " ^ Int.toString i)
    | _ => Log.error "Unexpected reply from Redis"
  )

fun ping conn =
  sendCommand conn ["PING"] (
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
  sendCommand conn ["HELLO"] (
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

fun repeat 0 _ = ()
  | repeat n f = (f (); repeat (n - 1) f)

fun infiniteLoop conn =
  let
    val t0 = Time.now ()
    val _ = ping conn
    val now = Time.now ()
    val duration = Time.toMilliseconds (Time.-(now, t0))
  in
    Log.info ("Current time: " ^ Date.fmt "%Y-%m-%d %H:%M:%S" (Date.fromTimeLocal now));
    Log.info ("PING Duration: " ^ (LargeInt.toString duration) ^ " ms");
    OS.Process.sleep (Time.fromSeconds 1);
    infiniteLoop conn
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
    | ["loop"] => infiniteLoop conn
    | ["incr", key] => incrCounter conn key
    | ["incr", key, times] =>
      (case Int.fromString times of
        SOME n => repeat n (fn () => incrCounter conn key)
      | NONE => Log.error "Invalid number of times")
    | _ => Log.error "Invalid subcommand"
  end
  handle ex => Log.error ("Error: " ^ General.exnMessage ex)

end
