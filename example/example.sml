local

open Shrewdish

structure Log =
struct
  fun debug msg = Shrewdish.Log.debug ("[Example] " ^ msg)
  fun info msg = Shrewdish.Log.info ("[Example] " ^ msg)
  fun warn msg = Shrewdish.Log.warn ("[Example] " ^ msg)
  fun error msg = Shrewdish.Log.error ("[Example] " ^ msg)
end

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
    Result.OK v => andThen v
  | Result.ERROR (Connection.WriteFailed s) => Log.error ("Write failed: " ^ s)
  | Result.ERROR (Connection.ReadFailed s) => Log.error ("Read failed: " ^ s)
  handle exn => Log.error ("Unhandled exception: " ^ General.exnMessage exn)

fun incrCounter conn key =
  sendCommand conn ["INCR", key] (
  fn Redis.Value.Integer i => Log.info ("Counter " ^ key ^ " is now " ^ Int.toString i)
    | _ => Log.error "Unexpected reply from Redis"
  )

fun ping conn =
  let
    val t0 = Time.now ()
    fun stop () = Time.toMilliseconds (Time.-(Time.now (), t0))
  in
    sendCommand conn ["PING"] (
      fn Redis.Value.String "PONG" => Log.info ("ping: " ^ LargeInt.toString (stop ()) ^ "ms")
      | _ => Log.error "Unexpected reply from Redis"
    )
  end

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
  fn Redis.Value.Array connInfo => Log.info ("Connected to " ^ showConnInfo connInfo)
    | v => Log.error ("Unexpected reply from Redis" ^ Redis.Value.toString v)
  )

fun mustConnect host port =
  case Shrewdish.connect host port of
    Result.ERROR (Connection.UnknownHost h) => raise Fail ("Cannot connect to Redis: unknown host: " ^ h)
  | Result.ERROR (Connection.ConnectionFailed s) => raise Fail ("Cannot connect to Redis: " ^ s)
  | Result.OK conn => conn

fun repeat 0 _ = ()
  | repeat n f = (f (); repeat (n - 1) f)

fun infiniteLoop conn = (
  ping conn;
  OS.Process.sleep (Time.fromSeconds 1);
  infiniteLoop conn
)

fun main () =
  let
    val subcommand = CommandLine.arguments ()
    val endpoint = Option.getOpt (OS.Process.getEnv "REDIS_ENDPOINT", "localhost:6379")
    val (host, port) = parseAddr endpoint
    val _ = Log.info ("Connecting to Redis at " ^ endpoint ^ " ...")
    val conn = mustConnect host port
  in
    Log.debug "Connected";
    case subcommand of
      ["hello"] => hello conn
    | ["ping"] => ping conn
    | ["loop"] => infiniteLoop conn
    | ["get", key] => sendCommand conn ["GET", key] (
        fn Redis.Value.BulkString (SOME value) => Log.info ("Value of " ^ key ^ " is " ^ value)
        | Redis.Value.BulkString NONE => Log.info ("Key " ^ key ^ " not found")
        | _ => Log.error "Unexpected reply from Redis"
      )
    | ["incr", key] => incrCounter conn key
    | ["incr", key, times] =>
      (case Int.fromString times of
        SOME n => repeat n (fn () => incrCounter conn key)
      | NONE => Log.error "Invalid number of times")
    | _ => Log.error "Invalid subcommand"
  end
  handle ex => Log.error ("Exiting: " ^ General.exnMessage ex)

end
