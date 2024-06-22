(*
 * Copyright 2024 Fabian Bergström
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

structure Connection =
struct

type t = {
  host : string,
  port : int,
  instream : TextIO.instream,
  outstream : TextIO.outstream
}

fun connect host port : t option =
  case NetHostDB.getByName host of
    NONE => (
      Log.error ("connect: host " ^ host ^ " not found");
      NONE
    )
  | SOME h =>
    let
      val addr = INetSock.toAddr (NetHostDB.addr h, port)
      val sock = INetSock.TCP.socket () : SockIO.active_sock
      val _ = Socket.connect (sock, addr)
      val instream = SockIO.textInStream sock
      val outstream = SockIO.textOutStream sock
    in
      SOME {
        host = host,
        port = port,
        instream = instream,
        outstream = outstream
      }
    end
    handle e => (
      Log.error ("connect: " ^ General.exnMessage e);
      NONE
    )

fun sendCommand (conn: t) (cmd : string list) : Redis.Value.t option =
  let
    val bulkStrings = List.map (fn s => Redis.Value.BulkString (SOME s)) cmd
    val message = Redis.Value.Array bulkStrings
    val outstream = #outstream conn
    val instream = #instream conn
  in
    Redis.Value.write outstream message;
    Log.debug "sendCommand: sent command, reading response";
    Redis.Value.read instream
  end
  handle e => (
    Log.error ("sendCommand: " ^ General.exnMessage e);
    NONE
  )

end
