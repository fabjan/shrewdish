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

open Result

type t = {
  instream : TextIO.instream,
  outstream : TextIO.outstream
}

datatype connection_error = UnknownHost | ConnectionFailed of string

fun connect host port : (t, connection_error) result =
  case NetHostDB.getByName host of
    NONE => ERROR UnknownHost
  | SOME h =>
    let
      val addr = INetSock.toAddr (NetHostDB.addr h, port)
      val sock = INetSock.TCP.socket () : SockIO.active_sock
      val _ = Socket.connect (sock, addr)
      val instream = SockIO.textInStream sock
      val outstream = SockIO.textOutStream sock
    in
      OK {
        instream = instream,
        outstream = outstream
      }
    end
    handle e => ERROR (ConnectionFailed (General.exnMessage e))

datatype send_error = WriteFailed of string | ReadFailed of string

fun sendCommand (conn: t) (cmd : string list) : (Redis.Value.t, send_error) result =
  let
    val bulkStrings = List.map (fn s => Redis.Value.BulkString (SOME s)) cmd
    val message = Redis.Value.Array bulkStrings
    val outstream = #outstream conn
    val instream = #instream conn
  in
    Redis.Value.write outstream message;
    Log.debug "sendCommand: sent command, reading response";
    OK (Redis.Value.read instream)
  end
  handle Redis.Value.WriteError s => ERROR (WriteFailed s)
       | Redis.Value.ReadError s => ERROR (ReadFailed s)
       | Redis.Value.ParseError s => ERROR (ReadFailed s)

end
