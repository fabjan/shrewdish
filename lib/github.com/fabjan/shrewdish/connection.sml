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

type active_sock = (Socket.active Socket.stream) INetSock.sock

fun connect host port : active_sock option =
  case NetHostDB.getByName host of
    NONE => (
      Log.error ("connect: host " ^ host ^ " not found");
      NONE
    )
  | SOME h =>
    let
      val addr = INetSock.toAddr (NetHostDB.addr h, port)
      val sock = INetSock.TCP.socket () : active_sock
      val _ = Socket.connect (sock, addr)
    in
      SOME sock
    end
    handle e => (
      Log.error ("connect: " ^ General.exnMessage e);
      NONE
    )

fun sockVecReader (sock : active_sock) : TextPrimIO.reader =
  let
    fun readVec len =
      let
        val bytes = Socket.recvVec (sock, len)
        val s = Byte.bytesToString bytes
      in
        Log.debug ("sockVecReader: read " ^ s);
        s
      end
    fun close () = Socket.close sock
    val iodesc = Socket.ioDesc sock
  in
    TextPrimIO.RD {
      name = "sockVecReader",
      chunkSize = 1500, (* MTU because why not? *)
      readVec = SOME readVec,
      readArr = NONE,
      readVecNB = NONE,
      readArrNB = NONE,
      block = NONE,
      canInput = NONE,
      avail = fn () => NONE,
      getPos = NONE,
      setPos = NONE,
      endPos = NONE,
      verifyPos = NONE,
      close = close,
      ioDesc = SOME iodesc
    }
  end

fun sockStream (sock : active_sock) : TextIO.instream =
  let
    val reader = sockVecReader sock
    val stream = TextIO.StreamIO.mkInstream (reader, "")
  in
    TextIO.mkInstream stream
  end

fun sendCommand (sock : active_sock) (cmd : string list) : Redis.Value.t option =
  let
    fun pushAllBytes _ 0 = ()
      | pushAllBytes bytes n =
        let
          val m = Socket.sendVec (sock, bytes)
          val bytes = Word8VectorSlice.subslice (bytes, m, NONE)
        in
          pushAllBytes bytes (n - m)
        end
    val bulkStrings = List.map (fn s => Redis.Value.BulkString (SOME s)) cmd
    val bytes = Byte.stringToBytes (Redis.Value.encode (Redis.Value.Array bulkStrings))
  in
    pushAllBytes (Word8VectorSlice.full bytes) (Word8Vector.length bytes);
    Log.debug "sendCommand: sent command, reading response";
    Redis.Value.decode (sockStream sock)
  end
  handle e => (
    Log.error ("sendCommand: " ^ General.exnMessage e);
    NONE
  )

end
