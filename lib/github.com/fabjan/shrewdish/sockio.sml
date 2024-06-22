structure SockIO =
struct

type active_sock = (Socket.active Socket.stream) INetSock.sock

fun textReader (sock : active_sock) : TextPrimIO.reader =
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

fun textWriter (sock : active_sock) : TextPrimIO.writer =
  let
    fun writeVec slice =
      let
        val s = Substring.string slice
        val bytes = Byte.stringToBytes s
        val vec = Word8VectorSlice.full bytes
        val sent = Socket.sendVec (sock, vec)
      in
        Log.debug ("sockVecWriter: wrote " ^ s);
        sent
      end
    fun writeArr arr = writeVec (Substring.full (CharArraySlice.vector arr))
    fun close () = Socket.close sock
    val iodesc = Socket.ioDesc sock
  in
    TextPrimIO.WR {
      name = "sockVecWriter",
      chunkSize = 1500, (* MTU because why not? *)
      writeVec = SOME writeVec,
      writeArr = SOME writeArr,
      writeVecNB = NONE,
      writeArrNB = NONE,
      block = NONE,
      canOutput = NONE,
      getPos = NONE,
      setPos = NONE,
      endPos = NONE,
      verifyPos = NONE,
      close = close,
      ioDesc = SOME iodesc
    }
  end

fun textInStream (sock : active_sock) : TextIO.instream =
  let
    val reader = textReader sock
    val stream = TextIO.StreamIO.mkInstream (reader, "")
  in
    TextIO.mkInstream stream
  end

fun textOutStream (sock : active_sock) : TextIO.outstream =
  let
    val writer = textWriter sock
    val stream = TextIO.StreamIO.mkOutstream (writer, IO.LINE_BUF)
  in
    TextIO.mkOutstream stream
  end

end
