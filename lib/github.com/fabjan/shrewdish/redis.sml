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

structure Redis =
struct

type substring = Substring.substring

structure Value =
struct

(* RESP2 only *)
datatype t =
    String of string
  | Error of string
  | Integer of int
  | BulkString of string option
  | Array of t list

fun toString (String s) = s
  | toString (Error s) = "ERROR: " ^ s
  | toString (Integer i) = Int.toString i
  | toString (BulkString NONE) = "<null>"
  | toString (BulkString (SOME s)) = s
  | toString (Array l) = "[" ^ String.concatWith ", " (map toString l) ^ "]"

fun encode (String s) = "+" ^ s ^ "\r\n"
  | encode (Error s) = "-" ^ s ^ "\r\n"
  | encode (Integer i) = ":" ^ Int.toString i ^ "\r\n"
  | encode (BulkString NONE) = "$-1\r\n"
  | encode (BulkString (SOME s)) = "$" ^ Int.toString (size s) ^ "\r\n" ^ s ^ "\r\n"
  | encode (Array l) = "*" ^ Int.toString (length l) ^ "\r\n" ^ String.concat (map encode l)

exception ReadError of string
exception WriteError of string
exception ParseError of string

fun write (stream: TextIO.outstream) (v: t) =
  TextIO.output (stream, encode v)
  handle exn => raise WriteError (General.exnMessage exn)

fun read (stream: TextIO.instream) : t =
  let
    infix 5 |>
    fun x |> f = f x

    fun readLine () =
      case TextIO.inputLine stream of
        NONE => raise ReadError "unexpected end of input reading line"
      | SOME s =>
        if String.isSuffix "\r\n" s
        then String.substring (s, 0, size s - 2)
        else raise ParseError "missing CRLF after line"

    fun readInt () =
      case readLine () |> Int.fromString of
        NONE => raise ParseError "invalid integer"
      | SOME i => i

    fun readBulkString 0 = BulkString NONE
      | readBulkString len =
        let
          val s = TextIO.inputN (stream, len)
          val crlf = TextIO.inputN (stream, 2)
          val received = String.size s
        in
          if received <> len
          then raise ParseError ("bulk string expected " ^ Int.toString len ^ " bytes, got " ^ Int.toString received)
          else if crlf <> "\r\n"
          then raise ParseError "expected CRLF after bulk string"
          else BulkString (SOME s)
        end

    fun readArray acc 0 = Array (List.rev acc)
      | readArray acc len = readArray ((read stream) :: acc) (len - 1)

  in
    case TextIO.input1 stream of
      NONE => raise ReadError "unexpected end of input reading type"
    | SOME #"+" => readLine () |> String
    | SOME #"-" => readLine () |> Error
    | SOME #":" => readInt () |> Integer
    | SOME #"$" => readInt () |> readBulkString
    | SOME #"*" => readInt () |> readArray []
    | SOME c => raise ParseError ("unexpected character reading type: '" ^ Char.toString c ^ "'")
  end
  handle exn => raise ReadError (General.exnMessage exn)

end

end
