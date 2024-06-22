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

fun write (stream: TextIO.outstream) (v: t) = TextIO.output (stream, encode v)

fun read (stream: TextIO.instream) : t option =
  let
    fun fail reason = (
      Log.error ("decode failed: " ^ reason ^ "\n");
      NONE
    )

    fun readLine () =
      case TextIO.inputLine stream of
        NONE => fail "unexpected end of input"
      | SOME s =>
        if String.isSuffix "\r\n" s
        then SOME (String.substring (s, 0, size s - 2))
        else fail "expected CRLF"

    fun readBulkString 0 = SOME (BulkString NONE)
      | readBulkString len =
        let
          val s = TextIO.inputN (stream, len)
          val crlf = TextIO.inputN (stream, 2)
          val read = String.size s
        in
          if read <> len
          then fail ("expected " ^ Int.toString len ^ " bytes, got " ^ Int.toString read)
          else if crlf <> "\r\n"
          then fail "expected CRLF"
          else SOME (BulkString (SOME s))
        end

    fun readArray acc 0 = SOME (Array (List.rev acc))
      | readArray acc len =
        case read stream of
          NONE => fail "unexpected end of input"
        | SOME v => readArray (v :: acc) (len - 1)

    infix 5 |>
    fun x |> f = f x

    fun readValue () =
      case TextIO.input1 stream of
        NONE => fail "unexpected end of input"
      | SOME c =>
        case c of
          #"+" => readLine () |> Option.map String
        | #"-" => readLine () |> Option.map Error
        | #":" => readLine () |> Option.mapPartial Int.fromString |> Option.map Integer
        | #"$" => readLine () |> Option.mapPartial Int.fromString |> Option.mapPartial readBulkString
        | #"*" => readLine () |> Option.mapPartial Int.fromString |> Option.mapPartial (readArray [])
        | _ => fail "unexpected character"
  in
    readValue ()
  end

end

end
