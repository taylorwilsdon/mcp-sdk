(** Model Context Protocol, revision 2026-07-28, for OCaml 5 and Eio.

    - Define primitives with {!Tool}, {!Resource} and {!Prompt}; describe tool arguments with
      {!Schema}.
    - Assemble them into a {!Server} and serve it with {!Stdio}, or over Streamable HTTP with the
      [mcp-sdk.http] library.
    - Ask the client for more with {!Input} (multi round-trip requests) and read what came back
      through {!Ctx}.
    - Talk to servers with {!Client}. *)

module Json = Json
module Id = Id
module Error = Error
module Wire = Wire
module Proto = Proto
module Schema = Schema
module Input = Input
module Ctx = Ctx
module Tool = Tool
module Resource = Resource
module Prompt = Prompt
module Server = Server
module Stdio = Stdio
module Client = Client
