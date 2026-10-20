(** Domain-local values.

    A server is shared by every domain that serves it, so whatever a handler captures must be safe
    to use from any of them. Resources bound to one domain, such as a pool of upstream connections
    (Eio switches, and the sockets and fibers attached to them, cannot be used from another domain),
    are declared as a [Local.t] instead and read with {!Ctx.local}. Each serving domain makes its
    own value on first use, much as [Domain.DLS] does, under a switch of that domain that lives as
    long as it serves.

    {[
    let upstream = Local.v (fun sw -> Upstream.pool ~sw net)
    let call ctx args = Upstream.get (Ctx.local ctx upstream) args
    ]} *)

type 'a t

val v : (Eio.Switch.t -> 'a) -> 'a t
(** [v make] is a value made with [make sw] the first time a request on a domain needs it. If [make]
    raises, the exception goes to that request and the next one tries again. *)

(** {1 Transports} *)

type scope
(** Where one domain keeps its values. Transports make one per serving domain. *)

val scope : Eio.Switch.t -> scope
(** A scope of the current domain whose values live under [sw]. *)

val get : scope -> 'a t -> 'a
(** @raise Invalid_argument if called from a domain other than the scope's. *)
