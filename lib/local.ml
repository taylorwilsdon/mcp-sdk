(* Domain-local values, keyed by type witnesses. A scope belongs to one
   domain, so its table is only touched by fibers of that domain and needs no
   lock; [Eio.Lazy] settles fibers racing to make the same value. *)

type 'a t = { id : 'a Type.Id.t; make : Eio.Switch.t -> 'a }
type entry = E : 'a Type.Id.t * 'a Eio.Lazy.t -> entry
type scope = { sw : Eio.Switch.t; domain : Domain.id; values : (int, entry) Hashtbl.t }

let v make = { id = Type.Id.make (); make }
let scope sw = { sw; domain = Domain.self (); values = Hashtbl.create 8 }

let find (type a) s (l : a t) : a Eio.Lazy.t option =
  match Hashtbl.find_opt s.values (Type.Id.uid l.id) with
  | Some (E (id, value)) -> (
      match Type.Id.provably_equal l.id id with Some Type.Equal -> Some value | None -> None)
  | None -> None

let get s l =
  if Domain.self () <> s.domain then invalid_arg "Local.get: scope used from another domain";
  let value =
    match find s l with
    | Some value -> value
    | None ->
        let value = Eio.Lazy.from_fun ~cancel:`Restart (fun () -> l.make s.sw) in
        Hashtbl.replace s.values (Type.Id.uid l.id) (E (l.id, value));
        value
  in
  match Eio.Lazy.force value with
  | v -> v
  | exception (Eio.Cancel.Cancelled _ as e) -> raise e
  | exception e ->
      (* Forget the failure, so that the next request makes the value afresh. *)
      (match find s l with
      | Some current when current == value -> Hashtbl.remove s.values (Type.Id.uid l.id)
      | _ -> ());
      raise e
