open Irmin.Type
open Lwt.Syntax
open Core

let foo = List.map (fun n -> Range.(n.value))
let unlocated node = Range.{ loc = None; value = node }

open Sem

module Ranges = struct
  open Asai
  open Range
  open Irmin.Type

  let string_source =
    record "string_source" (fun title content -> { title; content })
    |+ field "title" (option string) (fun s -> s.title)
    |+ field "content" string (fun s -> s.content)
    |> sealr

  let source =
    variant "source" (fun file string -> function
      | `File s -> file s | `String s -> string s)
    |~ case1 "`File" string (fun s -> `File s)
    |~ case1 "`String" string_source (fun s -> `String s)
    |> sealv

  let position =
    record "position" (fun source offset start_of_line line_num ->
        { source; offset; start_of_line; line_num })
    |+ field "source" source (fun t -> t.source)
    |+ field "offset" int (fun t -> t.offset)
    |+ field "start_of_line" int (fun t -> t.offset)
    |+ field "line_num" int (fun t -> t.offset)
    |> sealr

  let range : Range.t ty =
    (* required stuff for serializing abstract types *)
    let pp formatter a = () in
    let b =
      { source = `File "todo"; offset = 0; start_of_line = 0; line_num = 0 }
    in
    let e =
      { source = `File "todo"; offset = 0; start_of_line = 0; line_num = 0 }
    in
    let of_string _ = Ok (Range.make (b, e)) in
    let encode _ _ = () in
    let decode _ = Ok (Range.make (b, e)) in
    let encode_bin : _ encode_bin = fun _ _ -> () in
    let decode_bin _ _ = Range.make (b, e) in
    let unimplemented_size_of = failwith "todo" in
    let size_of : _ size_of = unimplemented_size_of in
    let equal _ _ = false in
    let compare _ _ = 0 in
    let short_hash ?seed a = 0 in
    let pre_hash _ _ = () in
    abstract ~pp ~of_string ~json:(encode, decode)
      ~bin:(encode_bin, decode_bin, size_of)
      ~equal ~compare ~short_hash ~pre_hash ()
end

let prim =
  enum "prim"
    [
      ("P", `P);
      ("Ol", `Ol);
      ("Ul", `Ul);
      ("Li", `Li);
      ("Em", `Em);
      ("Strong", `Strong);
      ("Code", `Code);
      ("Blockquote", `Blockquote);
      ("Pre", `Pre);
    ]

let date : Prelude.Date.t ty =
  let pp formatter a = () in
  let of_string s = Ok (Prelude.Date.parse s) in
  let encode _ _ = () in
  let decode _ = Ok (Prelude.Date.parse "todo") in
  let encode_bin : _ encode_bin = fun _ _ -> () in
  let decode_bin _ _ = Prelude.Date.parse "todo" in
  let unimplemented_size_of = failwith "todo" in
  let size_of : _ size_of = unimplemented_size_of in
  let equal _ _ = false in
  let compare _ _ = 0 in
  let short_hash ?seed a = 0 in
  let pre_hash _ _ = () in
  abstract ~pp ~of_string ~json:(encode, decode)
    ~bin:(encode_bin, decode_bin, size_of)
    ~equal ~compare ~short_hash ~pre_hash ()

module Tree : Irmin.Contents.S with type t = Sem.tree = struct
  type t = Sem.tree

  let math_mode = enum "math_mode" [ ("inline", Inline); ("display", Display) ]

  let sem_node : Sem.node ty =
    variant "node" (fun text -> function Text s -> text s)
    |~ case1 "Text" string (fun s -> Text s)
    |> sealv

  let located_sem_node : Sem.node Range.located ty =
    let open Asai in
    let open Range in
    record "located_sem_node" (fun loc value -> { loc; value })
    |+ field "loc" (option Ranges.range) (fun t -> None)
    |+ field "value" sem_node (fun t -> t.value)
    |> sealr

  let transclusion_opts =
    record "transclusion_opts"
      (fun
        toc
        show_heading
        show_metadata
        title_override
        taxon_override
        expanded
        numbered
      ->
        {
          toc;
          show_heading;
          show_metadata;
          title_override;
          taxon_override;
          expanded;
          numbered;
        })
    |+ field "toc" bool (fun t -> t.toc)
    |+ field "show_heading" bool (fun t -> t.show_heading)
    |+ field "show_metadata" bool (fun t -> t.show_metadata)
    |+ field "title_override"
         (option (list located_sem_node))
         (fun t -> t.title_override)
    |+ field "taxon_override" (option string) (fun t -> t.taxon_override)
    |+ field "expanded" bool (fun t -> t.expanded)
    |+ field "numbered" bool (fun t -> t.numbered)
    |> sealr

  let frontmatter =
    record "frontmatter"
      (fun
        title
        taxon
        authors
        contributors
        dates
        addr
        metas
        tags
        parent
        source_path
        number
      ->
        {
          title;
          taxon;
          authors;
          contributors;
          (* let* loc = t.loc in *)
          (* Some (loc, `Msg "This shouldn't be here")) *)
          dates;
          addr;
          metas;
          tags;
          parent;
          source_path;
          number;
        })
    |+ field "title" (option (list located_sem_node)) (fun t -> t.title)
    |+ field "taxon" (option string) (fun t -> t.taxon)
    |+ field "authors" (list string) (fun t -> t.authors)
    |+ field "contributors" (list string) (fun t -> t.contributors)
    |+ field "dates" (list date) (fun t -> t.dates)
    |+ field "addr" (option string) (fun t -> t.addr)
    |+ field "metas"
         (list (pair string (list located_sem_node)))
         (fun t -> t.metas)
    |+ field "tags" (list string) (fun t -> t.tags)
    |+ field "parent" (option string) (fun t -> t.parent)
    |+ field "source_path" (option string) (fun t -> t.source_path)
    |+ field "number" (option string) (fun t -> t.number)
    |> sealr

  let tree : Sem.tree ty =
    record "tree" (fun fm body : Sem.tree -> { fm; body })
    |+ field "fm" frontmatter (fun t -> t.fm)
    |+ field "body" (list located_sem_node) (fun (t : Sem.tree) -> t.body)
       (* without annotation thinks that t is obj_method *)
    |> sealr

  let t = tree
  let merge = Irmin.Merge.(option (idempotent t))

  (*
and sem_node =
  variant "node"
    (fun
      text
      transclude
      subtree
      query
      (* link *)
        xml_tag
      unresolved
      math
      embed_tex
      img
      if_tex
      prim
      object_
      ref
    -> function
    | Text str -> text
    | Transclude (x, y) -> transclude (x, y)
    | Subtree (x, y) -> subtree (x, y)
    | Query (x, y) -> query (x, y)
    (* | Link { dest; title; modifier } -> link { dest; title; modifier } *)
    | Xml_tag (_, _, _) -> xml_tag
    | Unresolved str -> unresolved
    | Math (_, _) -> math
    | Embed_tex _ -> embed_tex
    | Img _ -> img
    | If_tex (x, y) -> if_tex (x, y)
    | Prim (x, y) -> prim (x, y)
    | Object _ -> object_
    | Ref _ -> ref)
  |~ case1 "Text" string (fun s -> Text s)
  |~ case1 "Transclude" (pair transclusion_opts tree) (fun (x, y) ->
         Transclude (x, y))
  |~ case1 "Subtree" (pair transclusion_opts tree) (fun (x, y) ->
         Subtree (x, y))
  |~ case1 "Query" (pair transclusion_opts query) (fun (x, y) -> Query (x, y))
  (* |~ case1 "Link" string (fun s -> Text s) *)
  |~ case1 "Xml_tag"
       (triple string (list @@ pair string node_list) t)
       (fun s -> Text s)
  |~ case1 "Unresolved" string (fun s -> Unresolved s)
  |~ case1 "Math" (pair math_mode t) (fun (x, y) -> Math (x, y))
  |~ case1 "Embed_tex" string (fun s -> Embed_tex s)
  |~ case1 "Img" string (fun { path } -> Img { path })
  |~ case1 "If_tex" string (fun s -> If_tex s)
  |~ case1 "Prim" string (fun s -> Prim s)
  |~ case1 "Object_" string (fun s -> Object s)
  |~ case1 "Ref" string (fun s -> Ref s)
  |> sealv

  *)
end

module Git_store = Irmin_git_unix.FS.KV (Irmin.Contents.String)

let config = Irmin_git.config ~bare:true "/tmp/irmin/test"
let repo = Git_store.Repo.v config

module Git_info = Irmin_unix.Info (Git_store.Info)

let info message = Git_info.v ~author:"Exampe" "%s" message

let main_branch config =
  let* repo = Git_store.Repo.v config in
  Git_store.main repo

let main =
  let* t = main_branch config in
  let* () =
    Git_store.set_exn t [ "a"; "b"; "c" ] "Hello, Irmin!"
      ~info:(info "my first commit")
  in
  let+ s = Git_store.get t [ "a"; "b"; "c" ] in
  assert (s = "Hello, Irmin!")

let () = Lwt_main.run main
