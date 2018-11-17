module type Benchmarkable = sig
  val replace_all : string -> string
end

module LibRe : Benchmarkable = struct
  let re = Re.compile @@ Re.Pcre.re "[/:\\\\. (),'\"]+"
  let replace_all = Re.replace_string re ~by:"_"
end

module LibRe2 : Benchmarkable = struct
  let re = Re2.create_exn "[/:\\\\. (),'\"]+"
  let replace_all = Re2.rewrite_exn re ~template:"_"
end

module LibPcre : Benchmarkable = struct
  let re = Pcre.regexp "[/:\\\\. (),'\"]+"
  let replace_all s = Pcre.replace ~rex:re ~templ:"_" s
end

module LibAngstrom : Benchmarkable = struct
  open Angstrom

  let is_escape_char = function
    | '/' | ':' | '\\' | '.' | ' '
    | '(' | ')' | ',' | '"' | '\'' -> true
    | _ -> false

  let good = take_till is_escape_char
  let bad = take_while1 is_escape_char
  let p = sep_by1 bad good <* end_of_input

  let replace_all s = match parse_string p s with
    | Result.Ok xs -> String.concat "_" xs
    | Result.Error msg -> failwith msg
end

let main () =
  let open Core in
  let open Core_bench.Std in
  let rec gulp_all lines =
    match In_channel.(input_line stdin) with
    | Some line -> gulp_all (line :: lines)
    | None -> lines
  in
  let bench f lines =
    List.iter lines ~f:(fun x -> ignore @@ f x)
  in
  let lines = gulp_all [] in
  Core.Command.run (Bench.make_command [
      Bench.Test.create ~name:"Re" (fun () ->
          bench LibRe.replace_all lines);
      Bench.Test.create ~name:"Pcre" (fun () ->
            bench LibPcre.replace_all lines);
      Bench.Test.create ~name:"Re2" (fun () ->
            bench LibRe2.replace_all lines);
      Bench.Test.create ~name:"Angstrom" (fun () ->
            bench LibAngstrom.replace_all lines);
    ])
let () = main ()
