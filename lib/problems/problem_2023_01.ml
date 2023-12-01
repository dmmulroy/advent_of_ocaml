let year = 2023
let day = 1

module Utils = struct
  let split_lines input = input |> String.trim |> String.split_on_char '\n'
  let print_string_list list = Fmt.pr "%a\n" (Fmt.Dump.list Fmt.string) list
  let print_char_list list = Fmt.pr "%a\n" (Fmt.Dump.list Fmt.char) list
  let string_to_char_list str = str |> String.to_seq |> List.of_seq
end

module Part_1 = struct
  let parse_calibration_value (value : string) =
    let parsed_value =
      String.fold_left
        (fun acc char ->
          let partial_value =
            char |> Char.escaped |> int_of_string_opt
            |> Option.map string_of_int
          in
          match partial_value with
          | None -> acc
          | Some partial_value -> acc ^ partial_value)
        "" value
    in
    let parsed_value_length = String.length parsed_value in
    let first_digit = String.get parsed_value 0 |> Char.escaped in
    let last_digit =
      String.get parsed_value (parsed_value_length - 1) |> Char.escaped
    in
    first_digit ^ last_digit |> int_of_string
  ;;

  let run (input : string) : (string, string) result =
    input |> Utils.split_lines
    |> List.fold_left
         (fun acc raw_calibration_value ->
           acc + parse_calibration_value raw_calibration_value)
         0
    |> string_of_int |> Result.ok
  ;;
end

module Part_2 = struct
  let int_of_chars = function
    | 'o' :: 'n' :: 'e' :: _ -> Some 1
    | 't' :: 'w' :: 'o' :: _ -> Some 2
    | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ -> Some 3
    | 'f' :: 'o' :: 'u' :: 'r' :: _ -> Some 4
    | 'f' :: 'i' :: 'v' :: 'e' :: _ -> Some 5
    | 's' :: 'i' :: 'x' :: _ -> Some 6
    | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ -> Some 7
    | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ -> Some 8
    | 'n' :: 'i' :: 'n' :: 'e' :: _ -> Some 9
    | _ -> None
  ;;

  let int_to_word_length = function
    | 1 -> 3
    | 2 -> 3
    | 3 -> 5
    | 4 -> 4
    | 5 -> 4
    | 6 -> 3
    | 7 -> 5
    | 8 -> 5
    | 9 -> 4
    | _ -> failwith "Invalid number"
  ;;

  type acc = { value : string; index : int; resume_index : int }

  let parse_calibration_value (raw_value : string) =
    let { value; _ } =
      raw_value |> Utils.string_to_char_list
      |> List.fold_left
           (fun { value; index; resume_index } char ->
             if index != resume_index then
               { value; index = index + 1; resume_index }
             else
               let parsed_char =
                 char |> Char.escaped |> int_of_string_opt
                 |> Option.map string_of_int
               in
               match parsed_char with
               | Some parsed_char ->
                   {
                     value = value ^ parsed_char;
                     index = index + 1;
                     resume_index = index + 1;
                   }
               | None ->
                   String.sub raw_value index (String.length raw_value - index)
                   |> Utils.string_to_char_list |> int_of_chars
                   |> Option.fold
                        ~none:
                          { value; index = index + 1; resume_index = index + 1 }
                        ~some:(fun (parsed : int) ->
                          {
                            value = value ^ string_of_int parsed;
                            index = index + 1;
                            resume_index = int_to_word_length parsed + index;
                          }))
           { value = ""; index = 0; resume_index = 0 }
    in
    let value_length = String.length value in
    let first_digit = String.get value 0 |> Char.escaped in
    let last_digit = String.get value (value_length - 1) |> Char.escaped in
    first_digit ^ last_digit |> int_of_string
  ;;

  let run (input : string) : (string, string) result =
    input |> Utils.split_lines
    |> List.fold_left
         (fun acc raw_calibration_value ->
           acc + parse_calibration_value raw_calibration_value)
         0
    |> string_of_int |> Result.ok
  ;;
end
