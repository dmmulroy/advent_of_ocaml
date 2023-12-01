let year = 2023
let day = 1

let parse_first_and_last_digit value =
  let value_length = String.length value in
  let first_digit = String.get value 0 |> Char.escaped in
  let last_digit = String.get value (value_length - 1) |> Char.escaped in
  first_digit ^ last_digit |> int_of_string
;;

module Part_1 = struct
  let reduce_char_list acc char =
    Char.escaped char |> int_of_string_opt |> Option.map string_of_int
    |> Option.fold ~none:acc ~some:(String.cat acc)
  ;;

  let parse_calibration_value (value : string) =
    String.fold_left reduce_char_list "" value |> parse_first_and_last_digit
  ;;

  let run (input : string) : (string, string) result =
    Utils.split_lines input
    |> List.fold_left
         (fun acc raw_calibration_value ->
           acc + parse_calibration_value raw_calibration_value)
         0
    |> string_of_int |> Result.ok
  ;;
end

module Part_2 = struct
  let int_of_char_list = function
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

  type acc = {
    initial : string;
    value : string;
    index : int;
    digit_resume_index : int;
  }

  let parse_char char =
    char |> Char.escaped |> int_of_string_opt |> Option.map string_of_int
  ;;

  let is_digit char = char |> parse_char |> Option.is_some

  let parse_first_and_last_digit value =
    let value_length = String.length value in
    let first_digit = String.get value 0 |> Char.escaped in
    let last_digit = String.get value (value_length - 1) |> Char.escaped in
    first_digit ^ last_digit |> int_of_string
  ;;

  let get_remaining_characters index str =
    String.sub str index (String.length str - index)
    |> Utils.string_to_char_list
  ;;

  let reduce_char_list acc char =
    if acc.index = acc.digit_resume_index && is_digit char then
      {
        acc with
        value = acc.value ^ Char.escaped char;
        index = acc.index + 1;
        digit_resume_index = acc.index + 1;
      }
    else
      let parsed_int_opt =
        get_remaining_characters acc.index acc.initial |> int_of_char_list
      in
      match parsed_int_opt with
      | None ->
          let next_digit_resume_index =
            if acc.index = acc.digit_resume_index then acc.index + 1
            else acc.digit_resume_index
          in
          {
            acc with
            value = acc.value;
            index = acc.index + 1;
            digit_resume_index = next_digit_resume_index;
          }
      | Some parsed_int ->
          {
            acc with
            value = acc.value ^ string_of_int parsed_int;
            index = acc.index + 1;
            digit_resume_index = acc.index + int_to_word_length parsed_int;
          }
  ;;

  let parse_calibration_value (raw_value : string) =
    Utils.string_to_char_list raw_value
    |> List.fold_left reduce_char_list
         { initial = raw_value; value = ""; index = 0; digit_resume_index = 0 }
    |> fun { value; _ } -> parse_first_and_last_digit value
  ;;

  let run (input : string) : (string, string) result =
    Utils.split_lines input
    |> List.fold_left
         (fun acc raw_calibration_value ->
           acc + parse_calibration_value raw_calibration_value)
         0
    |> string_of_int |> Result.ok
  ;;
end
