let split_lines input = input |> String.trim |> String.split_on_char '\n'
let print_list list fmt_type = Fmt.pr "%a\n" (Fmt.Dump.list fmt_type) list
let print_string_list list = print_list list Fmt.string
let print_char_list list = print_list list Fmt.char
let string_to_char_list str = str |> String.to_seq |> List.of_seq
