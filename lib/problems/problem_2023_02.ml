open Base

let year = 2023
let day = 2

module Rgb = struct
  type t = int * int * int

  let power (red, green, blue) = red * green * blue
end

type color_limits = { red : int; green : int; blue : int }

let limits = { red = 12; green = 13; blue = 14 }

module Color = struct
  type t = Red | Green | Blue

  let of_string = function
    | "red" -> Red
    | "green" -> Green
    | "blue" -> Blue
    | _ -> failwith "Invalid color"
  ;;

  let to_string = function Red -> "red" | Green -> "green" | Blue -> "blue"
  let pp ppf color = Fmt.string ppf (to_string color)
end

module Color_count = struct
  type t = { color : Color.t; count : int }

  let pp ppf { color; count } =
    Fmt.pf ppf "%s: %d" (Color.to_string color) count
  ;;
end

module Hand = struct
  type t = Color_count.t list

  let to_rgb (hand : t) : Rgb.t =
    hand
    |> List.fold_left ~init:(0, 0, 0)
         ~f:(fun (r, g, b) { Color_count.color; count } ->
           match color with
           | Red -> (count, g, b)
           | Green -> (r, count, b)
           | Blue -> (r, g, count))
  ;;

  let pp ppf hand =
    Fmt.pf ppf "[%a]" Fmt.(list ~sep:(const string ", ") Color_count.pp) hand
  ;;
end

module Game = struct
  type t = { id : int; hands : Hand.t list }

  let pp ppf { id; hands } =
    Fmt.pf ppf "Game %d: %a" id
      Fmt.(list ~sep:(const string "; ") Hand.pp)
      hands
  ;;
end

module Parser = struct
  open Angstrom

  let is_digit = function '0' .. '9' -> true | _ -> false

  let digit =
    take_while1 is_digit >>| Int.of_string <?> "digit: Parse one or more digits"
  ;;

  let game_id =
    let* _ = string "Game " in
    let* id = digit in
    let* _ = string ": " in
    return id <?> "game_id: Parse a Game's id"
  ;;

  let is_whitespace char = Char.equal char ' '

  let color =
    string "red" <|> string "green" <|> string "blue" >>| Color.of_string
    <?> "color: Parse a color"
  ;;

  let color_count =
    let* count = digit in
    let* _ = skip is_whitespace in
    let* color' = color in
    return Color_count.{ color = color'; count }
    <?> "color_count: Parse a color and count"
  ;;

  let hand = sep_by1 (string ", ") color_count <?> "hand: Parse a hand"
  let round = sep_by1 (string "; ") hand <?> "round: Parse a round"

  let game =
    let* id = game_id in
    let* hands = round in
    return Game.{ id; hands } <?> "game: Parse a game"
  ;;

  let games = sep_by1 (char '\n') game <?> "games: Parse a list of games"
  let parse input = Angstrom.parse_string ~consume:All games input
end

module Part_1 = struct
  let color_count_exceeds_limit ({ color; count } : Color_count.t) =
    match color with
    | Red -> count > limits.red
    | Green -> count > limits.green
    | Blue -> count > limits.blue
  ;;

  let hand_exceeds_limits (hand : Hand.t) =
    let filtered_hand = List.filter ~f:color_count_exceeds_limit hand in
    List.length filtered_hand > 0
  ;;

  let game_exceeds_limits (game : Game.t) =
    let filtered_hands = List.filter ~f:hand_exceeds_limits game.hands in
    List.length filtered_hands > 0
  ;;

  let sum_game_ids sum (game : Game.t) =
    let filtered_hands = List.filter ~f:hand_exceeds_limits game.hands in
    if List.length filtered_hands = 0 then sum + game.id else sum
  ;;

  let run (input : string) : (string, string) Result.t =
    Parser.parse input
    |> Result.map ~f:(List.fold_left ~init:0 ~f:sum_game_ids)
    |> Result.map ~f:Int.to_string
  ;;
end

module Part_2 = struct
  let max_rgb_of_hands (r, g, b) (hand : Hand.t) =
    let r', g', b' = Hand.to_rgb hand in
    (Int.max r r', Int.max g g', Int.max b b')
  ;;

  let find_minimum_rgb_count_in_hands (hands : Hand.t list) : Rgb.t =
    List.fold_left ~init:(0, 0, 0) ~f:max_rgb_of_hands hands
  ;;

  let sum_power_of_games sum (game : Game.t) =
    sum + (Rgb.power @@ find_minimum_rgb_count_in_hands game.hands)
  ;;

  let run (input : string) : (string, string) Result.t =
    Parser.parse input
    |> Result.map ~f:(List.fold_left ~init:0 ~f:sum_power_of_games)
    |> Result.map ~f:Int.to_string
  ;;
end
