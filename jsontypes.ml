

type yaml =
  [
    `Null
  | `Bool of bool
  | `Float of (float [@equal fun x y -> 0 = compare x y])
  | `String of string
  | `A of yaml list
  | `O of (string * yaml) list
  ] [@@deriving show,eq]

type yaml_list = yaml list [@@deriving (show,eq)]
