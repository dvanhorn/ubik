open OUnit

let t () = assert_equal 1 2
let u () = assert_equal 1 1


let suite_trie = "Trie" >::: [
  "t" >:: t
]

let suite_lambda = "Lambda" >::: [
  "u" >:: u
]

let _ = Printf.printf "Trie:\n"
let _ = run_test_tt_main suite_trie
let _ = Printf.printf "\n\nLambda:\n"
let _ = run_test_tt_main suite_lambda

