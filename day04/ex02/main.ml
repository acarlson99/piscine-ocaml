let () =
  Printf.printf "%s\n" (Card.Color.toString Card.Color.Heart);
  let c = Card.newCard Card.Value.Jack Card.Color.Spade in
  Printf.printf "%s\n" (Card.toStringVerbose c)
