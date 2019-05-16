let () =
  Printf.printf "%s\n" (Card.Color.toString Card.Color.Heart);
  let c = Card.newCard Card.Value.Jack Card.Color.Spade in
  Printf.printf "%s\n" (Card.toStringVerbose c);
  Printf.printf "%b\n" (Card.isSpade c);
  Printf.printf "%b\n" (Card.isClub c);
  Printf.printf "%d\n" (Card.compare c c);
  let cn = Card.newCard Card.Value.T4 Card.Color.Diamond in
  Printf.printf "%d\n" (Card.compare cn c);
  Printf.printf "%s\n" (Card.toStringVerbose (Card.max c cn));
  Printf.printf "%s\n" (Card.toStringVerbose (Card.min c cn));
  Printf.printf "%s\n" (Card.toStringVerbose (Card.best [cn;c]));
  Printf.printf "%s\n" (Card.toString (Card.best [cn;c]))
