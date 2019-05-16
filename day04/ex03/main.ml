let () =
  let d = Deck.newDeck () in
  List.iter (Printf.printf "%s ") (Deck.toStringListVerbose d);
  Printf.printf "\n";
  List.iter (Printf.printf "%s ") (Deck.toStringList d);
  Printf.printf "\n";
  let d2 = Deck.drawCard d in
  List.iter (Printf.printf "%s ") (Deck.toStringList (snd d2));
  Printf.printf "%s\n" (Deck.Card.toStringVerbose (fst d2))
