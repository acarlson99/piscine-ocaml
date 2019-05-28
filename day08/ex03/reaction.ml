(* TODO: finish maybe *)

class virtual reaction start_m end_m =
        object(self)
          method virtual get_start : (molecule * int) list = start_m
          method virtual get_result : (molecule * int) list = end_m
          method virtual balance : reaction
          method virtual is_balanced : bool
        end
