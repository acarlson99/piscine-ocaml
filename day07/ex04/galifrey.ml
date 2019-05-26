class galifrey =
object (self)
  val dalek = new Army.army (let rec makeBois n l =
                               if n = 0 then l
                               else makeBois (n-1) (new Dalek.dalek::l)
                             in
                             makeBois 45 [])
end
