def ListaCharsADistFrec(listaChar: List[Char]): List[(Char, Int)] = {
  def listaFrecsAux(listaChar: List[Char], listaFrecs: List[(Char, Int)]): List[(Char, Int)] = {
    listaChar match
      case Nil => listaFrecs.reverse
      case h :: tail =>
        if estaenListaFrec(h, listaFrecs) then listaFrecsAux(tail, actualizarFrec(h, listaFrecs))
        else listaFrecsAux(tail, (h, 1) :: listaFrecs)

      case null => throw new NoSuchElementException("Error inesperado")
  }

  def estaenListaFrec(c: Char, listaFrecs: List[(Char, Int)]): Boolean = {
    listaFrecs match
      case Nil => false
      case (cr, _) :: tail =>
        if cr == c then true
        else estaenListaFrec(c, tail)
      case null => throw new NoSuchElementException("Error inesperado")
  }

  def actualizarFrec(c: Char, listaFrecs: List[(Char, Int)]): List[(Char, Int)] = {
    listaFrecs match
      case (cr, frec) :: tail =>
        if cr == c then (cr, frec + 1) :: tail
        else (cr, frec) :: actualizarFrec(c, tail)
      case Nil => throw new NoSuchElementException(s"El nodo $c no se encuentra en la lista de frecuencias para actualizar")
      case null => throw new NoSuchElementException
  }
  listaFrecsAux(listaChar, Nil)
}
val chars: List[Char] = List('a', 'b', 'a', 'h', 'c', 'a', 'c')
val frecuencias: List[(Char,Int)] = ListaCharsADistFrec(chars)
println("Las frecuencias son" + frecuencias)