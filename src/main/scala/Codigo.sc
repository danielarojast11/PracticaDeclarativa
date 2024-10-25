
//Clase Arbol
trait ArbolHuffman

//Clases Nodo y Rama
  case class NodoHuffman(caracter: Char, var pesoN: Int) extends ArbolHuffman
  case class RamaHuffman(nodoIzq: ArbolHuffman, nodoDch: ArbolHuffman) extends ArbolHuffman

//Codigo
trait CodigoHuffman {

  // Peso del árbol
  def peso(arbol: ArbolHuffman): Int = arbol match {
    case NodoHuffman(caracter, pesoN) => pesoN
    case RamaHuffman(nodoIzq, nodoDch) => peso(nodoIzq) + peso(nodoDch)
    case _ => throw new NoSuchElementException()
  }
  //Lista de Caracteres
  def caracteres(arbol: ArbolHuffman): List[Char] =
    def caracteresAux(arbolAux: ArbolHuffman, lista: List[Char]): List[Char] = arbolAux match
      case NodoHuffman (caracter, pesoN) => caracter::lista
      case RamaHuffman (nodoIzq, nodoDch) => caracteresAux(nodoIzq,lista):::caracteresAux(nodoDch, lista)
      case _ => throw new NoSuchElementException()
    caracteresAux(arbol,Nil)
}

// Crear un objeto del codigo
object Probar extends CodigoHuffman

//Crear instancias arbol
var arbol1: ArbolHuffman = RamaHuffman(NodoHuffman('e',2),NodoHuffman(' ',2))
var arbol2: ArbolHuffman = RamaHuffman(NodoHuffman('o',3), arbol1)
var arbol: ArbolHuffman = RamaHuffman(NodoHuffman('s',4), arbol2)


// Calcular el peso del árbol
val suma = Probar.peso(arbol)
println("El peso del arbol es: " + suma)

//Calcular la lista de caracteres
val lista = Probar.caracteres(arbol)
println("La lista de caracteres del arbol, de mayor  amenor frecuencia, es: "+lista)