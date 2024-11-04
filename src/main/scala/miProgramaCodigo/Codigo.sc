import scala.annotation.tailrec
import scala.collection.immutable.List

// Bit
type Bit = 1 | 0

// Trait App
trait App {

  // Clase Abstracta ArbolHuffman y sus metodos
  abstract class ArbolHuffman {

    def peso(arbol: ArbolHuffman): Int = arbol match {
      case NodoHuffman(_, pesoN) => pesoN
      case RamaHuffman(nodoIzq, nodoDch) => peso(nodoIzq) + peso(nodoDch)
      case null => throw new NoSuchElementException("El árbol no puede ser nulo.")
    }

    //Lista de Caracteres
    def caracteres(arbol: ArbolHuffman): List[Char] = {
      def caracteresAux(arbolAux: ArbolHuffman, lista: List[Char]): List[Char] = arbolAux match
        case NodoHuffman(caracter, pesoN) => caracter :: lista
        case RamaHuffman(nodoIzq, nodoDch) => caracteresAux(nodoIzq, lista) ::: caracteresAux(nodoDch, lista)
        case null => throw new NoSuchElementException()

      caracteresAux(arbol, Nil)
    }

    //Cadena a Lista de Caracteres
    def cadenaAListaCaracteres(cadena: String): List[Char] = cadena match {
      case m => cadena.toList
      case null => throw new NoSuchElementException()
    }

    //Lista de Caracteres a Cadena
    def listaCharsACadena(listaCar: List[Char]): String = listaCar match {
      case m => listaCar.mkString
      case null => throw new NoSuchElementException()
    }

    def decodificar(lista: List[Bit]): String = {
      @tailrec
      def decodificarAux(arbol: ArbolHuffman, listaAux: List[Bit], lChar: List[Char]): List[Char] = (arbol, listaAux) match {
        case (NodoHuffman(caracter, pesoN), _) => decodificarAux(this, listaAux, lChar :+ caracter)
        case (RamaHuffman(nodoIzq, nodoDch), 0 :: tail) => decodificarAux(nodoIzq, tail, lChar)
        case (RamaHuffman(nodoIzq, nodoDch), 1 :: tail) => decodificarAux(nodoDch, tail, lChar)
        case (_, Nil) => lChar
        case _ => throw new NoSuchElementException()
      }

      listaCharsACadena(decodificarAux(this, lista, Nil))
    }

    def codificar(cadena: String): List[Bit] = {
      def evaluarNodo(arbolN: ArbolHuffman, cN: Char): Boolean = arbolN match {
        case NodoHuffman(caracter, pesoN) => caracter == cN
        case _ => false
      }

      @tailrec
      def recorrerArbol(cR: Char, listaBR: List[Bit], arbolR: ArbolHuffman): List[Bit] = arbolR match {
        case NodoHuffman(caracter, pesoN) =>
          if caracter == cR then listaBR
          else throw new NoSuchElementException("No existe el caracter")
        case RamaHuffman(nodoIzq, nodoDch) =>
          if evaluarNodo(nodoIzq, cR) then 0 :: listaBR
          else recorrerArbol(cR, 1 :: listaBR, nodoDch)
        case null => throw new NoSuchElementException("Error inesperado")
      }

      def codifAux(caracteres: List[Char], arbolC: ArbolHuffman): List[Bit] = caracteres match {
        case Nil => Nil
        case h :: t => codifAux(t, arbolC) ++ recorrerArbol(h, Nil, this)
        case null => throw new NoSuchElementException("Error inesperado")
      }

      codifAux(cadenaAListaCaracteres(cadena), this).reverse
    }

  }

  //Clases Nodo y Rama
  case class NodoHuffman(caracter: Char, var pesoN: Int) extends ArbolHuffman
  case class RamaHuffman(nodoIzq: ArbolHuffman, nodoDch: ArbolHuffman) extends ArbolHuffman

  //Funciones App para llamar a los metodos del Arbol
  // Peso del árbol App
  def pesoApp(arbol: ArbolHuffman): Int = {
    arbol.peso(arbol)
  }

  //Lista de Caracteres App
  def caracteresApp(arbol: ArbolHuffman): List[Char] = {
    arbol.caracteres(arbol)
  }

  //Cadena a Lista de Caracteres App
  def cadenaAListaCaracteresApp(arbol:ArbolHuffman, cadena: String): List[Char] = {
    arbol.cadenaAListaCaracteres(cadena)
  }

  //Lista de Caracteres a Cadena App
  def listaCharsACadenaApp(arbol: ArbolHuffman, listaCar: List[Char]): String = {
    arbol.listaCharsACadena(listaCar)
  }

  //Decodificar App
  def decodificarApp(arbol: ArbolHuffman, lista: List[Bit]):String = {
    arbol.decodificar(lista)
  }

  //Codificar App
  def codificarApp (arbol: ArbolHuffman,cadena:String):List[Bit] = {
    arbol.codificar(cadena)
  }
}

// Objeto miPrograma para probar programa
object miPrograma extends App {

  // Crear árbol a mano
  val arbol1: ArbolHuffman = RamaHuffman(NodoHuffman('e', 2), NodoHuffman(' ', 2))
  val arbol2: ArbolHuffman = RamaHuffman(NodoHuffman('o', 3), arbol1)
  val arbol: ArbolHuffman = RamaHuffman(NodoHuffman('s', 4), arbol2)

  // Calcular el peso del árbol
  val resultadoPeso = miPrograma.pesoApp(arbol)

  //Calcular la lista de Caracteres
  val resultadoCaracteres = miPrograma.caracteresApp(arbol)

  //Crear cadena y lista
  val cadena: String = "sos ese oso"
  val resultadoListaChar: List[Char] = miPrograma.cadenaAListaCaracteresApp(arbol, cadena)
  val resultadoCadena: String = miPrograma.listaCharsACadenaApp(arbol,resultadoListaChar)

  //Decodificar
  val listaBits: List[Bit] = List(0,1,0,0,1,1,1,1,1,0,0,1,1,0,1,1,1,1,0,0,1,0)
  val resultadoDecod: String = miPrograma.decodificarApp(arbol,listaBits)

  //Codificar
  val cadenaCod: String = "sos ese oso"
  val resultadoCod: List[Bit] = miPrograma.codificarApp(arbol, cadenaCod)
}

println("El peso del arbol es: " + miPrograma.resultadoPeso)
println("Los caracteres del arbol son: "+miPrograma.resultadoCaracteres)
println(s"La cadena '${miPrograma.cadena}' en caracteres es: "+miPrograma.resultadoListaChar)
println(s"La lista de caracteres '${miPrograma.resultadoListaChar}' en cadena es: "+miPrograma.resultadoCadena)
println(miPrograma.resultadoDecod)
println(miPrograma.resultadoCod)
