

//Codigo
sealed trait ArbolHuffman {

  // Peso del árbol
  def peso(arbol: ArbolHuffman): Int = arbol match
    case NodoHuffman(caracter, pesoN) => pesoN
    case RamaHuffman(nodoIzq, nodoDch) => peso(nodoIzq) + peso(nodoDch)
    case null => throw new NoSuchElementException()

  //Lista de Caracteres
  def caracteres(arbol: ArbolHuffman): List[Char] =
    def caracteresAux(arbolAux: ArbolHuffman, lista: List[Char]): List[Char] = arbolAux match
      case NodoHuffman (caracter, pesoN) => caracter::lista
      case RamaHuffman (nodoIzq, nodoDch) => caracteresAux(nodoIzq,lista):::caracteresAux(nodoDch, lista)
      case null => throw new NoSuchElementException()
    caracteresAux(arbol,Nil)

  def cadenaAListaCaracteres (cadena: String): List[Char] = cadena match
    case m => cadena.toList
    case null => throw new NoSuchElementException()

  def listaCharsACadena(listaCar: List[Char]): String = listaCar match
    case m => listaCar.mkString
    case null => throw new NoSuchElementException()

  type Bit = 1 | 0

  def decodificar (lista: List[Bit]): String =
    def decodificarAux(arbol: ArbolHuffman, listaAux: List[Bit], lChar: List[Char]): List[Char] = (arbol, listaAux) match {
      case (NodoHuffman(caracter, pesoN), _) => decodificarAux(this, listaAux, lChar :+ caracter)
      case (RamaHuffman(nodoIzq, nodoDch), 0 :: tail) => decodificarAux(nodoIzq, tail, lChar)
      case (RamaHuffman(nodoIzq, nodoDch), 1 :: tail) => decodificarAux(nodoDch, tail, lChar)
      case (_, Nil) => lChar
      case _ => throw new NoSuchElementException()
    }
    listaCharsACadena(decodificarAux(this, lista, Nil))

  def codificar (cadena: String): List[Bit]=
    def evaluarNodo (arbolN: ArbolHuffman, cN: Char): Boolean = arbolN match
      case NodoHuffman(caracter, pesoN) => caracter==cN
      case _ => false
    def recorrerArbol(cR:Char, listaBR: List[Bit], arbolR: ArbolHuffman): List[Bit] = arbolR match
      case NodoHuffman(caracter,pesoN) =>
        if caracter==cR then listaBR
        else throw new NoSuchElementException("No existe el caracter")
      case RamaHuffman(nodoIzq,nodoDch) =>
        if evaluarNodo(nodoIzq,cR) then 0::listaBR
        else recorrerArbol(cR, 1::listaBR, nodoDch)
      case null => throw new NoSuchElementException("Error inesperado")
    def codifAux (caracteres: List[Char], arbolC: ArbolHuffman): List[Bit] = caracteres match
      case Nil=>Nil
      case h::t => codifAux(t,arbolC)++recorrerArbol(h, Nil, this)
      case null => throw new NoSuchElementException("Error inesperado")

    codifAux(cadenaAListaCaracteres(cadena),this).reverse
}

//Clases Nodo y Rama
case class NodoHuffman(caracter: Char, var pesoN: Int) extends ArbolHuffman
case class RamaHuffman(nodoIzq: ArbolHuffman, nodoDch: ArbolHuffman) extends ArbolHuffman

type Bit = 1|0

//Crear instancias arbol
var arbol1: ArbolHuffman = RamaHuffman(NodoHuffman('e',2),NodoHuffman(' ',2))
var arbol2: ArbolHuffman = RamaHuffman(NodoHuffman('o',3), arbol1)
var arbol: ArbolHuffman = RamaHuffman(NodoHuffman('s',4), arbol2)

// Calcular el peso del árbol
val suma = arbol.peso(arbol)
println("El peso del arbol es: " + suma)

//Calcular la lista de caracteres
val lista = arbol.caracteres(arbol)
println("La lista de caracteres del arbol, de mayor  amenor frecuencia, es: "+lista)

//Crear cadena y lista
var cadena: String = "Hola Ines"
val listaCar = arbol.cadenaAListaCaracteres(cadena)
val cadenaCar = arbol.listaCharsACadena(listaCar)

//Decodificar
var listaBits: List[Bit] = List(0,1,0,0,1,1,1,1,1,0,0,1,1,0,1,1,1,1,0,0,1,0)
val listDecod = arbol.decodificar(listaBits)

var cadenaCodif: String = "sos ese oso"
val listaCodif = arbol.codificar(cadenaCodif)

