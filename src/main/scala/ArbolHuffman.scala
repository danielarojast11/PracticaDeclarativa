import scala.annotation.tailrec
import scala.collection.immutable.Nil

abstract class ArbolHuffman {

  //Metodo Peso
  def peso: Int = this match {
    case HojaHuff(_, pesoN) => pesoN          //Si es hoja, devuelve el peso
    case RamaHuff(nodoIzq, nodoDch) => nodoIzq.peso + nodoDch.peso      //Si es rama, suma los pesos de los nodos
    case null => throw new NoSuchElementException("El árbol no puede ser nulo.")
  }

  //Metodo Lista de Caracteres
  def caracteres: List[Char] = {
    def caracteresAux(arbolAux: ArbolHuffman, lista: List[Char]): List[Char] = arbolAux match   //Funcion auxiliar
      case HojaHuff(caracter, pesoN) => caracter :: lista     //Si es hoja, concatena el caracter a la lista
      case RamaHuff(nodoIzq, nodoDch) => caracteresAux(nodoIzq, lista) ::: caracteresAux(nodoDch, lista)    //Si es rama, concatena la lista de caracteres de cada nodo
      case null => throw new NoSuchElementException()

    caracteresAux(this, Nil)
  }

  //Metodo Cadena a Lista de Caracteres
  def cadenaAListaCaracteres(cadena: String): List[Char] = cadena match {
    case _ => cadena.toList
    case null => throw new NoSuchElementException()
  }

  //Metodo Lista de Caracteres a Cadena
  def listaCharsACadena(listaCar: List[Char]): String = listaCar match {
    case m => listaCar.mkString
    case null => throw new NoSuchElementException()
  }

  //METODO DECODIFICAR
  def decodificar(lista: List[Bit]): String = {
    @tailrec
    def decodificarAux(arbol: ArbolHuffman, listaAux: List[Bit], lChar: List[Char]): List[Char] = (arbol, listaAux) match {   //Funcion auxiliar
      case (HojaHuff(caracter, pesoN), _) => decodificarAux(this, listaAux, lChar :+ caracter)
      case (RamaHuff(nodoIzq, nodoDch), 0 :: tail) => decodificarAux(nodoIzq, tail, lChar) //Si el primer bit es 0 va al nodoIzq y llama a la funcion con el resto de bits
      case (RamaHuff(nodoIzq, nodoDch), 1 :: tail) => decodificarAux(nodoDch, tail, lChar) //Si el primer bit es 1 va al nodoDch y llama a la funcion con el resto de bits
      case (_, Nil) => lChar
      case _ => throw new NoSuchElementException()
    }

    listaCharsACadena(decodificarAux(this, lista, Nil))
  }

  //METODO CODIFICAR
  def codificar(cadena: String): List[Bit] = {
    def buscarCaracter(arbol: ArbolHuffman, caracter: Char): Boolean = {    //busca el caracter en el arbol o subarbol
      val listaDeCaracteres: List[Char] = arbol.caracteres        //crea una lista con todos los caracteres del arbol o subarbol

      @tailrec
      def estaLista(lista: List[Char]): Boolean = lista match {
        case h :: t =>    //recorre la lista
          if h == caracter then true      //si encuentra el caracter deja de recorrerla y devuelve true
          else estaLista(t)
        case Nil => false     //ha recorrido toda la lista y no ha encontrado el caracter
      }

      estaLista(listaDeCaracteres)
    }

    @tailrec
    def codifAux(carac: Char, arbolC: ArbolHuffman, listaAux: List[Bit]): List[Bit] = arbolC match {
      case HojaHuff(caracter, pesoN) => listaAux    //no hay mas arbol, devuelve la lista
      case RamaHuff(nodoIzq, nodoDch) =>
        if buscarCaracter(nodoIzq, carac) then codifAux(carac, nodoIzq, 0 :: listaAux)    //busca el caracter en el subarbol izquierdo, si está, añade un 0
        else if buscarCaracter(nodoDch, carac) then codifAux(carac, nodoDch, 1 :: listaAux)   //busca el caracter en el subarbol derecho, si está, añade un 1
        else throw new NoSuchElementException("Caracter no encontrado")   //no ha encontrado el caracter en el arbol
    }

    def unirListas(listaChar: List[Char]): List[Bit] = listaChar match {    //une la lista de bits de cada caracter en unoa sola
      case h :: t => unirListas(t) ++ codifAux(h, this, Nil)
      case Nil => Nil
    }

    unirListas(cadenaAListaCaracteres(cadena)).reverse
  }
}

//Clases Hoja y Rama
case class HojaHuff(caracter: Char, var pesoN: Int) extends ArbolHuffman
case class RamaHuff(nodoIzq: ArbolHuffman, nodoDch: ArbolHuffman) extends ArbolHuffman

type Bit = 1 | 0
type TablaCodigos = List[(Char, List[Bit])]

def crearArbolHuffman(cadena: String): ArbolHuffman = {

  // Convierte la lista de caracteres en distribución de frecuencias.
  def ListaCharsADistFrec(listaChar: List[Char]): List[(Char, Int)] = {
    @tailrec
    def listaFrecsAux(listaChar: List[Char], listaFrecs: List[(Char, Int)]): List[(Char, Int)] = {
      listaChar match
        case Nil => listaFrecs.reverse
        case h :: tail =>
          if estaenListaFrec(h, listaFrecs) then listaFrecsAux(tail, actualizarFrec(h, listaFrecs))
          else listaFrecsAux(tail, (h, 1) :: listaFrecs)
        case null => throw new NoSuchElementException("Error inesperado")
    }

    @tailrec
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
        case Nil => throw new NoSuchElementException("El nodo no se encuentra en la lista de frecuencias para actualizar")
        case null => throw new NoSuchElementException
    }

    listaFrecsAux(listaChar, Nil)
  }

  // Convierte la distribución en una lista de hojas ordenada
  def DistribFrecAListaHojas(frec: List[(Char, Int)]): List[HojaHuff] = {
    def insertarOrdenada(tupla: (Char, Int), listaOrdenada: List[(Char, Int)]): List[(Char, Int)] = {
      listaOrdenada match
        case Nil => List(tupla)
        case (charOrd, frecOrd) :: tail =>
          val (characTupla, frecTupla): (Char, Int) = tupla
          if frecTupla <= frecOrd then tupla :: listaOrdenada
          else (charOrd, frecOrd) :: insertarOrdenada(tupla, tail)
    }

    @tailrec
    def ordenarListaTuplas(frec: List[(Char, Int)], listaOrdenada: List[(Char, Int)]): List[(Char, Int)] = {
      frec match
        case Nil => listaOrdenada
        case h :: tail => ordenarListaTuplas(tail, insertarOrdenada(h, listaOrdenada))
    }

    val frecsOrdenadas = ordenarListaTuplas(frec, Nil)

    frecsOrdenadas.map((charac, peso) => HojaHuff(charac, peso))
  }

  // Crea un objeto RamaHuff integrando los dos ArbolHuff (izquierdo y
  // derecho)que se le pasan como parámetros
  def creaRamaHuff(izq: ArbolHuffman, dch: ArbolHuffman): RamaHuff = {
    RamaHuff(izq, dch)
  }

  def combinar(nodos: List[ArbolHuffman]): List[ArbolHuffman] = {
    nodos match
      case Nil => throw new NoSuchElementException("Arbol vacío")
      case n1 :: Nil => List(n1)
      case n1 :: n2 :: resto =>
        val rama = creaRamaHuff(n1, n2) :: nodos.tail.tail
        rama.sortWith(_.peso < _.peso)
  }

  def esListaSingleton(lista: List[ArbolHuffman]): Boolean = {
    if lista.size == 1 then true
    else false
  }

  def repetirHasta(combinar: List[ArbolHuffman] => List[ArbolHuffman], esListaSingleton: List[ArbolHuffman] => Boolean)(listahojas: List[ArbolHuffman]): ArbolHuffman = {
    @tailrec
    def iterar(lista: List[ArbolHuffman]): ArbolHuffman = {
      if esListaSingleton(lista) then lista.head
      else iterar(combinar(lista))
    }

    iterar(listahojas)
  }

  val distFrec = ListaCharsADistFrec(cadena.toList)
  val distribFrec = DistribFrecAListaHojas(distFrec)
  repetirHasta(combinar, esListaSingleton)(distribFrec)
}

//Objeto de compañia
object ArbolHuffman {

  //Metodo constructor alternativo
  def apply(cadena: String): ArbolHuffman = crearArbolHuffman(cadena)
}

def deArbolATabla(arbol: ArbolHuffman): TablaCodigos = {
  def recorrerArbol(nodo: ArbolHuffman, codigoActual: List[Bit]): TablaCodigos = {
    nodo match
      case HojaHuff(caracter, _) => List((caracter, codigoActual))
      case RamaHuff(nodoIzq, nodoDch) =>
        recorrerArbol(nodoIzq, codigoActual :+ 0) ::: recorrerArbol(nodoDch, codigoActual :+ 1)
  }
  recorrerArbol(arbol, Nil)
}

def buscarCodigo(tabla: TablaCodigos, caract: Char): List[Bit] = {
  @tailrec
  def buscarAux(resto: TablaCodigos): List[Bit] = {
    resto match
      case (caracTabla, codigoTabla) :: tail =>
        if (caracTabla == caract) codigoTabla
        else buscarAux(tail)
      case Nil => throw new NoSuchElementException("El caracter no se encuentra en la tabla")
  }

  buscarAux(tabla)
}

def buscarCaracter(tabla: TablaCodigos, codigo: List[Bit]): Char = {
  @tailrec
  def buscarAux(resto: TablaCodigos): Char = {
    resto match
      case (caracTabla, codigoTabla) :: tail =>
        if (codigo == codigoTabla) caracTabla
        else buscarAux(tail)
      case Nil => throw new NoSuchElementException("El código de bits no se encuentra en la tabla")
  }
  buscarAux(tabla)
}

def codificar(tabla: TablaCodigos)(cadena: String): List[Bit] = {
  cadena.toList.flatMap(caracter => buscarCodigo(tabla, caracter))
}

//Objeto para realizar las pruebas
object miPrograma extends App {

  // Crear árbol a mano
  val arbol1: ArbolHuffman = RamaHuff(HojaHuff('e', 2), HojaHuff(' ', 2))
  val arbol2: ArbolHuffman = RamaHuff(HojaHuff('o', 3), arbol1)
  val arbol: ArbolHuffman = RamaHuff(HojaHuff('s', 4), arbol2)

  //PRUEBAS DE METODOS ARBOL A MANO
  println("\n-------PRUEBAS DEL ARBOL A MANO-------")
  println("Peso del arbol a mano: " + arbol.peso)
  println("Caracteres del arbol a mano: " + arbol.caracteres)

  val cadena = "sos ese oso"
  private val cadenaLista: List[Char] = arbol.cadenaAListaCaracteres(cadena)
  println("Cadena a lista de Caracteres del arbol a mano: " + cadenaLista)
  println("Lista a cadena de caracteres del arbol a mano: " + arbol.listaCharsACadena(cadenaLista))

  //Decodificar
  val listaBits: List[Bit] = List(0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0)
  val resultadoDecod: String = arbol.decodificar(listaBits)
  println("Lista decodificada del arbol a mano: " + resultadoDecod)

  //Codificar
  private val cadenaCod: String = "sos ese oso"
  private val resultadoCod: List[Bit] = arbol.codificar(cadenaCod)
  println("Lista codificada del arbol a mano: " + resultadoCod)

  //PRUEBAS DE METODOS ARBOL NO A MANO
  private val miArbol = ArbolHuffman("sos ese oso")
  println("\n-------PRUEBAS DE LOS METODOS DEL ARBOL CREADO-------")
  println("1. Peso del Arbol: " + miArbol.peso)
  println("2. Caracteres del arbol: " + miArbol.caracteres)
  println("3. Cadena a lista de Caracteres: " + miArbol.cadenaAListaCaracteres(cadena))
  private val listaChar: List[Char] = List('s', 'o', 's', ' ', 'e', 's', 'e', ' ', 'o', 's', 'o')
  println("4. Lista a cadena: " + miArbol.listaCharsACadena(listaChar))
  println(s"5. Lista $listaBits decodificada: " + miArbol.decodificar(listaBits))
  println(s"6. Cadena $cadenaCod codificada: " + miArbol.codificar(cadenaCod))

  val tablaCodificacion = deArbolATabla(miArbol)
  println("La tabla de codificación es: " + tablaCodificacion)

  val resultadoCod2: List[Bit] = miArbol.codificar(cadenaCod)
  println("Cadena codificada con miArbol:" + resultadoCod2)
}