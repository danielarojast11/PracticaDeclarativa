import miPrograma.arbol1

import scala.annotation.tailrec
import scala.collection.immutable.List

// Bit
type Bit = 1 | 0

// Trait App
trait App {

  // Clase Abstracta ArbolHuffman y sus metodos
  abstract class ArbolHuffman {

    def peso: Int = this match {
      case HojaHuff(_, pesoN) => pesoN
      case RamaHuff(nodoIzq, nodoDch) => nodoIzq.peso + nodoDch.peso
      case null => throw new NoSuchElementException("El árbol no puede ser nulo.")
    }

    //Lista de Caracteres
    def caracteres: List[Char] = {
      def caracteresAux(arbolAux: ArbolHuffman, lista: List[Char]): List[Char] = arbolAux match
        case HojaHuff(caracter, pesoN) => caracter :: lista
        case RamaHuff(nodoIzq, nodoDch) => caracteresAux(nodoIzq, lista) ::: caracteresAux(nodoDch, lista)
        case null => throw new NoSuchElementException()

      caracteresAux(this, Nil)
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
        case (HojaHuff(caracter, pesoN), _) => decodificarAux(this, listaAux, lChar :+ caracter)
        case (RamaHuff(nodoIzq, nodoDch), 0 :: tail) => decodificarAux(nodoIzq, tail, lChar)
        case (RamaHuff(nodoIzq, nodoDch), 1 :: tail) => decodificarAux(nodoDch, tail, lChar)
        case (_, Nil) => lChar
        case _ => throw new NoSuchElementException()
      }

      listaCharsACadena(decodificarAux(this, lista, Nil))
    }

    /*def codificar(cadena: String): List[Bit] = {

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

      codifAux(cadenaAListaCaracteres(cadena), this)
    }*/

    def codificarBien(cadena: String): List[Bit] = {
      def buscarCaracter(arbol: ArbolHuffman, caracter: Char): Boolean = {
        val listaDeCaracteres: List[Char] = arbol.caracteres

        @tailrec
        def estaLista(lista: List[Char]): Boolean = lista match {
          case h :: t =>
            if h == caracter then true
            else estaLista(t)
          case Nil => false
        }

        estaLista(listaDeCaracteres)
      }

      @tailrec
      def codifAux(carac: Char, arbolC: ArbolHuffman, listaAux: List[Bit]): List[Bit] = arbolC match {
        case HojaHuff(caracter, pesoN) => listaAux
        case RamaHuff(nodoIzq, nodoDch) =>
          if buscarCaracter(nodoIzq, carac) then codifAux(carac, nodoIzq, 0 :: listaAux)
          else if buscarCaracter(nodoDch, carac) then codifAux(carac, nodoDch, 1 :: listaAux)
          else throw new NoSuchElementException("Caracter no encontrado")
      }

      def unirListas(listaChar: List[Char]): List[Bit] = listaChar match {
        case h :: t => unirListas(t) ++ codifAux(h, this, Nil)
        case Nil => Nil
      }

      unirListas(cadenaAListaCaracteres(cadena)).reverse
    }
  }

  //Clases Nodo y Rama
  case class HojaHuff(caracter: Char, var pesoN: Int) extends ArbolHuffman

  case class RamaHuff(nodoIzq: ArbolHuffman, nodoDch: ArbolHuffman) extends ArbolHuffman

  //Funciones App para llamar a los metodos del Arbol
  // Peso del árbol App
  def pesoApp(arbol: ArbolHuffman): Int = {
    arbol.peso
  }

  //Lista de Caracteres App
  def caracteresApp(arbol: ArbolHuffman): List[Char] = {
    arbol.caracteres
  }

  //Cadena a Lista de Caracteres App
  def cadenaAListaCaracteresApp(arbol: ArbolHuffman, cadena: String): List[Char] = {
    arbol.cadenaAListaCaracteres(cadena)
  }

  //Lista de Caracteres a Cadena App
  def listaCharsACadenaApp(arbol: ArbolHuffman, listaCar: List[Char]): String = {
    arbol.listaCharsACadena(listaCar)
  }

  //Decodificar App
  def decodificarApp(arbol: ArbolHuffman, lista: List[Bit]): String = {
    arbol.decodificar(lista)
  }

  //Codificar App
  /*def codificarApp (arbol: ArbolHuffman,cadena:String):List[Bit] = {
    arbol.codificar(cadena)
  }*/

  def codificarBienApp(arbol: ArbolHuffman, cadena: String): List[Bit] = {
    arbol.codificarBien(cadena)
  }

  def crearArbolHuffman (cadena:String): ArbolHuffman = {
    
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
        case Nil => throw Error("Error inesperado")
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

  object ArbolHuffman {
    def apply(cadena: String): ArbolHuffman = crearArbolHuffman(cadena)
  }
}

// Objeto miPrograma para probar programa
object miPrograma extends App {

  // Crear árbol a mano
  val arbol1: ArbolHuffman = RamaHuff(null, null)
  
}

println(miPrograma.arbol1.peso)