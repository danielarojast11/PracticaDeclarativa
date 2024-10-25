
trait CodigoHuffman {
  //Peso Arbol
  def peso(arbol: ArbolHuffman): Int = peso match
    case NodoHuffman(caracter, pesoN) => pesoN
    case RamaHuffman(nodoIzq, nodoDch, pesoR) => peso(nodoDch) + peso(nodoIzq)
    
  
  //val arbol = new ArbolHuffman {RamaHuffman(new ArbolHuffman {NodoHuffman('e',2)},new ArbolHuffman {NodoHuffman(' ',3)}, 0)}
}
