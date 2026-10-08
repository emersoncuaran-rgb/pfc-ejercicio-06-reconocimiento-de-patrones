package taller

class Ejercicio() {

  // Punto 1
  def areaTotal(figuras: List[Figura]): Double = figuras match {
    case Nil => 0.0
    case fig :: resto => fig.area + areaTotal(resto)
  }

  // Funciones auxiliares sin métodos prohibidos
  private def pertenece(x: Int, l: List[Int]): Boolean = l match {
    case Nil => false
    case head :: tail => if (head == x) true else pertenece(x, tail)
  }

  private def insertarOrdenado(x: Int, l: List[Int]): List[Int] = l match {
    case Nil => List(x)
    case head :: tail =>
      if (x < head) x :: l
      else if (x == head) l
      else head :: insertarOrdenado(x, tail)
  }

  // Punto 2
  def sinRepetidos(l: List[Int]): List[Int] = {
    def ordenar(lista: List[Int], acc: List[Int]): List[Int] = lista match {
      case Nil => acc
      case x :: xs => ordenar(xs, insertarOrdenado(x, acc))
    }
    ordenar(l, Nil)
  }

  // Punto 3
  def comunes(l1: List[Int], l2: List[Int]): List[Int] = {
    def filtrarEInsertar(lista: List[Int], acc: List[Int]): List[Int] = lista match {
      case Nil => acc
      case x :: xs =>
        if (pertenece(x, l2)) filtrarEInsertar(xs, insertarOrdenado(x, acc))
        else filtrarEInsertar(xs, acc)
    }
    filtrarEInsertar(l1, Nil)
  }

  // Punto 4
  def evaluar(e: Expr): Int = e match {
    case Numero(v) => v
    case Suma(e1, e2) => evaluar(e1) + evaluar(e2)
    case Resta(e1, e2) => evaluar(e1) - evaluar(e2)
    case Prod(e1, e2) => evaluar(e1) * evaluar(e2)
  }

  def mostrar(e: Expr): String = e match {
    case Numero(v) => v.toString
    case Suma(e1, e2) => s"${mostrar(e1)} + ${mostrar(e2)}"
    case Resta(e1, e2) =>
      val izq = mostrar(e1)
      val der = e2 match {
        case Suma(_, _) | Resta(_, _) => s"(${mostrar(e2)})"
        case _ => mostrar(e2)
      }
      s"$izq - $der"
    case Prod(e1, e2) =>
      val izq = e1 match {
        case Suma(_, _) | Resta(_, _) => s"(${mostrar(e1)})"
        case _ => mostrar(e1)
      }
      val der = e2 match {
        case Suma(_, _) | Resta(_, _) => s"(${mostrar(e2)})"
        case _ => mostrar(e2)
      }
      s"$izq * $der"
  }

  def simplificar(e: Expr): Expr = e match {
    case Numero(v) => Numero(v)
    case Suma(e1, e2) =>
      (simplificar(e1), simplificar(e2)) match {
        case (Numero(0), s2) => s2
        case (s1, Numero(0)) => s1
        case (s1, s2) => Suma(s1, s2)
      }
    case Resta(e1, e2) =>
      (simplificar(e1), simplificar(e2)) match {
        case (s1, Numero(0)) => s1
        case (s1, s2) if s1 == s2 => Numero(0)
        case (s1, s2) => Resta(s1, s2)
      }
    case Prod(e1, e2) =>
      (simplificar(e1), simplificar(e2)) match {
        case (Numero(0), _) | (_, Numero(0)) => Numero(0)
        case (Numero(1), s2) => s2
        case (s1, Numero(1)) => s1
        case (s1, s2) => Prod(s1, s2)
      }
  }
}