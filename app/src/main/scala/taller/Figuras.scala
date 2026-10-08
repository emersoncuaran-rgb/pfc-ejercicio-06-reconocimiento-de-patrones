package taller

abstract class Figura {
  def nombre: String
  def area: Double
  def perimetro: Double
  def esMayorQue(otra: Figura): Boolean = this.area > otra.area
}

trait Escalable {
  def escalar(k: Double): Figura
}

class Circulo(val radio: Double) extends Figura with Escalable {
  override def nombre: String = "círculo"
  override def area: Double = math.Pi * radio * radio
  override def perimetro: Double = 2.0 * math.Pi * radio
  override def escalar(k: Double): Figura = new Circulo(radio * k)
}

class Rectangulo(val base: Double, val altura: Double) extends Figura with Escalable {
  override def nombre: String = "rectángulo"
  override def area: Double = base * altura
  override def perimetro: Double = 2.0 * (base + altura)
  override def escalar(k: Double): Figura = new Rectangulo(base * k, altura * k)
}

class Cuadrado(val lado: Double) extends Rectangulo(lado, lado) {
  override def nombre: String = "cuadrado"
  override def escalar(k: Double): Figura = new Cuadrado(lado * k)
}

class Triangulo(val base: Double, val altura: Double) extends Figura with Escalable {
  override def nombre: String = "triángulo"
  override def area: Double = (base * altura) / 2.0
  override def perimetro: Double = {
    val hipotenusa = math.sqrt((base * base) + (altura * altura))
    base + altura + hipotenusa
  }
  override def escalar(k: Double): Figura = new Triangulo(base * k, altura * k)
}