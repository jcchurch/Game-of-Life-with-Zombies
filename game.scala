import scala.util.Random
import scala.swing._
import event._
import java.awt.Color

class Square {
    val color = new Color(255, 255, 255)
}

class Space extends Square {
    override def toString = " "
    override val color = new Color(255, 255, 255)
}

class Human extends Square {
    override def toString = "H"
    override val color = new Color(40, 40, 40)
}

class Zombie extends Square {
    override def toString = "Z"
    override val color = new Color(40, 255, 40)
}

object game {
    val humanDensity = 0.2
    val zombieDensity = 0.2
    var height = 0
    var width = 0
    var squareSize = 0
    var world = Array.ofDim[Square](width+2,height+2)

    def advanceGeneration() = {
        var newworld = Array.ofDim[Square](width+2,height+2)
        var humanNeighbors = Array.ofDim[Int](width+2,height+2)
        var zombieNeighbors = Array.ofDim[Int](width+2,height+2)

        // Count Human and Zombie neighbors of each Square
        for (y <- 1 to height) {
            for (x <- 1 to width) {
                world(x)(y) match {
                    case h: Human => {
                        humanNeighbors(x+1)(y+1) += 1
                        humanNeighbors(x)(y+1) += 1
                        humanNeighbors(x-1)(y+1) += 1
                        humanNeighbors(x+1)(y) += 1
                        humanNeighbors(x-1)(y) += 1
                        humanNeighbors(x+1)(y-1) += 1
                        humanNeighbors(x)(y-1) += 1
                        humanNeighbors(x-1)(y-1) += 1
                    }
                    case z: Zombie => {
                        zombieNeighbors(x+1)(y+1) += 1
                        zombieNeighbors(x)(y+1) += 1
                        zombieNeighbors(x-1)(y+1) += 1
                        zombieNeighbors(x+1)(y) += 1
                        zombieNeighbors(x-1)(y) += 1
                        zombieNeighbors(x+1)(y-1) += 1
                        zombieNeighbors(x)(y-1) += 1
                        zombieNeighbors(x-1)(y-1) += 1
                    }
                    case _ => Unit
                }
            }
        }

        // General rules regarding zombies:
        // 1. Zombies cannot be born naturally. They must be overpower a human.
        // 2. Zombies need brains and die if there are no neighboring humans.
        // 3. An overpowered zombie dies (there are 4 more human neighbors than zombie neighbors).
        // 4. An overpowered human turns into a zombie (there are 1 more zombie neighbors than human neighbors).
        // 5. Humans still live and die according to the same rules set forth by Conway.
        // 6. Just like in the original Conway game, no one moves. Ergo, Zombies cannot shamble.

        for (y <- 1 to height) {
            for (x <- 1 to width) {
                newworld(x)(y) = new Space    

                world(x)(y) match {
                    case h: Human => {
                        if (humanNeighbors(x)(y) == 2 || humanNeighbors(x)(y) == 3) {
                            newworld(x)(y) = new Human
                        }

                        if (zombieNeighbors(x)(y) - humanNeighbors(x)(y) >= 1) {
                            newworld(x)(y) = new Zombie
                        }
                    }

                    case z: Zombie => {
                        newworld(x)(y) = new Zombie
                        if (humanNeighbors(x)(y) - zombieNeighbors(x)(y) >= 4) {
                            newworld(x)(y) = new Space
                        }

                        if (humanNeighbors(x)(y) == 0) {
                            newworld(x)(y) = new Space
                        }
                    }

                    case s: Space => {
                        if (humanNeighbors(x)(y) == 3) {
                            newworld(x)(y) = new Human
                        }
                    }
                }
            }
        }

        world = newworld
    }

    def printWorld() = {
        for (y <- 1 to height) {
            for (x <- 1 to width) {
                Console.print(world(x)(y))
            }
            Console.println()
        }
    }

    def paint(g: Graphics2D) {
        for (y <- 1 to height) {
            for (x <- 1 to width) {
                g setColor world(x)(y).color
                g fillRect( (x-1)*squareSize, (y-1)*squareSize, squareSize, squareSize )
            }
        }
    }

    def initalizeWorld(pixelWidth: Int, pixelHeight: Int, sSize: Int) = {

        squareSize = sSize
        width = pixelWidth / sSize
        height = pixelHeight / sSize
        world = Array.ofDim[Square](width+2,height+2)

        val rng = new Random
        val totalDensity = humanDensity + zombieDensity

        for (y <- 0 to height+1) {
            for (x <- 0 to width+1) {
                world(x)(y) = new Space
            }
        }

        for (y <- 1 to height) {
            for (x <- 1 to width) {
                if (rng.nextDouble <= totalDensity) {
                    if (rng.nextDouble <= humanDensity / totalDensity) {
                        world(x)(y) = new Human
                    }
                    else {
                        world(x)(y) = new Zombie
                    }
                }
            }
        }
    }

    def main(args: Array[String]) = {
        initalizeWorld(200, 100, 1)

        while (true) {
            printWorld
            Thread.sleep(1000)
            advanceGeneration
        }
    }
}

object GameSwingApp extends SimpleSwingApplication {
    val pixelWidth = 1260
    val pixelHeight = 630
    val squareSize = 5
    game.initalizeWorld(pixelWidth, pixelHeight, squareSize)

    def top = new MainFrame {
        title = "Game of Live + Zombies"
        contents = new Panel {

            preferredSize = new Dimension(pixelWidth, pixelHeight)

            focusable = true
            listenTo(keys)

            reactions += {
                case KeyPressed(_, key, _, _) =>
                    repaint
            }

            override def paint(g: Graphics2D) {
                game.advanceGeneration
                game.paint(g)
            }
        }
    }
}
