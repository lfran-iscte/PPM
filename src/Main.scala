import Util.Placement
import javafx.application.Application
import javafx.fxml.FXMLLoader
import javafx.scene.shape._
import javafx.scene.{Parent, PerspectiveCamera, Scene, SceneAntialiasing}
import javafx.stage.Stage
import scala.io.StdIn.readLine
import java.io.FileNotFoundException
import scala.io.Source
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer


class Main extends Application {
  //Shape3D is an abstract class that extends javafx.scene.Node
  //Box and Cylinder are subclasses of Shape3D

  def displayFiles(txtFiles: ListBuffer[String]): Unit = {
    txtFiles.foreach(x => println(" " + txtFiles.indexOf(x) + " - " + x))
  }

  def applyScale(oct: Octree[Placement]): Octree[Placement] = {
    println("Which is the factor to be applied? \n 1 - Factor 0.5 or 2 - Factor 2 ")
    readLine match {
      case "1" => Util.scaleOctree(0.5, oct)
      case "2" => Util.scaleOctree(2, oct)
      case _ =>
        println("Invalid option! Please choose 1 or 2")
        applyScale(oct)
    }
  }

  def applyMapColour(oct: Octree[Placement]): Octree[Placement] = {
    println("Which is the function to be applied? \n 1 - RemoveGreenComponent or 2 - Sepia")
    readLine match {
      case "1" => Util.mapColourEffect(Util.removeGreenComponent, oct)
      case "2" => Util.mapColourEffect(Util.sepia, oct)
      case _ =>
        println("Invalid option! Please choose 1 or 2")
        applyMapColour(oct)
    }
  }

  def validation(min: Int, max: Int): Int = {
    val userInput = readLine
    if (userInput.matches("(0?[0-9]{1,2}|1?[0-9]{1,2}|2[0-4][0-9]|25[0-5])"))
      userInput.toInt
    else {
      println("Incorrect value! Please choose a number between 0 and 255")
      validation(min, max)
    }
  }

  def confObject(lst: String): String = {
    println("Which is the value of the red component of the colour? Please choose a number between 0 and 255")
    val r = validation(0, 256)
    println("Which is the value of the green component of the colour? Please choose a number between 0 and 255")
    val g = validation(0, 256)
    println("Which is the value of the blue component of the colour? Please choose a number between 0 and 255")
    val b = validation(0, 256)
    println("Which is the value of the translate of x?")
    val translX = validation(0, 100)
    println("Which is the value of the translate of y?")
    val translY = validation(0, 100)
    println("Which is the value of the translate of z?")
    val translZ = validation(0, 100)
    println("Which is the value of the scale of x?")
    val escX = validation(0, 100)
    println("Which is the value of the scale of y?")
    val escY = validation(0, 100)
    println("Which is the value of the scale of z?")
    val escZ = validation(0, 100)

    val colorRGB = List("(" + r, g, b + ")")
    val colorString = colorRGB.mkString(",")
    val args = List(lst, colorString, translX.toString, translY.toString, translZ.toString, escX.toString, escY.toString, escZ.toString)
    val objLine = args.mkString(" ")
    objLine
  }

  def createObject(): String = {
    println("Which is the type of the object?\n 1 - Box\n 2 - Cylinder")
    readLine match {
      case "1" =>
        confObject("Box")
      case "2" =>
        confObject("Cylinder")
      case _ =>
        println("Please choose a valid option!")
        createObject()
    }
  }

  def saveStateOptions(): String = {
    println("Do you want to save the state of the octree?\n 1 - Yes\n 2 - No")
    readLine match {
      case "1" => "1"
      case "2" => "2"
      case _ => "3"
    }
  }


  override def start(stage: Stage): Unit = {
    //Get and print program arguments (args: Array[String])

    val params = getParameters
    println("Program arguments:" + getParameters.getRaw)

    if(params.getRaw.isEmpty || (!params.getRaw.get(0).equals("text") && !params.getRaw.get(0).equals("graphical"))) {
      println("Please execute with argument \"text\" or \"graphical\" ")
      sys.exit(0)
    }

    if (params.getRaw.get(0).equals("text")) {
      menu()
    }
    if (params.getRaw.get(0).equals("graphical")) {
      val fxmlLoader = new FXMLLoader(getClass.getResource("Controller.fxml"))
      val mainViewRoot: Parent = fxmlLoader.load()
      val scene = new Scene(mainViewRoot)
      scene.setCamera(new PerspectiveCamera(false))
      stage.setScene(scene)
      stage.show()
    }

    //Get and print program arguments (args: Array[String])
    @tailrec
    def menuOptions(graphics: List[Shape3D], mainOct: Octree[Placement]): Unit = {
      println("Menu\n" +
        " 1 - Insert object\n 2 - Apply function scaleOctree \n 3 - Apply function MapColourEffect\n 4 - Launch 3D environment\n 5 - Exit")
      readLine match {
        case "1" =>
          val l = List(createObject())
          val objects: List[Shape3D] = graphics.concat(Util.getGraphicModels(l))
          val oct = Util.insertTrees(objects)
          menuOptions(objects, oct)

        case "2" =>
          menuOptions(graphics, applyScale(mainOct))

        case "3" =>
          menuOptions(graphics, applyMapColour(mainOct))

        case "4" =>
          saveStateOptions() match {
            case "1" =>
              Util.saveGraphicModelsState(graphics)
              confScene(mainOct)

            case "2" => confScene(mainOct)
            case "3" =>
              println("Please choose a valid option!")
              saveStateOptions()

          }
        case "5" => sys.exit()
        case _ =>
          println("Please choose a valid option!")
          menuOptions(graphics, mainOct)
      }
    }

    def menu(): Unit = {
      if (Util.getFilesNameFromDirectory.length == 0) {
        println("Does not exist any configuration file on the directory")
        val l1 = List()
        val graphics = Util.getGraphicModels(l1)
        val mainOct = Util.insertTrees(graphics)
        menuOptions(graphics, mainOct)
      }
      else {
        println("Type the option of the configuration file ")
        val a: ListBuffer[String] = Util.getFilesNameFromDirectory
        displayFiles(a)
        val fileName = readLine
        try {
          if (fileName.toInt <= a.length - 1 && fileName.toInt >= 0) {
            val file = Source.fromFile("src/" + a(fileName.toInt))
            val l1 = file.getLines.toList
            val graphics = Util.getGraphicModels(l1)
            val mainOct = Util.insertTrees(graphics)
            file.close()
            menuOptions(graphics, mainOct)
          }
          else {
            println("Invalid option! Please choose a valid option!")
            menu()
          }
        }
        catch {
          case e: FileNotFoundException => println(a(fileName.toInt) + " file not found")
            println("Do you want to choose another file? 1 - Yes or 2 - No")
            readLine match {
              case "1" => menu()
              case "2" => sys.exit()
              case _ =>
                println("Invalid option! Please choose 1 or 2")
                menu()
            }
        }
      }
    }

    //   menu()
    def confScene(oct: Octree[Placement]) {
      Util.addToWorld(oct, InitSubScene.worldRoot)
      val partitions = Util.makeTreePartitions(oct)
      Util.addPartitionsToWorld(partitions, InitSubScene.worldRoot) // + wired boxes
      Util.changeColor(partitions, InitSubScene.camVolume)
      val scene = new Scene(InitSubScene.root, 810, 610, true, SceneAntialiasing.BALANCED)

      //Mouse left click interaction
      scene.setOnMouseClicked(event => {
        InitSubScene.camVolume.setTranslateX(InitSubScene.camVolume.getTranslateX + 2)
        InitSubScene.worldRoot.getChildren.removeAll()
        // metodo T3
        Util.changeColor(partitions, InitSubScene.camVolume)
      })

      //setup and start the Stage
      stage.setTitle("PPM Project 21/22")
      stage.setScene(scene)
      stage.show()
    }
  }

  override def init(): Unit = {
    println("init")
  }

  override def stop(): Unit = {
    println("stopped")
  }
}

object FxApp {

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[Main], args: _*)
  }
}