import com.sun.prism.image.Coords
import javafx.application.Application
import javafx.geometry.{Insets, Point3D, Pos}
import javafx.scene.paint.PhongMaterial
import javafx.scene.shape._
import javafx.scene.transform.{Rotate, Translate}
import javafx.scene.{Group, Node}
import javafx.stage.Stage
import javafx.scene.layout.StackPane
import javafx.scene.paint.Color
import javafx.scene.{PerspectiveCamera, Scene, SceneAntialiasing, SubScene}

import java.io.FileNotFoundException
import scala.io.Source
import scala.language.postfixOps

class Main extends Application {
  //TEST
  //Auxiliary types
  type Point = (Double, Double, Double)
  type Size = Double
  type Placement = (Point, Size) //1st point: origin, 2nd point: size

  //Materials to be applied to the 3D objects
  val redMaterial = new PhongMaterial()
  redMaterial.setDiffuseColor(Color.rgb(150, 0, 0))

  val greenMaterial = new PhongMaterial()
  greenMaterial.setDiffuseColor(Color.rgb(0, 255, 0))

  val blueMaterial = new PhongMaterial()
  blueMaterial.setDiffuseColor(Color.rgb(0, 0, 150))

  val yellowMaterial = new PhongMaterial()
  yellowMaterial.setDiffuseColor(Color.rgb(255, 255, 0))

  //Shape3D is an abstract class that extends javafx.scene.Node
  //Box and Cylinder are subclasses of Shape3D
  type Section = (Placement, List[Node]) //example: ( ((0.0,0.0,0.0), 2.0), List(new Cylinder(0.5, 1, 10)))

  case class NodeDepthOctant(shape: Shape3D, division: Box, depth: Int, octantNumber: Int, parentOctantNumber: Int)

  /*
    Additional information about JavaFX basic concepts (e.g. Stage, Scene) will be provided in week7
   */
  override def start(stage: Stage): Unit = {

    //Get and print program arguments (args: Array[String])
    val params = getParameters
    println("Program arguments:" + params.getRaw)


    //3D objects
    val lineX = new Line(0, 0, 200, 0)
    lineX.setStroke(Color.GREEN)

    val lineY = new Line(0, 0, 0, 200)
    lineY.setStroke(Color.YELLOW)

    val lineZ = new Line(0, 0, 200, 0)
    lineZ.setStroke(Color.LIGHTSALMON)
    lineZ.getTransforms().add(new Rotate(-90, 0, 0, 0, Rotate.Y_AXIS))

    val camVolume = new Cylinder(10, 50, 10)
    camVolume.setTranslateX(1)
    camVolume.getTransforms().add(new Rotate(45, 0, 0, 0, Rotate.X_AXIS))
    camVolume.setMaterial(blueMaterial)
    camVolume.setDrawMode(DrawMode.LINE)

    val wiredBox = new Box(32, 32, 32)
    wiredBox.setTranslateX(16)
    wiredBox.setTranslateY(16)
    wiredBox.setTranslateZ(16)
    wiredBox.setMaterial(redMaterial)
    wiredBox.setDrawMode(DrawMode.LINE)


    // 3D objects (group of nodes - javafx.scene.Node) that will be provide to the subScene
    val worldRoot: Group = new Group(camVolume, lineX, lineY, lineZ, wiredBox)
    try {
      val l1 = Source.fromFile("src/conf.txt").getLines.toList
    }
    catch{
      case e:FileNotFoundException => println("Conf file not found")
    }
    val l1 = Source.fromFile("src/conf.txt").getLines.toList
    val graphics = getGraphicModels(l1)
    println(graphics)
    val mainOct = insertTrees(graphics)
    println(mainOct)

    // val worldFromTextRoot: Group = getTextGroup(graphics,worldRoot)

    addToWorld(mainOct, worldRoot)
    changeColor(mainOct, camVolume)
    //scaleOctree(0.5, mainOct)
    //mapColourEffect(sepia, mainOct)


    // Camera
    val camera = new PerspectiveCamera(true)

    val cameraTransform = new CameraTransformer
    cameraTransform.setTranslate(0, 0, 0)
    cameraTransform.getChildren.add(camera)
    camera.setNearClip(0.1)
    camera.setFarClip(10000.0)

    camera.setTranslateZ(-500)
    camera.setFieldOfView(20)
    cameraTransform.ry.setAngle(-45.0)
    cameraTransform.rx.setAngle(-45.0)
    worldRoot.getChildren.add(cameraTransform)

    // SubScene - composed by the nodes present in the worldRoot
    val subScene = new SubScene(worldRoot, 800, 600, true, SceneAntialiasing.BALANCED)
    subScene.setFill(Color.DARKSLATEGRAY)
    subScene.setCamera(camera)


    // CameraView - an additional perspective of the environment
    val cameraView = new CameraView(subScene)
    cameraView.setFirstPersonNavigationEabled(true)
    cameraView.setFitWidth(350)
    cameraView.setFitHeight(225)
    cameraView.getRx.setAngle(-45)
    cameraView.getT.setZ(-100)
    cameraView.getT.setY(-500)
    cameraView.getCamera.setTranslateZ(-50)
    cameraView.startViewing

    // Position of the CameraView: Right-bottom corner
    StackPane.setAlignment(cameraView, Pos.BOTTOM_RIGHT)
    StackPane.setMargin(cameraView, new Insets(5))

    // Scene - defines what is rendered (in this case the subScene and the cameraView)
    val root = new StackPane(subScene, cameraView)
    subScene.widthProperty.bind(root.widthProperty)
    subScene.heightProperty.bind(root.heightProperty)

    val scene = new Scene(root, 810, 610, true, SceneAntialiasing.BALANCED)

    //Mouse left click interaction
    scene.setOnMouseClicked((event) => {
      camVolume.setTranslateX(camVolume.getTranslateX + 2)
      worldRoot.getChildren.removeAll()
      // metodo T3
      changeColor(mainOct, camVolume)
    })

    //setup and start the Stage
    stage.setTitle("PPM Project 21/22")
    stage.setScene(scene)
    stage.show

  }

  override def init(): Unit = {
    println("init")
  }

  override def stop(): Unit = {
    println("stopped")
  }


  //T1
  def novoObj(s: Shape3D, a: Array[String]): Shape3D = {
    val numPattern = "[0-9]+".r
    val getRGB = numPattern.findAllIn(a(1)).toArray
    val rgb = new PhongMaterial(Color.rgb(getRGB(0).toInt,getRGB(1).toInt,getRGB(2).toInt))

    // if (a(1) == "(150,0,0)") {
//      s.setMaterial(redMaterial)
//    } else if (a(1) == "(0,255,0)") {
//
//      s.setMaterial(greenMaterial)
//    } else if (a(1) == "(0,0,150)") {
//      s.setMaterial(blueMaterial)
//    } else if (a(1) == "(255,255,0)") {
//      s.setMaterial(yellowMaterial)
//    }
    s.setMaterial(rgb)
    s.setTranslateX(a(2).toInt)
    s.setTranslateY(a(3).toInt)
    s.setTranslateZ(a(4).toInt)
    s.setScaleX(a(5).toInt)
    s.setScaleY(a(6).toInt)
    s.setScaleZ(a(7).toInt)
    s
  }

  def getGraphicModels(l: List[String]): List[Shape3D] = {
    l match {
      case Nil => Nil
      case h :: t => {
        val arg = h.split(" ")
        if (h.startsWith("Cylinder")) {
          val x = new Cylinder(0.5, 1, 10)
          val obj = novoObj(x, arg)
          obj :: getGraphicModels(t)
        }
        else if(h.startsWith("Box")) {
          val x = new Box(1, 1, 1)
          val obj = novoObj(x, arg)
          obj :: getGraphicModels(t)
        } else{
          getGraphicModels(t)
        }
      }
    }
  }

  def getTextGroup(l: List[Shape3D], worldObjects: Group = new Group()): Group = {
    l match {
      // case Nil => new Group(camVolume, lineX, lineY, lineZ)
      case Nil => worldObjects
      case h :: t => {
        val x = getTextGroup(t, worldObjects)
        x.getChildren.add(h)
        x
      }
    }
  }


  //método T3
  def addToWorld(oct: Octree[Placement], worldObjects: Group): Unit = {
    oct match {
      case OcEmpty => Nil
      case OcLeaf(section: Section) => {
        section._2.map(x => {
          val y = x
          worldObjects.getChildren.remove(x)
          worldObjects.getChildren.add(y)
        })
      }
      case OcNode(coords, up_00, up_01, up_10, up_11, down_00, down_01, down_10, down_11) => {
        addToWorld(up_00, worldObjects)
        addToWorld(up_01, worldObjects)
        addToWorld(up_10, worldObjects)
        addToWorld(up_11, worldObjects)
        addToWorld(down_00, worldObjects)
        addToWorld(down_01, worldObjects)
        addToWorld(down_10, worldObjects)
        addToWorld(down_11, worldObjects)
      }
    }
  }

  def insertTrees(g: List[Shape3D], oct: Octree[Placement] = new OcNode[Placement](((0, 0, 0), 32.0), OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty), placement: Placement = ((0, 0, 0), 32.0)): Octree[Placement] = {
    g match {
      // case Nil => new Group(camVolume, lineX, lineY, lineZ)
      case Nil => oct
      case h :: t => {
        insertTrees(t, insertTree(h, oct, placement), placement)
      }
    }
  }

  def insertTree(shape: Shape3D, t: Octree[Placement], placement: Placement = ((0, 0, 0), 32.0)): Octree[Placement] = {

    def isContained(shape: Shape3D, placement: Placement): Boolean = {
      val oct = new Box(placement._2, placement._2, placement._2)
      oct.setTranslateX(placement._1._1)
      oct.setTranslateY(placement._1._2)
      oct.setTranslateZ(placement._1._3)
      oct.getBoundsInParent.contains(shape.getBoundsInParent)
    }

    def getPlacement(orig: Placement, pos: String): Placement = {
      val newSize = orig._2 / 2
      val posAdjustment = orig._2 / 4
      if (orig._1 == (0, 0, 0)) {
        pos match {
          case "up_00" =>
            ((posAdjustment, posAdjustment, posAdjustment), newSize)
          case "up_01" =>
            ((newSize + posAdjustment, posAdjustment, posAdjustment), newSize)
          case "up_10" =>
            ((posAdjustment, posAdjustment, newSize + posAdjustment), newSize)
          case "up_11" =>
            ((newSize + posAdjustment, posAdjustment, newSize + posAdjustment), newSize)
          case "down_00" =>
            ((newSize + posAdjustment, newSize + posAdjustment, newSize + posAdjustment), newSize)
          case "down_01" =>
            ((posAdjustment, newSize + posAdjustment, newSize + posAdjustment), newSize)
          case "down_10" =>
            ((posAdjustment, newSize + posAdjustment, posAdjustment), newSize)
          case "down_11" =>
            ((newSize + posAdjustment, newSize + posAdjustment, posAdjustment), newSize)
        }
      }
      else {
        pos match {
          case "up_00" =>
            ((orig._1._1 - posAdjustment, orig._1._2 - posAdjustment, orig._1._3 - posAdjustment), newSize)
          case "up_01" =>
            ((orig._1._1 + posAdjustment, orig._1._2 - posAdjustment, orig._1._3 - posAdjustment), newSize)
          case "up_10" =>
            ((orig._1._1 - posAdjustment, orig._1._2 - posAdjustment, orig._1._3 + posAdjustment), newSize)
          case "up_11" =>
            ((orig._1._1 + posAdjustment, orig._1._2 - posAdjustment, orig._1._3 + posAdjustment), newSize)
          case "down_00" =>
            ((orig._1._1 - posAdjustment, orig._1._2 + posAdjustment, orig._1._3 - posAdjustment), newSize)
          case "down_01" =>
            ((orig._1._1 - posAdjustment, orig._1._2 + posAdjustment, orig._1._3 + posAdjustment), newSize)
          case "down_10" =>
            ((orig._1._1 - posAdjustment, orig._1._2 + posAdjustment, orig._1._3 + posAdjustment), newSize)
          case "down_11" =>
            ((orig._1._1 + posAdjustment, orig._1._2 + posAdjustment, orig._1._3 + posAdjustment), newSize)
        }
      }
    }

    def createPartition(placement: Placement): Box = {
      val partition = new Box(placement._2, placement._2, placement._2)
      partition.setTranslateX(placement._1._1)
      partition.setTranslateY(placement._1._2)
      partition.setTranslateZ(placement._1._3)
      partition.setDrawMode(DrawMode.LINE)
      partition
    }

    def isContained1(shape: Shape3D, placement: Placement): (Boolean, Placement, String) = {

      val up_00 = createPartition(getPlacement(placement, "up_00")).getBoundsInParent.contains(shape.getBoundsInParent)
      val up_01 = createPartition(getPlacement(placement, "up_01")).getBoundsInParent.contains(shape.getBoundsInParent)
      val up_10 = createPartition(getPlacement(placement, "up_10")).getBoundsInParent.contains(shape.getBoundsInParent)
      val up_11 = createPartition(getPlacement(placement, "up_11")).getBoundsInParent.contains(shape.getBoundsInParent)
      val down_00 = createPartition(getPlacement(placement, "down_00")).getBoundsInParent.contains(shape.getBoundsInParent)
      val down_01 = createPartition(getPlacement(placement, "down_01")).getBoundsInParent.contains(shape.getBoundsInParent)
      val down_10 = createPartition(getPlacement(placement, "down_10")).getBoundsInParent.contains(shape.getBoundsInParent)
      val down_11 = createPartition(getPlacement(placement, "down_11")).getBoundsInParent.contains(shape.getBoundsInParent)

      if (up_00)
        (up_00, getPlacement(placement, "up_00"), "up_00")
      else if (up_01)
        (up_01, getPlacement(placement, "up_01"), "up_01")
      else if (up_10)
        (up_10, getPlacement(placement, "up_10"), "up_10")
      else if (up_11)
        (up_11, getPlacement(placement, "up_11"), "up_11")
      else if (down_00)
        (down_00, getPlacement(placement, "down_00"), "down_00")
      else if (down_01)
        (down_01, getPlacement(placement, "down_01"), "down_01")
      else if (down_10)
        (down_10, getPlacement(placement, "down_10"), "down_10")
      else if (down_11)
        (down_11, getPlacement(placement, "down_11"), "down_11")
      else
        (false, ((0.0, 0.0, 0.0), 0.0), "")
    }

    t match {

      case OcEmpty =>
        if (!isContained(shape, placement))
          OcEmpty
        else if (isContained(shape, getPlacement(placement, "up_00")))
          OcNode(placement, insertTree(shape, OcEmpty, getPlacement(placement, "up_00")), OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty)
        else if (isContained(shape, getPlacement(placement, "up_01")))
          OcNode(placement, OcEmpty, insertTree(shape, OcEmpty, getPlacement(placement, "up_01")), OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty)
        else if (isContained(shape, getPlacement(placement, "up_10")))
          OcNode(placement, OcEmpty, OcEmpty, insertTree(shape, OcEmpty, getPlacement(placement, "up_10")), OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty)
        else if (isContained(shape, getPlacement(placement, "up_11")))
          OcNode(placement, OcEmpty, OcEmpty, OcEmpty, insertTree(shape, OcEmpty, getPlacement(placement, "up_11")), OcEmpty, OcEmpty, OcEmpty, OcEmpty)
        else if (isContained(shape, getPlacement(placement, "down_00")))
          OcNode(placement, OcEmpty, OcEmpty, OcEmpty, OcEmpty, insertTree(shape, OcEmpty, getPlacement(placement, "down_00")), OcEmpty, OcEmpty, OcEmpty)
        else if (isContained(shape, getPlacement(placement, "down_01")))
          OcNode(placement, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, insertTree(shape, OcEmpty, getPlacement(placement, "down_01")), OcEmpty, OcEmpty)
        else if (isContained(shape, getPlacement(placement, "down_10")))
          OcNode(placement, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, insertTree(shape, OcEmpty, getPlacement(placement, "down_10")), OcEmpty)
        else if (isContained(shape, getPlacement(placement, "down_11")))
          OcNode(placement, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, insertTree(shape, OcEmpty, getPlacement(placement, "down_11")))

        else {
          OcLeaf((placement, List(shape, createPartition(placement))))
        }

      case OcNode(coords, up_00, up_01, up_10, up_11, down_00, down_01, down_10, down_11) =>
        if (isContained(shape, coords) && up_00.isInstanceOf[OcLeaf[Placement, Section]] && isContained(shape, up_00.asInstanceOf[OcLeaf[Placement, Section]].section._1) == false) {
          val s: Section = new Section(coords, up_00.asInstanceOf[OcLeaf[Placement, Section]].section._2.filter(x => x.asInstanceOf[Shape3D].getDrawMode != DrawMode.LINE).concat(List(shape, createPartition(coords))))
          OcLeaf(s)
          val o: Octree[Placement] = OcNode(coords,
            OcLeaf(s),
            insertTree(shape, up_01, getPlacement(placement, "up_01")),
            insertTree(shape, up_10, getPlacement(placement, "up_10")),
            insertTree(shape, up_11, getPlacement(placement, "up_11")),
            insertTree(shape, down_00, getPlacement(placement, "down_00")),
            insertTree(shape, down_01, getPlacement(placement, "down_01")),
            insertTree(shape, down_10, getPlacement(placement, "down_10")),
            insertTree(shape, down_11, getPlacement(placement, "down_11")))

          if (o.asInstanceOf[OcNode[Placement]].up_00.isInstanceOf[OcLeaf[Placement, Section]] && o.asInstanceOf[OcNode[Placement]].up_00.asInstanceOf[OcLeaf[Placement, Section]].section._1 == o.asInstanceOf[OcNode[Placement]].coords)
            o.asInstanceOf[OcNode[Placement]].up_00
          else
            o
        }
        else
          OcNode(coords,
            insertTree(shape, up_00, getPlacement(placement, "up_00")),
            insertTree(shape, up_01, getPlacement(placement, "up_01")),
            insertTree(shape, up_10, getPlacement(placement, "up_10")),
            insertTree(shape, up_11, getPlacement(placement, "up_11")),
            insertTree(shape, down_00, getPlacement(placement, "down_00")),
            insertTree(shape, down_01, getPlacement(placement, "down_01")),
            insertTree(shape, down_10, getPlacement(placement, "down_10")),
            insertTree(shape, down_11, getPlacement(placement, "down_11")))


      case OcLeaf(section: Section) =>
        if (isContained(shape, section._1)) {
          val newList: List[Node] = section._2.concat(List(shape))
          val newSection = new Section(section._1, newList)
          OcLeaf(newSection)
        }
        else
          OcLeaf(section)
    }
  }


  //método T3
  def changeColor(oct: Octree[Placement], intersectingObject: Shape3D): Octree[Placement] = {
    oct match {
      case OcEmpty => OcEmpty
      case OcLeaf(section: Section) =>
        val l: List[Node] = section._2.map(x => {
          val y = x
          if (x.asInstanceOf[Shape3D].getDrawMode == DrawMode.LINE) {
            if (intersectingObject.getBoundsInParent().intersects(x.getBoundsInParent)) {
              y.asInstanceOf[Shape3D].setMaterial(yellowMaterial)
            }
            else {
              if (x.asInstanceOf[Shape3D].getMaterial != redMaterial) {
                y.asInstanceOf[Shape3D].setMaterial((redMaterial))
              }
            }
          }
          y
        })
        OcLeaf(section._1, l)
      case OcNode(coords, up_00, up_01, up_10, up_11, down_00, down_01, down_10, down_11) => OcNode(coords, changeColor(up_00, intersectingObject), changeColor(up_01, intersectingObject), changeColor(up_10, intersectingObject),
        changeColor(up_11, intersectingObject), changeColor(down_00, intersectingObject), changeColor(down_01, intersectingObject), changeColor(down_10, intersectingObject), changeColor(down_11, intersectingObject))
    }
  }

  //T4 método 4 sobre a octree
  def scaleOctree(fact: Double, oct: Octree[Placement]): Octree[Placement] = {
    if (fact == 0.5 || fact == 2) {
      oct match {
        case OcEmpty => OcEmpty
        case OcLeaf(section) => {
          val a = (section.asInstanceOf[Section]._1._1._1 * fact)
          val b = (section.asInstanceOf[Section]._1._1._2 * fact)
          val c = (section.asInstanceOf[Section]._1._1._3 * fact)
          val s = (section.asInstanceOf[Section]._1._2 * fact)

          val l: List[Node] = section.asInstanceOf[Section]._2.map(w => {
            w.setScaleX(w.getScaleX * fact)
            w.setScaleY(w.getScaleY * fact)
            w.setScaleZ(w.getScaleZ * fact)
            w.setTranslateX(w.getTranslateX * fact)
            w.setTranslateY(w.getTranslateY * fact)
            w.setTranslateZ(w.getTranslateZ * fact)
            w
          })

          OcLeaf(((a, b, c), s), l)
        }
        case OcNode(coords, up_00, up_01, up_10, up_11, down_00, down_01, down_10, down_11) => {
          val c: Placement = ((coords._1._1 * fact, coords._1._2 * fact, coords._1._3 * fact), coords._2 * fact)

          OcNode(c, scaleOctree(fact, up_00), scaleOctree(fact, up_01), scaleOctree(fact, up_10),
            scaleOctree(fact, up_11), scaleOctree(fact, down_00), scaleOctree(fact, down_01), scaleOctree(fact, down_10), scaleOctree(fact, down_11))
        }
      }
    }
    else {
      oct
    }
  }

  //T5 método 5 sobre a octree
  def mapColourEffect(func: Color => Color, oct: Octree[Placement]): Octree[Placement] = {
    oct match {
      case OcEmpty => OcEmpty
      case OcLeaf(section) => {
        section.asInstanceOf[Section]._2.map(x => {
          if (x.asInstanceOf[Shape3D].getDrawMode != DrawMode.LINE) {
            val c = func(x.asInstanceOf[Shape3D].getMaterial().asInstanceOf[PhongMaterial].getDiffuseColor)
            val m = new PhongMaterial()
            m.setDiffuseColor(c)
            x.asInstanceOf[Shape3D].setMaterial(m)
            x
          }
          else
            x
        })
        OcLeaf(section)
      }
      case OcNode(coords, up_00, up_01, up_10, up_11, down_00, down_01, down_10, down_11) => {
        OcNode(coords, mapColourEffect(func, up_00), mapColourEffect(func, up_01), mapColourEffect(func, up_10),
          mapColourEffect(func, up_11), mapColourEffect(func, down_00), mapColourEffect(func, down_01), mapColourEffect(func, down_10), mapColourEffect(func, down_11))
      }
    }
  }

  //Efeitos T5
  def sepia(c: Color): Color = {
    val r = (0.40 * c.getRed.toInt + 0.77 * c.getGreen.toInt + 0.20 * c.getBlue.toInt).toInt
    val g = (0.35 * c.getRed.toInt + 0.69 * c.getGreen.toInt + 0.17 * c.getBlue.toInt).toInt
    val b = (0.27 * c.getRed.toInt + 0.53 * c.getGreen.toInt + 0.13 * c.getBlue.toInt).toInt

    if (r > 255)
      Color.rgb(255, g, b)
    else if (g > 255)
      Color.rgb(r, 255, b)
    else if (b > 255)
      Color.rgb(r, g, 255)
    else
      Color.rgb(r, g, b)
  }

  def removeGreenComponent(c: Color): Color = {
    Color.rgb((c.getRed * 255).toInt, 0, (c.getBlue * 255).toInt)
  }


  //Usado para testes
  def getPlacement(orig: Placement, pos: String): Placement = {
    val newSize = orig._2 / 2
    pos match {

      case "up_00" =>
        ((orig._1._1 - newSize / 2, orig._1._2 - newSize / 2, orig._1._3 - newSize / 2), newSize)
      case "up_01" =>
        ((orig._1._1 + newSize / 2, orig._1._2 - newSize / 2, orig._1._3 - newSize / 2), newSize)
      case "up_10" =>
        ((orig._1._1 - newSize / 2, orig._1._2 - newSize / 2, orig._1._3 + newSize / 2), newSize)
      case "up_11" =>
        ((orig._1._1 + newSize / 2, orig._1._2 - newSize / 2, orig._1._3 + newSize / 2), newSize)
      case "down_00" =>
        ((orig._1._1 - newSize / 2, orig._1._2 + newSize / 2, orig._1._3 - newSize / 2), newSize)
      case "down_01" =>
        ((orig._1._1 - newSize / 2, orig._1._2 + newSize / 2, orig._1._3 + newSize / 2), newSize)
      case "down_10" =>
        ((orig._1._1 - newSize / 2, orig._1._2 + newSize / 2, orig._1._3 + newSize / 2), newSize)
      case "down_11" =>
        ((orig._1._1 + newSize / 2, orig._1._2 + newSize / 2, orig._1._3 + newSize / 2), newSize)
    }

  }



  /*
    //Usado para testes
    def getOctant(octantNumber: Int, universe: Box): Box = {
      val octSize = universe.getHeight / 2
      val oct = new Box(octSize, octSize, octSize)
      octantNumber match {
        case 1 =>
          oct.setTranslateX(universe.getTranslateX + octSize / 2)
          oct.setTranslateY(universe.getTranslateY + octSize / 2)
          oct.setTranslateZ(universe.getTranslateZ + octSize / 2)
          oct
        case 2 =>
          oct.setTranslateX(universe.getTranslateX - octSize / 2)
          oct.setTranslateY(universe.getTranslateY + octSize / 2)
          oct.setTranslateZ(universe.getTranslateZ + octSize / 2)
          oct
        case 3 =>
          oct.setTranslateX(universe.getTranslateX + octSize / 2)
          oct.setTranslateY(universe.getTranslateY - octSize / 2)
          oct.setTranslateZ(universe.getTranslateZ + octSize / 2)
          oct
        case 4 =>
          oct.setTranslateX(universe.getTranslateX + octSize / 2)
          oct.setTranslateY(universe.getTranslateY + octSize / 2)
          oct.setTranslateZ(universe.getTranslateZ - octSize / 2)
          oct
        case 5 =>
          oct.setTranslateX(universe.getTranslateX - octSize / 2)
          oct.setTranslateY(universe.getTranslateY - octSize / 2)
          oct.setTranslateZ(universe.getTranslateZ + octSize / 2)
          oct
        case 6 =>
          oct.setTranslateX(universe.getTranslateX - octSize / 2)
          oct.setTranslateY(universe.getTranslateY + octSize / 2)
          oct.setTranslateZ(universe.getTranslateZ - octSize / 2)
          oct
        case 7 =>
          oct.setTranslateX(universe.getTranslateX + octSize / 2)
          oct.setTranslateY(universe.getTranslateY - octSize / 2)
          oct.setTranslateZ(universe.getTranslateZ - octSize / 2)
          oct
        case 8 =>
          oct.setTranslateX(universe.getTranslateX - octSize / 2)
          oct.setTranslateY(universe.getTranslateY - octSize / 2)
          oct.setTranslateZ(universe.getTranslateZ - octSize / 2)
          oct
      }
    }
    def getNodeDepthOctant(obj:Shape3D, depth:Int=0, universe:Box,octant:Int=0, parentOctant:Int=0):NodeDepthOctant = {
      def getOctant(octantNumber:Int): Box= {
        val octSize = universe.getHeight / 2
        val oct = new Box(octSize, octSize, octSize)
        octantNumber match {
          case 1 =>
            oct.setTranslateX(universe.getTranslateX + octSize / 2)
            oct.setTranslateY(universe.getTranslateY + octSize / 2)
            oct.setTranslateZ(universe.getTranslateZ + octSize / 2)
            oct
          case 2 =>
            oct.setTranslateX(universe.getTranslateX - octSize / 2)
            oct.setTranslateY(universe.getTranslateY + octSize / 2)
            oct.setTranslateZ(universe.getTranslateZ + octSize / 2)
            oct
          case 3 =>
            oct.setTranslateX(universe.getTranslateX + octSize / 2)
            oct.setTranslateY(universe.getTranslateY - octSize / 2)
            oct.setTranslateZ(universe.getTranslateZ + octSize / 2)
            oct
          case 4 =>
            oct.setTranslateX(universe.getTranslateX + octSize / 2)
            oct.setTranslateY(universe.getTranslateY + octSize / 2)
            oct.setTranslateZ(universe.getTranslateZ - octSize / 2)
            oct
          case 5 =>
            oct.setTranslateX(universe.getTranslateX - octSize / 2)
            oct.setTranslateY(universe.getTranslateY - octSize / 2)
            oct.setTranslateZ(universe.getTranslateZ + octSize / 2)
            oct
          case 6 =>
            oct.setTranslateX(universe.getTranslateX - octSize / 2)
            oct.setTranslateY(universe.getTranslateY + octSize / 2)
            oct.setTranslateZ(universe.getTranslateZ - octSize / 2)
            oct
          case 7 =>
            oct.setTranslateX(universe.getTranslateX + octSize / 2)
            oct.setTranslateY(universe.getTranslateY - octSize / 2)
            oct.setTranslateZ(universe.getTranslateZ - octSize / 2)
            oct
          case 8 =>
            oct.setTranslateX(universe.getTranslateX - octSize / 2)
            oct.setTranslateY(universe.getTranslateY - octSize / 2)
            oct.setTranslateZ(universe.getTranslateZ - octSize / 2)
            oct
        }
      }
      val r = new NodeDepthOctant(obj : Shape3D,universe,depth-1,octant, parentOctant)
      if (getOctant(1).getBoundsInParent.contains(obj.asInstanceOf[Shape3D].getBoundsInParent))
        getNodeDepthOctant(obj, depth + 1, getOctant(1), 1,octant)
      else if (getOctant(2).getBoundsInParent.contains(obj.asInstanceOf[Shape3D].getBoundsInParent))
        getNodeDepthOctant(obj, depth + 1, getOctant(2), 2,octant)
      else if (getOctant(3).getBoundsInParent.contains(obj.asInstanceOf[Shape3D].getBoundsInParent))
        getNodeDepthOctant(obj, depth + 1, getOctant(3), 3,octant)
      else if (getOctant(4).getBoundsInParent.contains(obj.asInstanceOf[Shape3D].getBoundsInParent))
        getNodeDepthOctant(obj, depth + 1, getOctant(4), 4,octant)
      else if (getOctant(5).getBoundsInParent.contains(obj.asInstanceOf[Shape3D].getBoundsInParent))
        getNodeDepthOctant(obj, depth + 1, getOctant(5), 5,octant)
      else if (getOctant(6).getBoundsInParent.contains(obj.asInstanceOf[Shape3D].getBoundsInParent))
        getNodeDepthOctant(obj, depth + 1, getOctant(6), 6,octant)
      else if (getOctant(7).getBoundsInParent.contains(obj.asInstanceOf[Shape3D].getBoundsInParent))
        getNodeDepthOctant(obj, depth + 1, getOctant(7), 7,octant)
      else if (getOctant(8).getBoundsInParent.contains(obj.asInstanceOf[Shape3D].getBoundsInParent))
        getNodeDepthOctant(obj, depth + 1, getOctant(8), 8,octant)
      else
        r
    }
    // ----------------- TESTE DE OCTREE -----------------
    def hasObj(x: List[Shape3D], y:Node): Boolean = x match{
      case Nil => false
      case h::t => if(y.getBoundsInParent.contains(h.getBoundsInParent)) true  else hasObj(t,y)
    }
    def shapeToNode(s: List[Shape3D]): List[Node] = s match{
      case Nil => Nil
      case h::t => h.asInstanceOf[Node]::shapeToNode(t)
    }
    def getOcLeafs (root: Box): List[Node] = {
      val newSide = root.asInstanceOf[Box].getHeight/2
      val tx = root.asInstanceOf[Box].getTranslateX; val ty = root.asInstanceOf[Box].getTranslateY; val tz = root.asInstanceOf[Box].getTranslateZ
      val oc1 = new Box(tx+newSide, ty+newSide, tz+newSide)
      val oc2 = new Box(tx-newSide, ty+newSide, tz+newSide)
      val oc3 = new Box(tx+newSide, ty-newSide, tz+newSide)
      val oc4 = new Box(tx+newSide, ty+newSide, tz-newSide)
      val oc5 = new Box(tx-newSide, ty-newSide, tz+newSide)
      val oc6 = new Box(tx-newSide, ty+newSide, tz-newSide)
      val oc7 = new Box(tx+newSide, ty-newSide, tz-newSide)
      val oc8 = new Box(tx+newSide, ty+newSide, tz+newSide)
      List(oc1,oc2,oc3,oc4,oc5,oc6,oc7,oc8)
    }
    def setOcleafList(newOcleaf:Node, objs: List[Shape3D]): List[Node] ={
      objs match {
        case Nil => Nil
        case h::t => { if(newOcleaf.asInstanceOf[Shape3D].getBoundsInParent.contains(h.getBoundsInParent))
          h::setOcleafList(newOcleaf,t)
        else setOcleafList(newOcleaf, t)}
      }
    }
    def getOcTree(newOcleaf: Node, x:List[Shape3D]): Octree[Placement] = {
      if (hasObj(x, newOcleaf)) {
        //definir a lista de objetos dentro do nó
        val lst = setOcleafList(newOcleaf, x)
        //definir a section
        val s: Section = (((newOcleaf.getTranslateX, newOcleaf.getTranslateY, newOcleaf.getTranslateZ), newOcleaf.asInstanceOf[Box].getHeight), lst)
        //nova ocleaf, com novas coordenadas, tamanho e lista de objetos
        if (getTree(x,newOcleaf.asInstanceOf[Shape3D]) == OcEmpty){
          OcLeaf(s)
        }
        else getTree(x,newOcleaf.asInstanceOf[Shape3D])
      }
      else OcEmpty
    }
    def getTree(x:List[Shape3D], root: Shape3D):Octree[Placement] ={
      val h: Size = root.asInstanceOf[Box].getHeight
      val p: Placement = ((root.getTranslateX, root.getTranslateY, root.getTranslateZ),h)
      //val z = shapeToNode(x)
      //val s: Section = (((root.getTranslateX, root.getTranslateY, root.getTranslateZ),h),z)
      //val l = OcLeaf(s)
      //verificar se Ocnode é OcEmpty
      if(hasObj(x,root)){
        //Se tem Objetos gera OcTrees (OcLeafs ou OcEmpty) => criar novos nós (Octrees)
        val newSide = root.asInstanceOf[Box].getHeight/2
        val tx = root.asInstanceOf[Box].getTranslateX; val ty = root.asInstanceOf[Box].getTranslateY; val tz = root.asInstanceOf[Box].getTranslateZ
        val oc1 = new Box(tx+newSide, ty+newSide, tz+newSide)
        val oc2 = new Box(tx-newSide, ty+newSide, tz+newSide)
        val oc3 = new Box(tx+newSide, ty-newSide, tz+newSide)
        val oc4 = new Box(tx+newSide, ty+newSide, tz-newSide)
        val oc5 = new Box(tx-newSide, ty-newSide, tz+newSide)
        val oc6 = new Box(tx-newSide, ty+newSide, tz-newSide)
        val oc7 = new Box(tx+newSide, ty-newSide, tz-newSide)
        val oc8 = new Box(tx+newSide, ty+newSide, tz+newSide)
        // Verificar se os objetos ficam contidos nalgum dos novos nós
        if ((hasObj(x,oc1))||(hasObj(x,oc2))|| (hasObj(x,oc3)) || (hasObj(x,oc4) || (hasObj(x,oc5)) || (hasObj(x,oc6)) || (hasObj(x,oc7))) || hasObj(x,oc8)){
          OcNode[Placement](p,getOcTree(oc1.asInstanceOf[Node],x),
            getOcTree(oc2.asInstanceOf[Node],x),
            getOcTree(oc3.asInstanceOf[Node],x),
            getOcTree(oc4.asInstanceOf[Node],x),
            getOcTree(oc5.asInstanceOf[Node],x),
            getOcTree(oc6.asInstanceOf[Node],x),
            getOcTree(oc7.asInstanceOf[Node],x),
            getOcTree(oc8.asInstanceOf[Node],x))
        }
        // se os novos nós não contêm objectos => OcEmpty
        else OcEmpty
      }
      // Se o nó não contem objetos => OcEmpty
      else OcEmpty
    }
    // --------------- FIM DE TESTE DE OCTREE -------------
  */

}

object FxApp {

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[Main], args: _*)
  }
}

