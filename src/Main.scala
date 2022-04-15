import com.sun.prism.image.Coords
import javafx.application.Application
import javafx.geometry.Insets
import javafx.scene.paint.PhongMaterial
import javafx.scene.shape._
import javafx.scene.transform.{Rotate, Translate}
import javafx.scene.{Group, Node}
import javafx.stage.Stage
import javafx.geometry.Pos
import javafx.scene.layout.StackPane
import javafx.scene.paint.Color
import javafx.scene.{PerspectiveCamera, Scene, SceneAntialiasing, SubScene}

import scala.io.Source

class Main extends Application {
  //TEST
  //Auxiliary types
  type Point = (Double, Double, Double)
  type Size = Double
  type Placement = (Point, Size) //1st point: origin, 2nd point: size

  //Shape3D is an abstract class that extends javafx.scene.Node
  //Box and Cylinder are subclasses of Shape3D
  type Section = (Placement, List[Node])  //example: ( ((0.0,0.0,0.0), 2.0), List(new Cylinder(0.5, 1, 10)))
  //type NodeDepthOctant = (Node,Box,Int,Int) // Contains a node, the box where it's contained(octant), depth and octant number

  case class NodeDepthOctant(shape : Shape3D, division : Box, depth : Int, octantNumber : Int, parentOctantNumber : Int)


  /*
    Additional information about JavaFX basic concepts (e.g. Stage, Scene) will be provided in week7
   */
  override def start(stage: Stage): Unit = {

    //Get and print program arguments (args: Array[String])
    val params = getParameters
    println("Program arguments:" + params.getRaw)

    //Materials to be applied to the 3D objects
    val redMaterial = new PhongMaterial()
    redMaterial.setDiffuseColor(Color.rgb(150, 0, 0))

    val greenMaterial = new PhongMaterial()
    greenMaterial.setDiffuseColor(Color.rgb(0, 255, 0))

    val blueMaterial = new PhongMaterial()
    blueMaterial.setDiffuseColor(Color.rgb(0, 0, 150))

    val yellowMaterial = new PhongMaterial()
    yellowMaterial.setDiffuseColor(Color.rgb(255, 255, 0))

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

    val cylinder1 = new Cylinder(0.5, 1, 10)
    cylinder1.setTranslateX(2)
    cylinder1.setTranslateY(2)
    cylinder1.setTranslateZ(2)
    cylinder1.setScaleX(2)
    cylinder1.setScaleY(2)
    cylinder1.setScaleZ(2)
    cylinder1.setMaterial(greenMaterial)

    val cylinder2 = new Cylinder(0.5, 1, 10)
    cylinder2.setTranslateX(4)
    cylinder2.setTranslateY(2)
    cylinder2.setTranslateZ(2)
    cylinder2.setScaleX(2)
    cylinder2.setScaleY(10)
    cylinder2.setScaleZ(2)
    cylinder2.setMaterial(redMaterial)

    val box1 = new Box(1, 5, 1) //
    box1.setTranslateX(10)
    box1.setTranslateY(12)
    box1.setTranslateZ(5)
    box1.setMaterial(yellowMaterial)

    // 3D objects (group of nodes - javafx.scene.Node) that will be provide to the subScene
    val worldRoot: Group = new Group(wiredBox, camVolume, lineX, lineY, lineZ)

    // 3D objects from conf.txt

    /*    def getObjList(): List[String] ={
          Source.fromFile("src/conf.txt").getLines.toList
        }
        val l1 = getObjList()*/

    val l1 = Source.fromFile("src/conf.txt").getLines.toList

    def novoObj(s: Shape3D, a: Array[String]): Shape3D = {
      if (a(1) == "(150,0,0)") {
        s.setMaterial(redMaterial)
      } else if (a(1) == "(0,255,0)") {
        s.setMaterial(greenMaterial)
      } else if (a(1) == "(0,0,150)") {
        s.setMaterial(blueMaterial)
      } else if (a(1) == "(255,255,0)") {
        s.setMaterial(yellowMaterial)
      }
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
          else {
            val x = new Box(1, 1, 1)
            val obj = novoObj(x, arg)
            obj :: getGraphicModels(t)
          }
        }
      }
    }

    val graphics = getGraphicModels(l1)

    def getTextGroup(l: List[Shape3D]): Group = {
      l match {
        // case Nil => new Group(camVolume, lineX, lineY, lineZ)
        case Nil => new Group(camVolume)
        case h :: t => {
          val x = getTextGroup(t)
          x.getChildren.add(h)
          x
        }
      }
    }

    val worldFromTextRoot: Group = getTextGroup(graphics)

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
    //val subScene = new SubScene(worldRoot, 800, 600, true, SceneAntialiasing.BALANCED)
    val subScene = new SubScene(worldFromTextRoot, 800, 600, true, SceneAntialiasing.BALANCED)
    subScene.setFill(Color.DARKSLATEGRAY)
    subScene.setCamera(camera)

def  funcaoteste(g:List[Shape3D]):List[NodeDepthOctant] =
    {
      g match {
        // case Nil => new Group(camVolume, lineX, lineY, lineZ)
        case Nil => Nil
        case h :: t => {
          getNodeDepthOctant(h, 0, wiredBox, 1) :: funcaoteste(t)
        }
      }
    }

    /*
    def nodeDepthtoOctree(nodes:Map[Int, List[NodeDepthOctant]], depth:Int, ):Octree[Placement] =
      {

      }

     */


    val resultado = funcaoteste(graphics)
    //  resultado.foreach(x => {x._2.setDrawMode(DrawMode.LINE)
    //  worldFromTextRoot.getChildren.add(x._2)})


    //método T3
    def changeColor(lst: List[NodeDepthOctant]): List[NodeDepthOctant] = {
      lst match {
        case Nil => Nil
        case h :: t => {
          if (camVolume.getBoundsInParent().intersects(h.division.getBoundsInParent) && (h.division.getHeight != 32)) {
            h.division.setMaterial(yellowMaterial)
            h :: changeColor(t)
          }
          else {
            h.division.setMaterial(redMaterial)
            h :: changeColor(t)
          }
        }
      }
    }

    //método T4
    def scaleOctree(fact: Double, lst: List[NodeDepthOctant]): List[NodeDepthOctant]={
      lst match{
        case Nil=> Nil
        case h::t=> {
          if (h.division.getHeight != 32 && (fact == 0.5 || fact == 2)) {
            h.division.setScaleX(fact * h.division.getScaleX)
            h.division.setScaleY(fact * h.division.getScaleY)
            h.division.setScaleZ(fact * h.division.getScaleZ)
            if(wiredBox.getBoundsInParent().contains(h.shape.getBoundsInParent()))
              h :: scaleOctree(fact, t)
            else
              scaleOctree(fact, t)
          }
          else
            scaleOctree(fact, t)
        }
        }
      }
  //método 5
    def mapColourEffect(func:Color=>Color,lst: List[NodeDepthOctant]): List[NodeDepthOctant]= {
      lst.map(x=>{
        val c=func(x.shape.getMaterial().asInstanceOf[PhongMaterial].getDiffuseColor)
        val m = new PhongMaterial()
        m.setDiffuseColor(c)
        x.shape.setMaterial(m)
        x
      })
    }

    def sepia (c:Color):Color= {
      val r1 = (0.40 * c.getRed.toInt + 0.77 * c.getGreen.toInt + 0.20 * c.getBlue.toInt).toInt
      val g1 = (0.35 * c.getRed.toInt + 0.69 * c.getGreen.toInt + 0.17 * c.getBlue.toInt).toInt
      val b1 = (0.27 * c.getRed.toInt + 0.53 * c.getGreen.toInt + 0.13 * c.getBlue.toInt).toInt

      if (r1 > 255)
        Color.rgb(255, g1, b1)
      else if (g1 > 255)
        Color.rgb(r1, 255, b1)
      else if (b1 > 255)
        Color.rgb(r1, g1, 255)
      else
          Color.rgb(r1, g1, b1)
    }

    def removeGreenComponent (c:Color):Color= {Color.rgb((c.getRed*255).toInt,0,(c.getBlue*255).toInt)}

    changeColor(resultado)
    scaleOctree(0.5,resultado)
   // mapColourEffect(sepia,resultado)
    resultado.foreach(x => {
      x.division.setDrawMode(DrawMode.LINE)
      worldFromTextRoot.getChildren.add(x.division)
    })
    val nodesByDepth = resultado.groupBy(_.depth)
    val maxDepth = nodesByDepth.maxBy(_._1)
    val maxDepth2 = nodesByDepth.keysIterator.max
    println(maxDepth)
    println(maxDepth2)
    println(nodesByDepth)





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
      changeColor(resultado)
    })

    //setup and start the Stage
    stage.setTitle("PPM Project 21/22")
    stage.setScene(scene)
    stage.show


    //oct1 - example of an Octree[Placement] that contains only one Node (i.e. cylinder1)
    //In case of difficulties to implement task T2 this octree can be used as input for tasks T3, T4 and T5
    /*
           val placement1: Placement = ((0, 0, 0), 8.0)
           val sec1: Section = (((0.0,0.0,0.0), 4.0), List(cylinder2.asInstanceOf[Node],cylinder1.asInstanceOf[Node],box1.asInstanceOf[Node]))
           val ocLeaf1 = OcLeaf(sec1)
           val oct1:Octree[Placement] = OcNode[Placement](placement1, ocLeaf1, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty)
           val oct2:Octree[Placement] = OcNode[Placement](placement1, oct1, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty)
           //example of bounding boxes (corresponding to the octree oct1) added manually to the world
           val b2 = new Box(8,8,8)
           //translate because it is added by defaut to the coords (0,0,0)
           b2.setTranslateX(8/2)
           b2.setTranslateY(8/2)
           b2.setTranslateZ(8/2)
           b2.setMaterial(redMaterial)
           b2.setDrawMode(DrawMode.LINE)
           val b4 = new Box(4,4,4)
           //translate because it is added by defaut to the coords (0,0,0)
           b4.setTranslateX(4/2+4)
           b4.setTranslateY(4/2+4)
           b4.setTranslateZ(4/2+4)
           b4.setMaterial(redMaterial)
           b4.setDrawMode(DrawMode.LINE)
           //adding boxes b2 and b3 to the world
           worldRoot.getChildren.add(b2)
          // worldRoot.getChildren.add(b3)
           worldRoot.getChildren.add(b4)
           worldRoot.getChildren.add(cylinder2)
           worldRoot.getChildren.add(cylinder1)
           worldRoot.getChildren.add(box1)




       val b3 = new Box(4, 4, 4)
       //translate because it is added by defaut to the coords (0,0,0)
       b3.setTranslateX(4 / 2)
       b3.setTranslateY(4 / 2)
       b3.setTranslateZ(4 / 2)
       b3.setMaterial(redMaterial)
       b3.setDrawMode(DrawMode.LINE)
       worldRoot.getChildren.add(b3)


       //método 5 sobre a octree
       def mapColourEffect1(func:Color=>Color,oct:Octree[Placement]): Octree[Placement] = {
         oct match {
           case OcEmpty => OcEmpty
           case OcLeaf(section) => section.asInstanceOf[Section]._2.map(x=>{

             val c=func(x.asInstanceOf[Shape3D].getMaterial().asInstanceOf[PhongMaterial].getDiffuseColor)
             val m = new PhongMaterial()
             m.setDiffuseColor(c)
             x.asInstanceOf[Shape3D].setMaterial(m)
             val y = x
             worldRoot.getChildren.remove(x)
             worldRoot.getChildren.add(y)
             y
             })
           OcLeaf(section)
           case OcNode(coords, up_00, up_01, up_10, up_11, down_00, down_01, down_10, down_11) =>OcNode(coords, mapColourEffect1(func,up_00),mapColourEffect1(func,up_01),mapColourEffect1(func,up_10),
             mapColourEffect1(func,up_11),mapColourEffect1(func,down_00),mapColourEffect1(func,down_01),mapColourEffect1(func,down_10),mapColourEffect1(func,down_11))
         }
       }
       //método 4 sobre a octree
       def scaleOctree1(fact:Double, oct:Octree[Placement]):Octree[Placement] = {
         oct match {
           case OcEmpty => OcEmpty
           case OcLeaf(section) => if(fact==0.5||fact==2)
           { val s = section.asInstanceOf[Section]._1._2*fact
             val l:List[Node]=section.asInstanceOf[Section]._2.map(x=>{
               val y = x
               y.setScaleX(x.getScaleX*fact)
               y.setScaleY(x.getScaleY*fact)
               y.setScaleZ(x.getScaleZ*fact)
               worldRoot.getChildren.remove(x)
               if(wiredBox.getHeight()==32&&wiredBox.getBoundsInParent().contains(y.getBoundsInParent)) {
                 worldRoot.getChildren.add(y)
               }
               y
           })
             OcLeaf(s,l)
           }
           else
             OcLeaf(section)
           case OcNode(coords, up_00, up_01, up_10, up_11, down_00, down_01, down_10, down_11) => {
             val c:Placement = (coords._1, coords._2*fact)
             OcNode(c, scaleOctree1(fact, up_00), scaleOctree1(fact, up_01), scaleOctree1(fact, up_10),
               scaleOctree1(fact, up_11), scaleOctree1(fact, down_00), scaleOctree1(fact, down_01), scaleOctree1(fact, down_10), scaleOctree1(fact, down_11))
           }
         }
       }

      mapColourEffect1(removeGreenComponent,oct2)
       // mapColourEffect1(sepia,oct2)
       scaleOctree1(2,oct2)
   */
    /* Testes
        val octant1 = getOctant(1,wiredBox)
        octant1.setMaterial(redMaterial)
        octant1.setDrawMode(DrawMode.LINE)
        val octant2 = getOctant(2,wiredBox)
        octant1.setMaterial(redMaterial)
        val octant3 = getOctant(3,wiredBox)
        octant1.setMaterial(redMaterial)
        val octant4 = getOctant(4,wiredBox)
        octant1.setMaterial(redMaterial)
        val octant5 = getOctant(5,wiredBox)
        octant1.setMaterial(redMaterial)
        val suboctant1 = getOctant(1,octant1)
        val suboctant2 = getOctant(2,octant1)
        val suboctant3 = getOctant(3,octant1)
        val suboctant4 = getOctant(4,octant1)
        val suboctant5 = getOctant(5,octant1)
        val suboctant6 = getOctant(6,octant1)
        suboctant1.setMaterial(redMaterial)
        suboctant2.setMaterial(redMaterial)
        suboctant3.setMaterial(redMaterial)
        suboctant4.setMaterial(redMaterial)
        suboctant5.setMaterial(redMaterial)
        suboctant6.setMaterial(redMaterial)
        println(getOctant(1,wiredBox).getBoundsInParent.contains(b3.asInstanceOf[Shape3D].getBoundsInParent))
        println(getOctant(2,wiredBox).getBoundsInParent.contains(b3.asInstanceOf[Shape3D].getBoundsInParent))
        println(getOctant(3,wiredBox).getBoundsInParent.contains(b3.asInstanceOf[Shape3D].getBoundsInParent))
        println(getOctant(4,wiredBox).getBoundsInParent.contains(b3.asInstanceOf[Shape3D].getBoundsInParent))
        println(getOctant(5,wiredBox).getBoundsInParent.contains(b3.asInstanceOf[Shape3D].getBoundsInParent))
        println(getOctant(6,wiredBox).getBoundsInParent.contains(b3.asInstanceOf[Shape3D].getBoundsInParent))
        println(getOctant(7,wiredBox).getBoundsInParent.contains(b3.asInstanceOf[Shape3D].getBoundsInParent))
        println(getOctant(8,wiredBox).getBoundsInParent.contains(b3.asInstanceOf[Shape3D].getBoundsInParent))
    */


    //worldRoot.getChildren.add(octant1)
    //worldRoot.getChildren.add(octant2)
    //worldRoot.getChildren.add(octant3)
    //worldRoot.getChildren.add(octant4)
    //worldRoot.getChildren.add(octant5)
    //worldRoot.getChildren.add(suboctant1)
    //worldRoot.getChildren.add(suboctant2)
    //worldRoot.getChildren.add(suboctant3)
    //worldRoot.getChildren.add(suboctant4)
    //worldRoot.getChildren.add(suboctant5)
    //worldRoot.getChildren.add(suboctant6)


    //


  }

  override def init(): Unit = {
    println("init")
  }

  override def stop(): Unit = {
    println("stopped")
  }

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

  //Mover isto para outro sitio, acrescentar validações
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
      getNodeDepthOctant(obj, depth + 1, getOctant(1), 1)
    else if (getOctant(2).getBoundsInParent.contains(obj.asInstanceOf[Shape3D].getBoundsInParent))
      getNodeDepthOctant(obj, depth + 1, getOctant(2), 2)
    else if (getOctant(3).getBoundsInParent.contains(obj.asInstanceOf[Shape3D].getBoundsInParent))
      getNodeDepthOctant(obj, depth + 1, getOctant(3), 3)
    else if (getOctant(4).getBoundsInParent.contains(obj.asInstanceOf[Shape3D].getBoundsInParent))
      getNodeDepthOctant(obj, depth + 1, getOctant(4), 4)
    else if (getOctant(5).getBoundsInParent.contains(obj.asInstanceOf[Shape3D].getBoundsInParent))
      getNodeDepthOctant(obj, depth + 1, getOctant(5), 5)
    else if (getOctant(6).getBoundsInParent.contains(obj.asInstanceOf[Shape3D].getBoundsInParent))
      getNodeDepthOctant(obj, depth + 1, getOctant(6), 6)
    else if (getOctant(7).getBoundsInParent.contains(obj.asInstanceOf[Shape3D].getBoundsInParent))
      getNodeDepthOctant(obj, depth + 1, getOctant(7), 7)
    else if (getOctant(8).getBoundsInParent.contains(obj.asInstanceOf[Shape3D].getBoundsInParent))
      getNodeDepthOctant(obj, depth + 1, getOctant(8), 8)
    else
      r

  }


}

object FxApp {

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[Main], args: _*)
  }
}




