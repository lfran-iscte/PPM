import javafx.scene.{Group, Node}
import javafx.scene.paint.{Color, PhongMaterial}
import javafx.scene.shape.{Box, Cylinder, DrawMode, Shape3D}

import java.io.{BufferedWriter, File, FileNotFoundException, FileWriter}
import java.nio.file.{Files, Paths}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine

object Util {

  type Section = (Placement, List[Node]) //example: ( ((0.0,0.0,0.0), 2.0), List(new Cylinder(0.5, 1, 10)))
  type Point = (Double, Double, Double)
  type Size = Double
  type Placement = (Point, Size) //1st point: origin, 2nd point: size

  /*
    //Materials to be applied to the 3D objects
    val redMaterial = new PhongMaterial()
    redMaterial.setDiffuseColor(Color.rgb(150, 0, 0))

    val greenMaterial = new PhongMaterial()
    greenMaterial.setDiffuseColor(Color.rgb(0, 255, 0))

    val blueMaterial = new PhongMaterial()
    blueMaterial.setDiffuseColor(Color.rgb(0, 0, 150))

    val yellowMaterial = new PhongMaterial()
    yellowMaterial.setDiffuseColor(Color.rgb(255, 255, 0))
  */


  def getFilesNameFromDirectory: ListBuffer[String] = {
    val fileList = new ListBuffer[String]()
    try {
      val stream = Files.newDirectoryStream(Paths.get("src/"))
      stream.forEach(file => {
        if (file.getFileName.toFile.toString.endsWith(".txt"))
          fileList += file.getFileName.toFile.toString
      })
      fileList
    }
  }

  //T1
  def newShape(s: Shape3D, a: Array[String]): Shape3D = {

    val numPattern = "[0-9]+".r
    val getRGB = numPattern.findAllIn(a(1)).toArray
    val rgb = new PhongMaterial(Color.rgb(getRGB(0).toInt, getRGB(1).toInt, getRGB(2).toInt))

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
          val obj = newShape(x, arg)
          obj :: getGraphicModels(t)
        }
        else if (h.startsWith("Box")) {
          val x = new Box(1, 1, 1)
          val obj = newShape(x, arg)
          obj :: getGraphicModels(t)
        } else {
          getGraphicModels(t)
        }
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
          OcLeaf((placement, List(shape)))
        }

      case OcNode(coords, up_00, up_01, up_10, up_11, down_00, down_01, down_10, down_11) =>
        if (isContained(shape, getPlacement(placement, "down_00")))
          OcNode(coords, up_00, up_01, up_10, up_11, insertTree(shape, down_00, getPlacement(placement, "down_00")), down_01, down_10, down_11)
        else if (isContained(shape, getPlacement(placement, "down_01")))
          OcNode(coords, up_00, up_01, up_10, up_11, down_00, insertTree(shape, down_01, getPlacement(placement, "down_01")), down_10, down_11)
        else if (isContained(shape, getPlacement(placement, "down_10")))
          OcNode(coords, up_00, up_01, up_10, up_11, down_00, down_01, insertTree(shape, down_10, getPlacement(placement, "down_10")), down_11)
        else if (isContained(shape, getPlacement(placement, "down_11")))
          OcNode(coords, up_00, up_01, up_10, up_11, down_00, down_01, down_10, insertTree(shape, down_11, getPlacement(placement, "down_11")))
        else if (isContained(shape, getPlacement(placement, "up_00")))
          OcNode(coords, insertTree(shape, up_00, getPlacement(placement, "up_00")), up_01, up_10, up_11, down_00, down_01, down_10, down_11)
        else if (isContained(shape, getPlacement(placement, "up_01")))
          OcNode(coords, up_00, insertTree(shape, up_01, getPlacement(placement, "up_01")), up_10, up_11, down_00, down_01, down_10, down_11)
        else if (isContained(shape, getPlacement(placement, "up_10")))
          OcNode(coords, up_00, up_01, insertTree(shape, up_10, getPlacement(placement, "up_10")), up_11, down_00, down_01, down_10, down_11)
        else if (isContained(shape, getPlacement(placement, "up_11")))
          OcNode(coords, up_00, up_01, up_10, insertTree(shape, up_11, getPlacement(placement, "up_11")), down_00, down_01, down_10, down_11)
        else { // Se o modelo gráfico não cabe em nenhuma das sub-partições do nós, então vai buscar os modelos gráficos da folhas desse nó para os guardar na nova leaf
          val nodeObjs = getTreeGraphics(t) //get modelos gráficos do OcNode - Octree
          if (nodeObjs.isEmpty) { // Se ainda não existiam modelos gráficos na dependência do nó
            //val a = new Section(coords, List(shape, createPartition(coords)))
            val a = new Section(coords, List(shape))
            OcLeaf(a)
          } else { // Se ainda já existiam modelos gráficos na dependência do nó
            val s = new Section(coords, List(shape).concat(nodeObjs)) // cria a nova secao e acrescenta os modelos gráficos na dependência do nó
            OcLeaf(s) // o OcNode recebido para se inserir o modelo gráfico, é retornado como folha
          }
        }

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

  def getTreeGraphics(oc: Octree[Placement]): List[Shape3D] = {
    oc match {
      case OcEmpty => Nil
      case OcLeaf(section: Section) =>
        section._2.map(x => x.asInstanceOf[Shape3D])
      case OcNode(coords, up_00, up_01, up_10, up_11, down_00, down_01, down_10, down_11) =>
        (getTreeGraphics(up_00) ::: getTreeGraphics(up_01) ::: getTreeGraphics(up_10) ::: getTreeGraphics(up_11) ::: getTreeGraphics(down_00) ::: getTreeGraphics(down_01) ::: getTreeGraphics(down_10) ::: getTreeGraphics(down_11))
    }
  }

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


  def removeObjects(l: List[Shape3D], worldObjects: Group): Unit = {
    l match {
      case Nil => Nil

      case h :: t => {
        worldObjects.getChildren.remove(h)
        removeObjects(t, worldObjects)
      }
      case default => Nil
    }
  }

  def removeObjects(oct: Octree[Placement], worldObjects: Group): Octree[Placement] = {
    oct match {
      case OcEmpty => OcEmpty
      case OcLeaf(section) =>
        val l: List[Node] = section.asInstanceOf[Section]._2.map(w => {
          worldObjects.getChildren.remove(w)
          w
        })
        OcEmpty
      case OcNode(coords, up_00, up_01, up_10, up_11, down_00, down_01, down_10, down_11) =>
        OcNode(coords, removeObjects(up_00, worldObjects), removeObjects(up_01, worldObjects), removeObjects(up_10, worldObjects),
          removeObjects(up_11, worldObjects), removeObjects(down_00, worldObjects), removeObjects(down_01, worldObjects), removeObjects(down_10, worldObjects), removeObjects(down_11, worldObjects))
      case default => OcEmpty
    }
  }

  def changeColor(l: List[Shape3D], intersectingObject: Shape3D): Unit = {
    l match {
      case Nil => Nil
      case h :: t => {
        if (h.getDrawMode == DrawMode.LINE) {
          if (intersectingObject.getBoundsInParent.intersects(h.getBoundsInParent)) {
            h.setMaterial(InitSubScene.yellowMaterial)
          }
          else {
            if (h.getMaterial != InitSubScene.redMaterial) {
              h.setMaterial(InitSubScene.redMaterial)
            }
          }
        }
        changeColor(t, intersectingObject)
      }
    }
  }

  //T4 método 4 sobre a octree
  def scaleOctree(fact: Double, oct: Octree[Placement]): Octree[Placement] = {
    if (fact == 0.5 || fact == 2) {
      oct match {
        case OcEmpty => OcEmpty
        case OcLeaf(section) =>
          val a = section.asInstanceOf[Section]._1._1._1 * fact
          val b = section.asInstanceOf[Section]._1._1._2 * fact
          val c = section.asInstanceOf[Section]._1._1._3 * fact
          val s = section.asInstanceOf[Section]._1._2 * fact

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
        case OcNode(coords, up_00, up_01, up_10, up_11, down_00, down_01, down_10, down_11) =>
          val c: Placement = ((coords._1._1 * fact, coords._1._2 * fact, coords._1._3 * fact), coords._2 * fact)

          OcNode(c, scaleOctree(fact, up_00), scaleOctree(fact, up_01), scaleOctree(fact, up_10),
            scaleOctree(fact, up_11), scaleOctree(fact, down_00), scaleOctree(fact, down_01), scaleOctree(fact, down_10), scaleOctree(fact, down_11))
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
      case OcLeaf(section) =>
        section.asInstanceOf[Section]._2.map(x => {
          if (x.asInstanceOf[Shape3D].getDrawMode != DrawMode.LINE) {
            val c = func(x.asInstanceOf[Shape3D].getMaterial.asInstanceOf[PhongMaterial].getDiffuseColor)
            val m = new PhongMaterial()
            m.setDiffuseColor(c)
            x.asInstanceOf[Shape3D].setMaterial(m)
            x
          }
          else
            x
        })
        OcLeaf(section)
      case OcNode(coords, up_00, up_01, up_10, up_11, down_00, down_01, down_10, down_11) =>
        OcNode(coords, mapColourEffect(func, up_00), mapColourEffect(func, up_01), mapColourEffect(func, up_10),
          mapColourEffect(func, up_11), mapColourEffect(func, down_00), mapColourEffect(func, down_01), mapColourEffect(func, down_10), mapColourEffect(func, down_11))
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

  def makeTreePartitions(oct: Octree[Placement]): List[Shape3D] = {
    oct match {
      case OcEmpty => Nil
      case OcLeaf(section: Section) =>
        val partition = new Box(section._1._2, section._1._2, section._1._2)
        partition.setTranslateX(section._1._1._1)
        partition.setTranslateY(section._1._1._2)
        partition.setTranslateZ(section._1._1._3)
        partition.setDrawMode(DrawMode.LINE)
        List(partition.asInstanceOf[Shape3D])

      case OcNode(coords, up_00, up_01, up_10, up_11, down_00, down_01, down_10, down_11) =>
        val partition = new Box(coords._2, coords._2, coords._2)
        if ((coords._1._1, coords._1._2, coords._1._3) == (0, 0, 0)) {
          partition.setTranslateX(coords._2 / 2)
          partition.setTranslateY(coords._2 / 2)
          partition.setTranslateZ(coords._2 / 2)
        }
        else {
          partition.setTranslateX(coords._1._1)
          partition.setTranslateY(coords._1._2)
          partition.setTranslateZ(coords._1._3)
        }
        partition.setDrawMode(DrawMode.LINE)
        List(partition.asInstanceOf[Shape3D]) :::
          makeTreePartitions(up_00) :::
          makeTreePartitions(up_01) :::
          makeTreePartitions(up_10) :::
          makeTreePartitions(up_11) :::
          makeTreePartitions(down_00) :::
          makeTreePartitions(down_01) :::
          makeTreePartitions(down_10) :::
          makeTreePartitions(down_11)
    }
  }

  def addPartitionsToWorld(l: List[Shape3D], worldObjects: Group): Unit = {
    l match {
      case Nil => Nil
      case h :: t => {
        worldObjects.getChildren.add(h)
        addPartitionsToWorld(t, worldObjects)
      }
    }
  }

  def saveModel(lst: List[Shape3D]): List[String] = {
    lst match {
      case Nil => Nil
      case h :: t =>
        if (h.getMaterial.asInstanceOf[PhongMaterial] == null) {
          saveModel(t)
        }
        else {
          val color = h.getMaterial.asInstanceOf[PhongMaterial].getDiffuseColor
          val colorRGB = List("(" + (color.getRed * 255).toInt.toString, (color.getGreen * 255).toInt.toString, (color.getBlue * 255).toInt.toString + ")")
          val colorString = colorRGB.mkString(",")
          val shp = h.toString.split("@")
          //println(h.toString)
          val args = List(shp(0), colorString, h.getTranslateX.toInt.toString, h.getTranslateY.toInt.toString, h.getTranslateZ.toInt.toString, h.getScaleX.toInt.toString, h.getScaleY.toInt.toString, h.getScaleZ.toInt.toString)
          val objLine = args.mkString(" ")
          objLine :: saveModel(t)
        }
    }
  }

  def saveGraphicModelsState(l: List[Shape3D]): Unit = {
    val filename = readLine("Type the filename to save the state: \n")
    val tStampRaw = LocalDateTime.now()
    val tStamp = DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm-ss").format(tStampRaw)
    val file = new File("src/" + filename + "_" + tStamp + ".txt")
    try {
      val bw = new BufferedWriter(new FileWriter(file))
      val objLines = saveModel(l).mkString("\n")
      bw.write(objLines)
      bw.close()
    }
    catch {
      case e: FileNotFoundException => println(" file not found")
        println("Do you want to try again? 1 - Yes or 2 - No")
        readLine match {
          case "1" => saveGraphicModelsState(l)
          case "2" => sys.exit()
          case _ =>
            println("Invalid option! Please choose 1 or 2")
            saveGraphicModelsState(l)
        }
    }
  }

  def getTextGroup(l: List[Shape3D], worldObjects: Group = new Group()): Group = {
    l match {
      // case Nil => new Group(camVolume, lineX, lineY, lineZ)
      case Nil => worldObjects
      case h :: t =>
        val x = getTextGroup(t, worldObjects)
        x.getChildren.add(h)
        x
    }
  }
}
