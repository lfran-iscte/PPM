import javafx.fxml.FXML
import javafx.geometry.Insets
import javafx.scene.control._
import javafx.scene.SubScene
import javafx.scene.layout.GridPane
import Util._
import javafx.scene.shape.Shape3D

import scala.io.Source



class Controller {

  @FXML
  private var menuPane:GridPane = _

  @FXML
  private var graphPane:GridPane = _

  @FXML
  private var subScene1:SubScene = _

  @FXML
  private var graphComboBox:ComboBox[String] = _

  @FXML
  private var filesComboBox:ComboBox[String] = _

  private var doubleSizeButton:Button = _
  private var halfSizeButton:Button = _

  private var doubleSizeLabel:Label = _
  private var halfSizeLabel:Label = _

  private var effectLabel:Label = _
  private var applyEffectButton:Button = _
  private var effectComboBox:ComboBox[String] = _

  var currentOct:Octree[Placement] = OcEmpty
  var currentPartitions:List[Shape3D] = _


  //method automatically invoked after the @FXML fields have been injected
  @FXML
  def initialize(): Unit = {

    InitSubScene.subScene.widthProperty.bind(subScene1.widthProperty)
    InitSubScene.subScene.heightProperty.bind(subScene1.heightProperty)
    populateFileList()
    initializeScale()
    initializeColourEffect()
    subScene1.setRoot(InitSubScene.root)

    InitSubScene.root.setOnMouseClicked((event) => {
      InitSubScene.camVolume.setTranslateX(InitSubScene.camVolume.getTranslateX + 2)
      //InitSubScene.worldRoot.getChildren.removeAll()
      // metodo T3
      changePartitionColor(currentPartitions,InitSubScene.camVolume)
    })

    graphComboBox.getItems().addAll(
      "Scale",
      "Apply Effects");
  }

  def populateFileList() : Unit = {

    filesComboBox.getItems().clear()
    getFilesNameFromDirectory.foreach { filesComboBox.getItems().add(_)}
  }

  def initializeScale() : Unit = {
    doubleSizeButton = new Button
    halfSizeButton = new Button

    doubleSizeLabel = new Label
    halfSizeLabel = new Label

    doubleSizeLabel.setText("Increase :")
    halfSizeLabel.setText("Decrease :")
    doubleSizeButton.setText("x2")
    halfSizeButton.setText("x0.5")

    doubleSizeButton.setOnMouseClicked((event) => {
      currentOct = scaleOctree(2,currentOct)
      resetPartitions()

    })
    halfSizeButton.setOnMouseClicked((event) => {
      currentOct = scaleOctree(0.5,currentOct)
      resetPartitions()

    })
  }

  def initializeColourEffect() : Unit ={

    effectLabel = new Label("Select Colour Effect:")
    effectComboBox = new ComboBox[String]()
    applyEffectButton = new Button("Apply")

    applyEffectButton.setOnMouseClicked(event =>  {
      if (effectComboBox.getValue.equals("Remove Green"))
      {
        currentOct = mapColourEffect(sepia, currentOct)
      }

      if (effectComboBox.getValue.equals("Sepia"))
      {
        currentOct = mapColourEffect(removeGreenComponent, currentOct)
      }

    })
    effectComboBox.getItems().addAll(
      "Remove Green",
      "Sepia");

  }

  def resetPartitions() : Unit = {
    removeObjects(currentPartitions,InitSubScene.worldRoot)
    currentPartitions = makeTreePartitions(currentOct)
    addPartitionsToWorld(currentPartitions,InitSubScene.worldRoot)
    changePartitionColor(currentPartitions,InitSubScene.camVolume)
  }

  def onLoadFileButtonPressed() : Unit = {
    print("Load File pressed")
    val l1 = Source.fromFile("src/"+filesComboBox.getValue).getLines.toList
    val graphics = getGraphicModels(l1)


    removeObjects(currentOct, InitSubScene.worldRoot)
    removeObjects(currentPartitions, InitSubScene.worldRoot)

    currentOct = insertTrees(graphics)
    currentPartitions = makeTreePartitions(currentOct)
    addToWorld(currentOct, InitSubScene.worldRoot)
    addPartitionsToWorld(currentPartitions,InitSubScene.worldRoot)
    changePartitionColor(currentPartitions,InitSubScene.camVolume)
  }

  def onGraphComboBoxAction() : Unit = {

    print(graphComboBox.getValue)

    if(graphComboBox.getValue.equals("Scale"))
      {
        graphPane.getChildren.clear()
        graphPane.add(halfSizeLabel,0,0)
        graphPane.add(doubleSizeLabel,0,2)
        graphPane.add(halfSizeButton,1,0)
        graphPane.add(doubleSizeButton,1,2)
      }

    if(graphComboBox.getValue.equals("Apply Effects"))
    {
      graphPane.getChildren.clear()
      graphPane.add(effectLabel,0,1)
      graphPane.add(effectComboBox,1,1)
      graphPane.add(applyEffectButton,1,3)

    }

  }

}
