import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Parent, Scene}
import javafx.scene.control.Button
import javafx.stage.{Modality, Stage}

class ControllerFirstWindow {

  @FXML
  var button1: Button = _

  def OnButton1Clicked(): Unit = {
    val fxmlLoader = new FXMLLoader(getClass.getResource("Menu.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    val scene = new Scene(mainViewRoot)

    val secondStage: Stage = new Stage()
    secondStage.setScene(scene)
    secondStage.show()
  }

}
