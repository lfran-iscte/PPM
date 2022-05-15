
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Parent, Scene}
import javafx.scene.control.Button
import javafx.scene.layout.Pane
import javafx.stage.{Modality, Stage}

class Menu {
  @FXML
  var insertShapeButton: Button = _
  var insertShapePanel:Pane=_

  def OnInsertShapeButtonClicked(): Unit = {
    println("Clicked")
  }
}
