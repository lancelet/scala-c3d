package c3d.viewer;

import c3d.viewer.scene.control.FrameNumberAxis;
import javafx.application.Application;
import javafx.geometry.Insets;
import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;

public class TimelineTest extends Application {
    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) {
        primaryStage.setTitle("Timeline Test");
        BorderPane root = new BorderPane();
        root.setPadding(new Insets(50));
        FrameNumberAxis fna = new FrameNumberAxis();
        root.setStyle("-fx-background-color: black;");
        root.setBottom(fna);
        primaryStage.setScene(new Scene(root, 800, 200));
        primaryStage.show();
    }
}