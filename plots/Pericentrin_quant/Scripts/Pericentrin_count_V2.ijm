// Macro to take z stack images containing Affimer and pericentrin channels 
// It will detect pericentrin dots and output measurements of the detected dots

// May need to manually change the channel name for the dataset depending what channel Affimer/pericentrin is
// The size and theshold values may also be modified depending on the dataset  


macro "Pericentrin counter" {
inputFolder=getDirectory("Choose input folder");
outputFolder=getDirectory("Choose output folder for the results");
list=getFileList(inputFolder);

 
for(i=0; i<list.length; i++) {
 path=inputFolder+list[i];
 if(endsWith(path,".tif")) open(path);
 showProgress(i, list.length);
 if(nImages>=1) {
  if(i==0) {
   
  }

outputPath=outputFolder+list[i];
 // The following two lines removes the file extension
  fileExtension=lastIndexOf(outputPath,"."); 
  if(fileExtension!=-1) outputPath=substring(outputPath,0,fileExtension);

roiManager("reset");
run("Split Channels");
// Select the Affimer channel to draw around the cell outline
selectWindow("C2-" + list[i]);
run("In [+]");
run("In [+]");
setSlice(8);
setTool("freehand");
waitForUser("Draw around the cell");
roiManager("Add");
// Set measurements and measure mCh intensity
run("Set Measurements...", "area mean standard min fit integrated median area_fraction redirect=None decimal=3");
run("Measure");
Table.rename("Results", "mCh_measurement");
// Add this outline to the pericentrin channel and run the 3DOC
selectWindow("C1-" + list[i]);
roiManager("select", 0);
run("Clear Outside", "stack");

// Can change the threshold and min value
run("3D Objects Counter", "threshold=1400 slice=8 min.=100 max.=1062500 statistics summary");


// Save the results window from 3DOC
 selectWindow("Results");
 saveAs("Results", outputPath+".csv");
  run("Close"); //closes summary window
// Save the threshold info from 3DOC
 selectWindow("Log");
 saveAs("Text", outputPath+".txt");
  run("Close"); //closes summary window
// Save the mCh intensity
selectWindow("mCh_measurement");
 saveAs("Results", outputPath+".csv");
  run("Close"); //closes summary window
  selectWindow("ROI Manager");
  run("Close");
  run("Close All"); //closes all images




 }}}

