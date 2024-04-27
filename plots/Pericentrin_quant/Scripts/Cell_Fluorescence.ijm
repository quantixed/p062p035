// Macro to take z stack images 
// Measure fluorescence intensity of cell using middle slice
// Save the output

macro "Measure Cell Fluorescence" {
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
// Set measurements and measure GFP intensity
run("Set Measurements...", "area mean standard min fit integrated median area_fraction redirect=None decimal=3");
run("Measure");
Table.rename("Results", "fluorescence_measurement");


// Save the GFP intensity
selectWindow("fluorescence_measurement");
 saveAs("Results", outputPath+"_fluorescence_intensity.csv");
  run("Close"); //closes summary window
  run("Close All"); //closes all images

 }}}

