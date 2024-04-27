// Macro to load in montages of z projection images containing pericentrin and centrin stain
// Use cell counter to count how many centrin foci are present in each pericentrin site. 
// Save result of cell counter. Number of columns = number of pericentrin foci, number in the column = number of centrin foci
  
macro "Pericentrin_centrin counter" {
inputFolder=getDirectory("Choose input folder");
outputFolder=getDirectory("Choose output folder for the results");
run("Cell Counter");
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


waitForUser("Initialise, remove counters to match the number of pericentrin foci, select centrin foci in each pericentrin site, click result");

// Save the results window
 selectWindow("Results");
 saveAs("Results", outputPath+".csv");
  run("Close"); //closes results window
  run("Close All"); //closes all images




 }}}

