macro "labkit_segmentation" {
	inputFolder = getDirectory("Choose input folder");
	// save outputs in the same place as files
	outputFolder = inputFolder;
	list = getFileList(inputFolder);
	
	setBatchMode(true);
	
	for(i = 0; i < list.length; i ++) {
		path = inputFolder + list[i];
		if(checkPath(inputFolder, list[i], outputFolder) == 1)	{
			open(path);
					
			outputPath = outputFolder+list[i];
	 		// The following two lines remove the file extension
			fileExtension = lastIndexOf(outputPath,"."); 
			if(fileExtension != -1) outputPath = substring(outputPath,0,fileExtension);
			getDimensions(ww, hh, cc, ss, ff);
			title = getTitle();
			
			run("Duplicate...", "duplicate channels=2");
			er = getTitle();
			close(title);
			selectWindow(er);
			run("Enhance Contrast", "saturated=0.35");
			run("Apply LUT", "stack");
			run("Segment Image With Labkit", "segmenter_file=[" + "affimer.classifier" + "] use_gpu=false");
			close(er);
			save(outputPath + "_EZmask.tif");
			run("Close All");
		}
	}
	setBatchMode(false);
}


// this function checks if the path points to a tif and if it has already been analysed

function checkPath(dir, name, outdir) {
	path = dir + name;
	if(endsWith(path,".tif")) {
		outputPath = outdir + name;
		// The following two lines remove the file extension
		fileExtension = lastIndexOf(outputPath,"."); 
		if(fileExtension != -1)
			outputPath = substring(outputPath,0,fileExtension);
		// form potential file path to check for
		possibleFilePath = outputPath + "_centrosomes.csv";
		list = getFileList(outdir);
		for(i = 0; i < list.length; i ++) {
			if(outdir + list[i] == possibleFilePath) {
				return 0; // it has a companion csv file - skip
			}
		}
		return 1; // it doesn't have a companion csv file
	}
	else {
		return -1; // it wasn't a tif file
	}
}

