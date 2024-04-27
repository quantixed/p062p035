/*
 * Original code by James Shelford, modified by Laura Downie and Stephen Royle
 * Make measurements of spindle parameters in Fiji for analysis in R
 * Requires a folder of TIFFs each with a corresponding segmented ER TIFF
 * which was output from LabKit (32-bit, [0,1])
 */

macro "Spindle analytics" {
	inputFolder = getDirectory("Choose input folder");
	// save outputs in the same place as files
	outputFolder = inputFolder;
	list = getFileList(inputFolder);
	run("Set Measurements...", "area mean min integrated stack redirect=None decimal=3");
	
	for(i = 0; i < list.length; i ++) {
		path = inputFolder + list[i];
		if(checkPath(inputFolder, list[i], outputFolder) == 1)	{
			if(endsWith(path, "_EZmask.tif")) continue;
			open(path);
					
			outputPath = outputFolder+list[i];
	 		// The following two lines remove the file extension
			fileExtension = lastIndexOf(outputPath,"."); 
			if(fileExtension != -1) outputPath = substring(outputPath,0,fileExtension);
			// info about the image
			getDimensions(ww, hh, cc, ss, ff);
			title = getTitle();
		
			// Select the centrosomes
			run("ROI Manager...");
			roiManager("reset");
			setTool("multipoint");
			selectWindow(title);
			run("Select None");
			
			Stack.setChannel(1); // set to 4th channel
			Stack.setSlice(floor(ss/2)); // set to midpoint of stack
			waitForUser("Select the two centrosomes\nThen click OK");
			roiManager("Add");
			roiManager("Select", 0);
			// get array of x and y coords
			getSelectionCoordinates(xCoords, yCoords);
			f = File.open(outputPath + "_centrosomes.csv");
			// to get the z position we need this little hack
			run("Clear Results");
			run("Measure");
			zPos = newArray(nResults());
			for (i = 0; i < nResults(); i++) {
				zPos[i] = getResult('Slice', i);
				print(f, xCoords[i] + ',' + yCoords[i] + ',' + zPos[i] + '\n');
			}
			File.close(f);
			close("Results");
			// get rid of ROI
			roiManager("reset");
			run("Select None");
			
			// Draw a line through the DNA 
			setTool("line");
			setLineWidth(1);
			Stack.setChannel(4); // set to last channel
			Stack.setSlice(floor((zPos[1] + zPos[0])/2)); // set to midpoint of centrosomes
			waitForUser("Draw a line through the DNA\nThen click OK");
			roiManager("Add");
			roiManager("Select", 0);
			// get array of x and y coords
			getSelectionCoordinates(xCoords, yCoords);
			f = File.open(outputPath + "_DNA.csv");
			// to get the z position we need this little hack
			run("Clear Results");
			run("Measure");
			zPos = newArray(xCoords.length);
			for (i = 0; i < xCoords.length; i++) {
				zPos[i] = getResult('Slice', 0);
				print(f, xCoords[i] + ',' + yCoords[i] + ',' + zPos[i] + '\n');
			}
			File.close(f);
			close("Results");
			// get rid of ROI
			roiManager("reset");
			run("Select None");

			// now we will switch to the output from LabKit (segmented 3D of ER channel) - generated separately
			selectWindow(title);
			// outputPath currently has the full path of the original tif without extension so:
			open(outputPath + "_EZmask.tif");
			title = getTitle();
			getDimensions(ww, hh, cc, ss, ff);

			// CELL OUTLINE
			run("ROI Manager...");
			roiManager("reset");
			setTool(2); // polygon
			// user needs to draw 3 approximate outlines CW from 12 O'Clock around cell (top middle bottom)
			Stack.setSlice(floor(ss*0.2)); // set to "top" of stack
			waitForUser("Cell Outline\nUPPER\nDraw rough outline clockwise starting from 12 O'Clock\nThen click OK");
			roiManager("Add");
			run("Select None");
			
			Stack.setSlice(floor(ss*0.5)); // set to "mid" of stack
			waitForUser("Cell Outline\nMIDDLE\nDraw rough outline clockwise starting from 12 O'Clock\nThen click OK");
			roiManager("Add");
			run("Select None");
			
			Stack.setSlice(floor(ss*0.8)); // set to "bottom" of stack
			waitForUser("Cell Outline\nLOWER\nDraw rough outline clockwise starting from 12 O'Clock\nThen click OK");
			roiManager("Add");
			run("Select None");
			// save cell outline rois
			roiManager("save", outputPath + "_cellrois.zip");
			
			// the ROIset will be processed in a separate script
			
			run("Close All");
		}
	}
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