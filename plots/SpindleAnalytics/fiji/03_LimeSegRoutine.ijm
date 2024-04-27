/*
 * Requires a folder of *_EZmask.tif files each with ONE corresponding roiset
 * 1) _cellrois.zip
 */

macro "Process All EZ Masks" {
	dir1 = getDirectory("Original Image Directory ");
	list1 = getFileList(dir1);
	run("ROI Manager...");

	setBatchMode(true);
	// Identify _EZmask.tif files
	for (i = 0; i < list1.length; i ++) {		
 		if (endsWith(list1[i], "_EZmask.tif")) {
 			tifname = list1[i];
 			pathname = dir1 + tifname;
 			// check we have both roisets
 			if (File.exists(replace(pathname, "_EZmask.tif", "_cellrois.zip"))) {
 				processImageLimeSeg(pathname);
 			} else {
 				print("Missing roiset",tifname);
 			}
 		}
 	}
 	setBatchMode(false);
}

function processImageLimeSeg(path) {
	// load image
	open(path);
	title = getTitle();
	Stack.getDimensions(width, height, channels, slices, frames);
	slices = nSlices;
	// convert to 8-bit
	run("8-bit");
	// load cell ROIs
	run("ROI Manager...");
	roiManager("reset");
	open(replace(path, "_EZmask.tif", "_cellrois.zip"));
	// use mid to make mask for the top and bottom
	newImage("TopBottom", "8-bit black", width, height, 1);
	selectWindow("TopBottom");
	roiManager("Select", 1);
	setForegroundColor(255, 255, 255);
	run("Fill", "slice");
	// concatenate to make new stack
	run("Concatenate...", "  title=NewOne image1=TopBottom image2="+title+" image3=TopBottom"); // keep?
	// specify top
	selectWindow("NewOne");
	roiManager("Select", 1);
	getSelectionCoordinates(xCoords, yCoords);
	Array.getStatistics(xCoords, min, max, mean, stdDev);
	xx = floor(mean);
	Array.getStatistics(yCoords, min, max, mean, stdDev);
	yy = floor(mean);
	selectWindow("NewOne");
	Stack.setSlice(1);
	makePoint(xx, yy);
	Roi.setPosition(1);
	roiManager("add");
	run("Select None");
	// shift ROIs up in z by 1
	for (i = 0; i < 3; i++) {
		roiManager("Select", 0);
		run("Next Slice [>]");
		Stack.getPosition(channel, slice, frame);
		Roi.setPosition(slice);
		roiManager("add");
		roiManager("Select", 0);
		roiManager("delete"); // remove 1st ROI from list
		run("Select None");
	}
	// add bottom point
	getDimensions(width, height, channels, slices, frames);
	slices = nSlices;
	Stack.setSlice(slices);
	makePoint(xx, yy);
	Roi.setPosition(slices);
	roiManager("add");
	run("Select None");
	// run lime seg SKELETON
	run("Clear all");
	run("Skeleton Seg", "d_0=8.0 f_pressure=0.0 z_scale=3.08 range_in_d0_units=2.0 color=255,0,0 show3d=false numberofintegrationstep=-1 realxypixelsize=0.065");
	// save overlay (do we delete the top and bottom first (what happens to overlay)
	run("List Elements");
	saveAs("Results", replace(path, "_EZmask.tif", "_cellPoints.csv"));
	run("Close");
	
	run("Close All");
}