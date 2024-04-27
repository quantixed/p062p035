macro "Max Z projection of stack" {
	dir1 = getDirectory("Choose Source Directory ");
	dir2 = getDirectory("Choose Destination Directory ");
	list = getFileList(dir1);

	// Make an array of tif files only
	tiflist = newArray(0);
	for (i=0; i<list.length; i++) {
		if (endsWith(list[i], ".tif")) {
			tiflist = append(tiflist, list[i]);
		}
	}

	setBatchMode(true);
	for (i=0; i<tiflist.length; i++) {
	    showProgress(i+1, tiflist.length);
	     s = "open=["+dir1+tiflist[i]+"] autoscale color_mode=Grayscale rois_import=[ROI manager] view=Hyperstack stack_order=XYCZT";
	    run("Bio-Formats Importer", s);
run("Grouped Z Project...", "projection=[Max Intensity]");
	    // comment this next part for usual output
	   // Stack.getDimensions(width, height, channels, slices, frames);
	    //if (channels>1) {
	    	//for (j=1; j<=channels; j++){
	    	//Stack.setChannel(j);
	    	//run("Invert LUT");
	    	
	    	//
	    saveAs("tiff", dir2+replace(tiflist[i],".tif",".tif"));
		close();
	}
	setBatchMode(false);
}

function append(arr, value) {
	arr2 = newArray(arr.length+1);
	for (i=0; i<arr.length; i++)
		arr2[i] = arr[i];
		arr2[arr.length] = value;
	return arr2;
}