// Macro to measure spindle recruitment of tubulin and two other MAPs 
// A 31 px square ROI is used, dimensions will change depending on the objective used to take the images
// State what channel tubulin is and what channel the other protein is 
// Will need to have Click Square ROI on the toolbar of ImageJ

// This is adapted for the Fixed GTSE1 KSW expt. GTSE1 is protein 1 and the other complex member is protein 2.

#@ File (label = "Input directory:", style = "directory") input
#@Integer(label="Tubulin Channel:",value=3) tubulin_channel
#@Integer(label="Protein1 Channel:",value=1) protein_1_channel
#@Integer(label="Protein2 Channel:",value=2) protein_2_channel

run("Clear Results");


processFolder(input);



function processFolder(input) {
	list = getFileList(input);
	list = Array.sort(list);
	for (i = 0; i < list.length; i++) {
		if(File.isDirectory(input + File.separator + list[i]))
			processFolder(input + File.separator + list[i]);
		if(endsWith(list[i], "tif")){
			fname = list[i];
			fname = replace(list[i], ".tif", "");
			if (File.exists(input+"/"+fname+".csv")){
				File.delete(input+"/"+fname+".csv");
			}
			f = File.open(input+"/"+fname+".csv");
			print(f, "measurement,line,channel,Mean,StdDev,Min,Max,IntDen,RawIntDen");
			processFile(input, list[i]);
			File.close(f);
		}
	}
}

function processFile(input, file) {
	// Do the processing here by adding your own code.
	// Leave the print statements until things work, then remove them.

	

	//Draw 3 square ROIs, take measurement on red and green channel, adjust for background

	//Move the 3 square ROIs, take measurement on red and green channel, adjust for background

	open(input+"/"+file);
	run("In [+]");
	run("In [+]");

	bckgrnd = calculateBackground();
	//print(bckgrnd[0], bckgrnd[1]);

	
	first = initLines(f, bckgrnd);

	
	second = moveLines(f, bckgrnd);



	
	run("Close All");

	
	
	
	
}



function calculateBackground(){
	makeRectangle(64, 97, 8, 8);
	Stack.setChannel(protein_1_channel);
	run("Set Measurements...", "area mean standard min fit integrated redirect=None decimal=3");
	waitForUser("Select a background region:");
	Stack.setChannel(tubulin_channel);
	run("Measure");
	results = newArray(3);
	results[0] = getResult("Mean", 0);
	Stack.setChannel(protein_1_channel);
	run("Measure");
	results[1] = getResult("Mean", 1);
	Stack.setChannel(protein_2_channel);
	run("Measure");
	results[2] = getResult("Mean", 2);
	run("Clear Results");
	return results;
}



function initLines(f, background){
	makeRectangle(64, 97, 8, 8);
	run("Set Measurements...", "area mean standard min bounding integrated redirect=None decimal=3");
	Stack.setChannel(tubulin_channel);
	waitForUser("spindle box 1:");
	roiManager("Add");
	waitForUser("spindle box 2:");
	roiManager("Add");
	waitForUser("spindle box 3:");
	roiManager("Add");
	roiManager("multi-measure append");
	results = newArray(9);
	for (i=0;i<3;i++){
		
		print(f, "microtubule,"+(i+1)+","+tubulin_channel+","+getResult("Mean", i)-background[0]+","+getResult("StdDev", i)+","+(getResult("Min", i)-background[0])+","+(getResult("Max", i)-background[0])+","+getResult("IntDen", i)+","+getResult("RawIntDen", i));
	}
	
	Stack.setChannel(protein_1_channel);
	roiManager("multi-measure append");
	for (i=3;i<6;i++){
		print(f, "microtubule,"+(i-2)+","+protein_1_channel+","+getResult("Mean", i)-background[1]+","+getResult("StdDev", i)+","+(getResult("Min", i)-background[1])+","+(getResult("Max", i)-background[1])+","+getResult("IntDen", i)+","+getResult("RawIntDen", i));
		
	}
	Stack.setChannel(protein_2_channel);
	roiManager("multi-measure append");
	for (i=6;i<9;i++){
		print(f, "microtubule,"+(i-5)+","+protein_2_channel+","+getResult("Mean", i)-background[2]+","+getResult("StdDev", i)+","+(getResult("Min", i)-background[2])+","+(getResult("Max", i)-background[2])+","+getResult("IntDen", i)+","+getResult("RawIntDen", i));
		
	}
	run("Clear Results");
	
	return results;
}


function moveLines(f, background){
	results = newArray(6);
	Stack.setChannel(protein_1_channel);
	total = roiManager("count");
	for (i=0;i<total;i++){
		
		roiManager("Select", i);
		count = i+1;
		Stack.setChannel(protein_1_channel);
		waitForUser("cyto box "+count+" :");
		roiManager("Add");
	}
	for (i=0;i<total;i++){
		roiManager("Select", 0);
		roiManager("Delete");
	}
	roiManager("multi-measure append");
	for (i=0;i<3;i++){
		print(f, "cytoplasm,"+(i+1)+","+tubulin_channel+","+getResult("Mean", i)-background[0]+","+getResult("StdDev", i)+","+(getResult("Min", i)-background[0])+","+(getResult("Max", i)-background[0])+","+getResult("IntDen", i)+","+getResult("RawIntDen", i));
	}
	Stack.setChannel(protein_1_channel);
	roiManager("multi-measure append");
	for (i=3;i<6;i++){
		print(f, "cytoplasm,"+(i-2)+","+protein_1_channel+","+getResult("Mean", i)-background[1]+","+getResult("StdDev", i)+","+(getResult("Min", i)-background[1])+","+(getResult("Max", i)-background[1])+","+getResult("IntDen", i)+","+getResult("RawIntDen", i));
	}
	Stack.setChannel(protein_2_channel);
	roiManager("multi-measure append");
	for (i=6;i<9;i++){
		print(f, "cytoplasm,"+(i-5)+","+protein_2_channel+","+getResult("Mean", i)-background[2]+","+getResult("StdDev", i)+","+(getResult("Min", i)-background[2])+","+(getResult("Max", i)-background[2])+","+getResult("IntDen", i)+","+getResult("RawIntDen", i));
	}
	run("Clear Results");
	roiManager("reset");
	return results;
}


