#pragma TextEncoding = "UTF-8"
#pragma rtGlobals=3				// Use modern global access method and strict wave access
#pragma DefaultTab={3,20,4}		// Set default tab width in Igor Pro 9 and later
#include "PXPUtils"
#include "SuperPlot"

////////////////////////////////////////////////////////////////////////
// Menu items
////////////////////////////////////////////////////////////////////////
Menu "Macros"
	"Spindle Analytics...",  SpindleERCalcWrapper()
	"Collated Spindle Analytics...", FinalWrapper()
	"Start Over", CleanSlate()
	End
End

////////////////////////////////////////////////////////////////////////
// Master functions and wrappers
////////////////////////////////////////////////////////////////////////
Function SpindleERCalcWrapper()
	Variable nFiles = PreLoader()
	if(nFiles == -1)
		return -1
	elseif(nFiles == 0)
		return 0
	endif
	// make a matrix for the results to go into
	String labelStr = "spindleLength;spindlewidth;spindleAR;spindleTheta;spindlePhi;"
	labelStr += "d1;d2;ddiff;spindleOffset;cellLength;"
	labelStr += "spindleCellRatio;cellVol;"
	Make/O/N=(nFiles,ItemsInList(labelStr)) resultMat
	PXPUtils#LabelWaveDimensions(resultMat,1,labelStr)
	WorkflowForSpindleERCalc()
	WorkflowForSummary()
	PXPUtils#MakeTheLayouts("p",5,4, saveit = 0)
End

Function FinalWrapper()
	SetDataFolder root:
	String wList, wName
	wList = WaveList("resultMat_*", ";","")
	// find all resultMats from different experiments (must be preloaded into Igor) and collate
	Concatenate/O/NP=0 wList, resultMat
	// do the same for fileName labels
	wList = WaveList("fileNameWave_*", ";","")
	Concatenate/O/NP=0/T wList, fileNameWave
	Concatenate/O/NP=0/T wList, exptWave
	Variable i, left = 0, right = 0
	// because the * in fileNameWave is the expt name
	for(i = 0; i < ItemsInList(wList); i += 1)
		wName = StringFromList(i, wList)
		Wave/T tw = $wName
		right = left + numpnts(tw) - 1
		exptWave[left,right] = ReplaceString("fileNameWave_",wName,"")
		left = right + 1
	endfor
	WorkflowForAltSummary()
	PXPUtils#MakeTheLayouts("p",5,3, saveit = 0)
End

////////////////////////////////////////////////////////////////////////
// Main functions
////////////////////////////////////////////////////////////////////////
Function PreLoader()
	NewPath/O/Q/M="Please find folder with LimeSeg csv files" expDiskFolder
	
	if (V_flag != 0)
		DoAlert 0, "Disk folder error"
		return -1
	endif
	
	String allCSVList = IndexedFile(expDiskFolder,-1,".csv")
	allCSVList = SortList(allCSVList)
	
	if(ItemsInList(allCSVList) == 0)
		DoAlert 0, "No csv files found"
		return -1
	endif
	
	// get a sublist of "_centrosomes.csv" that we will use to trigger loading
	String subCSVList = ListMatch(allCSVList, "*_centrosomes.csv")
	// now check that we have the other three associated files otherwise the program will crash later
	Variable nFile = ItemsInList(subCSVList)
	
	Variable i
	// ascend list and delete any that don't have all associated files
	for(i = nFile - 1; i > -1; i -= 1)
		if(WhichListItem(ReplaceString("_centrosomes.csv",StringFromList(i,subCSVList),"_DNA.csv"),allCSVList) == -1)
			subCSVList = RemoveListItem(i,subCSVList)
			continue
		elseif(WhichListItem(ReplaceString("_centrosomes.csv",StringFromList(i,subCSVList),"_cellPoints.csv"),allCSVList) == -1)
			subCSVList = RemoveListItem(i,subCSVList)
			continue
		endif
	endfor
	
	Wave/T fileNameWave = ListToTextWave(subCSVList,";")
	MoveWave fileNameWave, root:fileNameWave
	
	return ItemsInList(subCSVList)
End

Function WorkflowForSpindleERCalc()
	Wave/T fileNameWave = root:fileNameWave
	Variable nFiles = numpnts(fileNameWave)
	
	// set up data folder
	NewDataFolder/O/S root:data
	
	Variable i
	
	for(i = 0; i < nFiles; i += 1)
		NewDataFolder/O/S $("cell_" + num2str(i))
		// load data
		if(LoadAndScale(i) != 1)
			Print "Problem with dataset",num2str(i), "Load and Scale"
			SetDataFolder root:data
			continue
		endif
		// process data
		if(SpindleProcessor(i) != 1)
			Print "Problem with dataset",num2str(i), "Spindle Processor"
			SetDataFolder root:data
			continue
		endif
		// build gizmo
		BuildGizmoForDataset(i,1)
		SetDataFolder root:data
	endfor
	
	SetDataFolder root:
End

STATIC Function LoadAndScale(iter)
	Variable iter
	Wave/T fileNameWave = root:fileNameWave
	
	String masterFileName = fileNameWave[iter]
	String exceptionList = "centMat;dnaMat;cellMat;"
	// load data
	LoadWave/A=centMat/J/D/O/K=1/M/P=expDiskFolder/Q masterFileName
	Rename centMat0, centMat
	PXPUtils#KillAllExcept(exceptionList)
	LoadWave/A=dnaMat/J/D/O/K=1/M/P=expDiskFolder/Q ReplaceString("_centrosomes.csv",masterFileName,"_DNA.csv")
	Rename dnaMat0, dnaMat
	WAVE/Z centMat, dnaMat
	// problem here that DNA z-values may be NaN
	if(numtype(dnaMat[0][2]) == 2 || numtype(dnaMat[1][2] == 2))
		dnaMat[][2] = (centMat[0][2] + centMat[1][2]) / 2
	endif
	PXPUtils#KillAllExcept(exceptionList)
	LoadWave/A/W/J/D/O/K=0/P=expDiskFolder/Q ReplaceString("_centrosomes.csv",masterFileName,"_cellPoints.csv")
	WAVE/Z XW,YW,ZW
	if(!WaveExists(XW) || !WaveExists(YW) || !WaveExists(ZW))
		return -1
	endif
	Concatenate/O/KILL {XW,YW,ZW}, cellMat
	PXPUtils#KillAllExcept(exceptionList)
	// at this point we have 4 2D waves in this data folder
	CorrectMatrix(cellMat)
	ScaleMatrices(exceptionList)
	
	return 1
End

STATIC Function CorrectMatrix(w)
	Wave w
	// we need to offset cellMat and ezMat as they are one higher in Z than centMat and dnaMat
	w[][2] -= 1
	WaveStats/Q/RMD=[][2] w
	w[][] = (w[p][2] <= V_min || w[p][2] >= V_max) ? NaN : w[p][q]
	MatrixOp/O $(NameOfWave(w)) = zapnans(w)
	Redimension/N=(numpnts(w)/3,3) w
End

STATIC Function ScaleMatrices(list)
	String list	
	
	Variable i
	
	for(i = 0; i < ItemsInList(list); i += 1)
		Wave w = $StringFromList(i,list)
		// this is specific to JS110 settings - checked on OMERO
//		w[][0,1] *= 0.1107
//		w[][2] *= 0.2
		// Voxel size: 0.1819x0.1819x0.3 micron^3 for JS117 and JS128
		w[][0,1] *= 0.1819
		w[][2] *= 0.3
	endfor
End

Function SpindleProcessor(iter)
	Variable iter
	Wave resultMat = root:resultMat
	
	WAVE/Z cellMat, centMat, dnaMat
	// find bounding ball
	BoundingBall/F cellMat
	// radius is V_Radius and centre is V_CenterX etc.
	Make/O/FREE/N=(1,3) centre = {{V_CenterX},{V_CenterY},{V_CenterZ}}
	
	// for the origin of line we will find the x0 coord
	Make/O/FREE/N=(1,3) originMat
	Differentiate/DIM=0/EP=1/METH=1 centMat /D=centVec
	// when x = 0
	originMat[0][0] = (0 - centMat[0][0]) / centVec[0][0]
	originMat[0][1] = (originMat[0][0] * centVec[0][1]) + centMat[0][1]
	originMat[0][2] = (originMat[0][0] * centVec[0][2]) + centMat[0][2]
	originMat[0][0] = 0
	
	// unit vector
	MatrixOp/O/FREE centMag = sqrt(sumrows(centVec * centVec))
	Duplicate/O/FREE centVec, unitVec
	unitVec /= centMag[0]
	
	// dee = -(uhat dot (origin - centre)) ± sqrt(nabla)
	// nabla = (uhat dot (origin - centre))^2 - (magnitude of origin - centre ^2 - radius^2)
	// uhat dot (origin - centre) is
	MatrixOp/O/FREE o_c = originMat - centre
	MatrixOp/O/FREE aaa = unitVec . o_c
	MatrixOp/O/FREE nabla = (aaa * aaa) - (sumrows(o_c * o_c) - (V_radius * V_radius))
	MatrixOp/O/FREE ddd1 = colrepeat(-aaa + sqrt(nabla),3)
	MatrixOp/O/FREE ddd2 = colrepeat(-aaa - sqrt(nabla),3)
	MatrixOp/O/FREE ppp1 = originMat + (unitVec * ddd1)
	MatrixOp/O/FREE ppp2 = originMat + (unitVec * ddd2)
	Concatenate/O/NP=0 {ppp1,ppp2}, intersectMat
	
	// spindle length
	resultMat[iter][%spindleLength] = centMag[0]
	// spindle width
	resultMat[iter][%spindleWidth] = sqrt((dnaMat[1][0] - dnaMat[0][0])^2 + (dnaMat[1][1] - dnaMat[0][1])^2 + (dnaMat[1][2] - dnaMat[0][2])^2)
	// spindle aspect ratio
	resultMat[iter][%spindleAR] = resultMat[iter][%spindleWidth] / resultMat[iter][%spindleLength]
	// spindle angle (angle in XY plane between plate and spindle axis & angle between spindle axis and z = 0)
	// inclination/polar, theta. azimuth, phi
	Wave sphericalResult = FindSphericalResult(dnaMat,centMat)
	// sphericalResult is in radians. A flat spindle with DNA normal to spindle axis would be pi/2 for both
	// let's show the deviation from this - and convert it to degrees.
	resultMat[iter][%spindleTheta] = abs(pi/2 - abs(sphericalResult[0])) * (180/pi)
	resultMat[iter][%spindlePhi] = (sphericalResult[1] > pi/2) ? abs(sphericalResult[1] - pi/2) * (180/pi) : abs(sphericalResult[1]) * (180/pi)
	// d1 and d2 and difference
	MatrixOp/O/FREE littled1 = sqrt(sumrows((centMat - rowrepeat(ppp1,2)) * (centMat - rowrepeat(ppp1,2))))
	MatrixOp/O/FREE littled2 = sqrt(sumrows((centMat - rowrepeat(ppp2,2)) * (centMat - rowrepeat(ppp2,2))))
	resultMat[iter][%d1] = WaveMin(littled1)
	resultMat[iter][%d2] = WaveMin(littled2)
	resultMat[iter][%ddiff] = abs(resultMat[iter][%d2] - resultMat[iter][%d1])
	// spindle offset (distance between cell centre and spindle centre)
	MatrixOp/O/FREE spCentre = averagecols(centMat)
	MatrixOp/O/FREE off = sqrt(sumrows((centre - spCentre) * (centre - spCentre)))
	resultMat[iter][%spindleOffset] = off[0]
	// cell length
	MatrixOp/O/FREE cellL = sqrt(sumrows((ppp1 - ppp2) * (ppp1 - ppp2)))
	resultMat[iter][%cellLength] = cellL[0]
	// spindle length vs cell length
	resultMat[iter][%spindleCellRatio] = resultMat[iter][%spindleLength] / resultMat[iter][%cellLength]
	// volumes
	Triangulate3D/VOL cellMat
	resultMat[iter][%cellVol] = V_Value
	
	return 1
End

// input is 2 xyz matrix
Function/WAVE FindSphericalResult(refMat,realMat)
	Wave refMat, realMat
	Make/O/N=(2)/FREE resultW
	// offset both matrices to origin
	MatrixOp/O/FREE refMat_off = refMat - rowrepeat(averagecols(refMat),2)
	MatrixOp/O/FREE realMat_off = realMat - rowrepeat(averagecols(realMat),2)
	if(refMat_off[1][1] < 0)
		Reverse/DIM=0/P refMat_off
	endif
	if(realMat_off[1][2] < 0)
		Reverse/DIM=0/P realMat_off
	endif
	// vector and magnitude of real coords
	Differentiate/DIM=0/EP=1/METH=1 refMat_off /D=refVec
	MatrixOp/O/FREE refMag = sqrt(sumrows(refVec * refVec))
	// make vector matrix for x-axis, magnitude is 1
	Make/O/N=(1,3)/FREE xVec = {{1},{0},{0}}
	// find angle for rotation
	MatrixOp/O/FREE dotprod = (xVec . refVec) / (1 * refMag[0])
	Variable theta = aCos(dotprod[0])
	// make rotation matrix
	Make/O/FREE zRotationMatrix={{cos(theta),sin(theta),0},{-sin(theta),cos(theta),0},{0,0,1}}
	// rotate both offset matrices
	MatrixMultiply refMat_off, zRotationMatrix
	WAVE/Z M_product
	Duplicate/O/FREE M_product, refMat_r
	MatrixMultiply realMat_off, zRotationMatrix
	Duplicate/O/FREE M_product, realMat_r
	// now find azimuthal and polar. store in resultW to return to main function
	// should return -90 or 90 for theta (first angle) and deviation from 0,0,1 for Phi
	// this means we can take abs of [0] and find abs diff from 90 for spindle angle
	// if [1] is 0,pi/2 we take angle, if it is pi/2,pi we take angle - 90 (possibly not necessary after adding polarity check for realvec
	resultW[0] = atan2(realMat_r[0][1],realMat_r[0][0])
	resultW[1] = acos(realMat_r[0][2] / sqrt(realMat_r[0][0]^2 + realMat_r[0][1]^2 + realMat_r[0][2]^2))
	// clean-up
	KillWaves/Z M_Product, refVec
	
	return resultW
End

Function BuildGizmoForDataset(iter, opt)
	Variable iter, opt
	String dfName = "cell_" + num2str(iter)
	if(cmpstr(GetDataFolder(0),dfName) != 0)
		if(!DataFolderExists("root:data:"+dfName))
			Print "Do cell of that number"
			return -1
		else
			SetDataFolder $("root:data:"+dfName)
		endif
	endif
	String gizName = "g_" + num2str(iter) + "_cell"
	WAVE/Z centMat,dnaMat,cellMat,ezMat,intersectMat
	
	KillWindow/Z $gizName
	NewGizmo/N=$gizName
	AppendToGizmo/N=$gizName/D Scatter=centMat,name=scatter0
	ModifyGizmo/N=$gizName ModifyObject=scatter0,objectType=scatter,property={ size,0.5},property={ color,1,0,1,1}
	AppendToGizmo/N=$gizName/D path=centMat,name=path0
	ModifyGizmo/N=$gizName ModifyObject=path0,objectType=path,property={ pathColor,1,0,1,0.5}
	AppendToGizmo/N=$gizName/D path=dnaMat,name=path1
	ModifyGizmo/N=$gizName ModifyObject=path1,objectType=path,property={ pathColor,0,0,1,0.5}
	ModifyGizmo/N=$gizName ModifyObject=path1,objectType=path,property={ drawTube,1}
	AppendToGizmo/N=$gizName/D Scatter=intersectMat,name=scatter1
	ModifyGizmo/N=$gizName ModifyObject=scatter1,objectType=scatter,property={ size,0.5}
	ModifyGizmo/N=$gizName ModifyObject=scatter1,objectType=scatter,property={ color,0.5,0.5,0.5,1}
	AppendToGizmo/N=$gizName/D Scatter=cellMat,name=scatter2
	ModifyGizmo/N=$gizName ModifyObject=scatter2,objectType=scatter,property={ size,0.1},property={ color,0,0,0,0.25}
	if(opt == 1)
		String pictName = gizName + ".png"
		SavePICT/O/P=ExpDiskFolder/E=-5/B=300 as pictName
		KillWindow/Z $gizName
	else
		DoWindow/F $gizName
	endif
End

Function WorkflowForSummary()
	String sList = "-C1_;-E4_;-E7_;-E8_;" // this is the list of substrings to classify the groups
	String str
	SetDataFolder root:
	
	Make/O/N=(4)/T condNameWave = {"mCh","E4","E7","E8"}
	WAVE/Z resultMat
	WAVE/Z/T fileNameWave
	Variable nFiles = numpnts(fileNameWave)
	Make/O/N=(nFiles) condWave
	
	Variable i,j
	
	for(i = 0; i < nFiles; i += 1)
		for(j = 0; j < ItemsInList(sList); j += 1)
			str = StringFromList(j, sList)
			if(stringmatch(fileNameWave[i],"*"+str+"*") == 1)
				condWave[i] = j
			endif
		endfor
	endfor
	String parameters = PXPUtils#GetDimLabelsFromWave(resultMat,1)
	Variable nParameters = ItemsInList(parameters)
	String kvPairList = "spindleLength:Spindle Length (µm);spindlewidth:Spindle Width (µm);spindleAR:Spindle Aspect Ratio;spindleTheta:Spindle Angle (°);spindlePhi:Spindle Tilt (°);"
	kvPairList += "d1:d1 (µm);d2:d2 (µm);ddiff:d2-d1 (µm);spindleOffset:Spindle Offset (µm);cellLength:Cell Length (µm);"
	kvPairList += "spindleCellRatio:Spindle Cell Length Ratio;cellVol:Cell Volume (µm\\S3\\M);"
	String plotName, wName
	
	for(i = 0; i < nParameters; i += 1)
		plotName = "p_" + StringFromList(i,parameters)
		KillWindow/Z $plotName
		Display/HIDE=1/N=$plotName
		
		for(j = 0; j < ItemsInList(sList); j += 1)
			wName = StringFromList(i,parameters) + "_" + num2str(j)
			Duplicate/O/RMD=[][i] resultMat, $wName
			Wave w = $wName
			Redimension/N=-1 w
			// remove the other condition
			w[] = (condWave[p] == j) ? w[p] : NaN
			// remove any cells with errors (cell vol or ez vol are 0)
			w[] = (resultMat[p][%cellVol] == 0) ? NaN : w[p]
			WaveTransform zapnans w
			if(j == 0)
				AppendBoxPlot/W=$plotName w vs condNameWave
			else
				AddWavesToBoxPlot/W=$plotName w
			endif
		endfor
		Label/W=$plotName left StringByKey(StringFromList(i,parameters),kvPairList)
		SetAxis/A/E=1/N=1/W=$plotName left
		ModifyBoxPlot/W=$plotName trace=$(StringFromList(i,parameters) + "_0"),whiskerMethod=4,markers={19,-1,19},markerSizes={2,2,2}
		ModifyGraph/W=$plotName rgb=(65535,0,0,32768)
	endfor
End

Function WorkflowForAltSummary()
	String sList = "-C1_;-E4_;-E7_;-E8_;" // this is the list of substrings to classify the groups
	String str
	SetDataFolder root:
	
	Make/O/N=(4)/T condNameWave = {"mCh","E4","E7","E8"}
	WAVE/Z resultMat
	WAVE/Z/T fileNameWave, exptWave
	Variable nFiles = numpnts(fileNameWave)
	Make/O/N=(nFiles)/T condWave
	
	Variable i,j
	
	for(i = 0; i < nFiles; i += 1)
		for(j = 0; j < ItemsInList(sList); j += 1)
			str = StringFromList(j, sList)
			if(stringmatch(fileNameWave[i],"*"+str+"*") == 1)
				condWave[i] = condNameWave[j]
			endif
		endfor
	endfor
	
	String parameters = PXPUtils#GetDimLabelsFromWave(resultMat,1)
	Variable nParameters = ItemsInList(parameters)
	String kvPairList = "spindleLength:Spindle Length (µm);spindlewidth:Spindle Width (µm);spindleAR:Spindle Aspect Ratio;spindleTheta:Spindle Angle (°);spindlePhi:Spindle Tilt (°);"
	kvPairList += "d1:d1 (µm);d2:d2 (µm);ddiff:d2-d1 (µm);spindleOffset:Spindle Offset (µm);cellLength:Cell Length (µm);"
	kvPairList += "spindleCellRatio:Spindle Cell Length Ratio;cellVol:Cell Volume (µm\\S3\\M);"
	String plotName, wName
	
	for(i = 0; i < nParameters; i += 1)
		// make a 1d wave of each parameter
		wName = StringFromList(i,parameters)
		Duplicate/O/RMD=[][i] resultMat, $wName
		Wave w = $wName
		Redimension/N=-1 w
		
		// superplot params
		// rep, cond, meas
		// width, auto, bars, stats = 0.4, 0, 1, 1
		SuperplotHeadless(exptWave, condWave, w, 0.4, 0, 1, 1)
		Label left StringByKey(StringFromList(i,parameters),kvPairList)
		SetAxis/A/E=1/N=1 left
	endfor
End

Function PrintDatasets(list)
	String list	

	Variable nn = ItemsInList(list)
	WAVE/Z/T fileNameWave = root:fileNameWave

	Variable i

	for(i = 0; i < nn; i += 1)
		Print fileNameWave[str2num(StringFromList(i,list))]
	endfor
End