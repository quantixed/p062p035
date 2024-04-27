#pragma TextEncoding = "UTF-8"
#pragma rtGlobals=3		// Use modern global access method and strict wave access.

// Users record data in an Excel spreadsheet
// User records the frame numbers for onset of three phases: NEB, metaphase, anaphase
// in 3 columns per sheet (each sheet is a different condition - name is important)
// Data should start at A1

// Intervals of <= 0 min are removed (assumed to be human error)
// Cumulative histograms show the full timescale of the data
// Note that this means that failure to divide needs attention

////////////////////////////////////////////////////////////////////////
// Menu items
////////////////////////////////////////////////////////////////////////
Menu "Macros"
	"Mitotic Timing...", /Q, MitoticTiming()
	"Mitotic Timing on Directory", /Q, UseDefaultSettings()
End


////////////////////////////////////////////////////////////////////////
// Master functions and wrappers
////////////////////////////////////////////////////////////////////////
Function MitoticTiming()
	CleanSlate()
	SpecifySettings()
End

Function PostLoadProcess()
	WAVE/Z settingsMT
	CalculateTimings(settingsMT[0])
	MakeHistograms()
End

Function UseDefaultSettings()
	CleanSlate()
	
	NewPath/O/Q DirOfWorkbooks
	PathInfo DirOfWorkbooks
	String pathToDiskFolder = S_Path
	String fileList = IndexedFile(DirOfWorkbooks,-1,".xlsx")
	Variable nFiles = ItemsInList(fileList)
	
	String xlsName, pdfName
	Variable i
	
	for(i = 0; i < nFiles; i += 1)	
		Make/O/N=(3) settingsMT = {3,1,0}
		xlsName = StringFromList(i, fileList)
		if(stringmatch(xlsName, "*.xls") == 1)
			pdfName = ReplaceString(".xls", xlsName, ".pdf") // should not use this since we specified xlsx above
		else
			pdfName = ReplaceString(".xlsx", xlsName, ".pdf")
		endif
		// run code
		if(LoadDataFromExcel(pathToDiskFolder + xlsName) == 0)
			CalculateTimings(settingsMT[0])
			MakeHistograms()
			DoWindow/F summaryLayout
			SavePICT/E=-2/W=(0,0,0,0)/P=DirOfWorkbooks/O as pdfName
			CleanSlate()
		else
			return -1
		endif
	endfor
End

////////////////////////////////////////////////////////////////////////
// Main functions
////////////////////////////////////////////////////////////////////////
Function LoadDataFromExcel(filePath)
	String filePath
	
	// each experimental condition needs to be a separate sheet
	String sheet,wList,wName
	Variable i,j
	
	XLLoadWave/J=1 filePath
	if(V_Flag)
		DoAlert 0, "The user pressed Cancel"
	endif
	Variable nSheets = ItemsInList(S_value)
	NewPath/O/Q path1, S_path
	String colList = "NEB;Metaphase;Anaphase;"
	String nameList
	Make/O/T/N=(nSheets) condWave
	Variable nExp,nCell,Length
	String newName
	
	for(i = 0; i < nSheets; i += 1)
		sheet = StringFromList(i,S_Value)
		condWave[i] = sheet
		nameList = PrefixStringList(sheet,colList)
		XLLoadWave/S=sheet/D/W=1/NAME=nameList/O/K=0/P=path1 S_fileName
		// now check lengths. Any NaNs at end of wave will be truncated by XLLoadWave. Concatenate will fail
		wList = WaveList(sheet + "_*",";","")
		nExp = ItemsInList(wList)
		nCell = 0
		for(j = 0; j < nExp; j += 1)
			wName = StringFromList(j,wList)
			Wave w0 = $wName
			nCell = max(numpnts(w0),nCell) // find max length
		endfor
		// correct the lengths
		for(j = 0; j < nExp; j += 1)
			wName = StringFromList(j,wList)
			Wave w0 = $wName
			length = numpnts(w0)
			if (length < nCell)
				InsertPoints/V=(NaN) length, (nCell - length), w0 // add NaNs if necessary
			endif
		endfor	
		// make matrix
		Concatenate/O/NP=1/KILL nameList, $(sheet + "_frames")
	endfor
	// Print "***\r The sheet", sheet, "was loaded from", S_path,"\r  ***"
	return 0
End

///	@param	prefix	string for inserting before each item in list
///	@param	strList	the string list to be modified.
Function/S PrefixStringList(prefix,strList)
	String prefix,strList
	String newString = ""
	
	Variable i
	
	for(i = 0; i < ItemsInList(strList); i += 1)
		newString += prefix + "_" + StringFromList(i,strList) + ";"
	endfor
	
	return newString
End

///	@param	rate	variable, minutes per frame
Function CalculateTimings(rate)
	Variable rate
	WAVE/Z/T condWave
	
	Variable nCond = numpnts(condWave)
	Variable nCell
	String wName, newName
	Variable i
	
	for(i = 0; i < nCond; i += 1)
		wName = condWave[i] + "_frames"
		Wave w0 = $wName
		nCell = DimSize(w0,0)
		// NEB-to-metaphase
		newName = ReplaceString("_frames",wName,"_NM")
		Make/O/N=(nCell) $newName
		Wave w1 = $newName
		w1[] = (w0[p][1] - w0[p][0]) * rate
		w1[] = (w1[p] <= 0) ? NaN : w1[p] // remove any negative or 0 values
		// metaphase-to-anaphase
		newName = ReplaceString("_frames",wName,"_MA")
		Make/O/N=(nCell) $newName
		Wave w1 = $newName
		w1[] = (w0[p][2] - w0[p][1]) * rate
		w1[] = (w1[p] <= 0) ? NaN : w1[p]
		// NEB-to-anaphase
		newName = ReplaceString("_frames",wName,"_NA")
		Make/O/N=(nCell) $newName
		Wave w1 = $newName
		w1[] = (w0[p][2] - w0[p][0]) * rate
		w1[] = (w1[p] <= 0) ? NaN : w1[p]
		// Make 1D wave of NEB onset (used for plotting durations vs onset)
		newName = ReplaceString("_frames",wName,"_onset")
		Make/O/N=(nCell) $newName
		Wave w1 = $newName
		w1[] = w0[p][0] * rate
	endfor
End

Function MakeHistograms()
	WAVE/Z/T condWave
	WAVE/Z settingsMT
	Variable rate = settingsMT[0]
	
	Variable nCond = numpnts(condWave)
	MakeColorWave(nCond,"colorWave",alpha = 32639)
	WAVE/Z colorWave
	
	String plotList = "NM;MA;NA;"
	String labelList = "NEB-Meta;Meta-Ana;NEB-Ana;"
	String plotName, wName, histName
	Variable nPlots = ItemsInList(plotList)
	DoWindow/K summaryLayout
	NewLayout/N=summaryLayout
	LayoutPageAction/W=summaryLayout size(-1)=(595, 842), margins(-1)=(18, 18, 18, 18)
	
	Variable maxLength = 0
	String legendStr = "", labelStr = ""
	Variable normVar
	
	Variable i,j
	
	for(i = 0; i < nPlots; i += 1)
		plotname = "p_" + StringFromList(i,plotList)
		KillWindow/Z $plotName
		Display/HIDE=1/N=$plotName
		maxLength = 0
		legendStr = ""
		
		for(j = 0; j < nCond; j += 1)
			wName = condWave[j] + "_" + StringFromList(i,plotList)
			Wave w0 = $wName
			maxLength = max(WaveMax(w0),maxLength)
		endfor
		
		for(j = 0; j < nCond; j += 1)
			wName = condWave[j] + "_" + StringFromList(i,plotList)
			Wave w0 = $wName
			histName = wName + "_hist"
			Make/O/N=(ceil((maxLength) / rate) + 2) $histName
			Histogram/P/CUM/B={0,3,ceil((maxLength) / rate) + 2} w0,$histName
			Wave w1 = $histName
			if(settingsMT[1] == 1)
				ScaleHisto(w0,w1)
			endif
			AppendToGraph/W=$plotName $histName
			ModifyGraph/W=$plotName rgb($histName)=(colorWave[j][0],colorWave[j][1],colorWave[j][2],colorWave[j][3])
			if(j > 0)
				legendStr += "\r"
			endif
			legendStr += "\\s(" + histName + ") " + condWave[j]
			// add to labelStr
			labelStr += "\r" + NameOfWave(w0) + " progression: " + num2str(round(w1[numpnts(w1) - 1] * numpnts(w0))) + " for " + num2str(numpnts(w0)) + " cells."
			FindLevel/Q w1, (w1[numpnts(w1) - 1] * 0.5)
			labelStr += "T-half: " + num2str(V_LevelX)
		endfor
		FormatHisto(plotName, legendStr, StringFromList(i,labelList))
		AppendLayoutObject/W=summaryLayout graph $plotName
	endfor
	
	// make a plot of both
	plotName = "p_NMNA"
	KillWindow/Z $plotName
	Display/HIDE=1/N=$plotName
	for(i = 0; i < nCond; i += 1)
		wName = condWave[i] + "_NM"
		histName = wName + "_hist"
		AppendToGraph/W=$plotName $histName
		ModifyGraph/W=$plotName rgb($histName)=(colorWave[i][0],colorWave[i][1],colorWave[i][2],colorWave[i][3])
		wName = condWave[i] + "_NA"
		histName = wName + "_hist"
		AppendToGraph/W=$plotName $histName
		ModifyGraph/W=$plotName rgb($histName)=(colorWave[i][0],colorWave[i][1],colorWave[i][2],colorWave[i][3])
	endfor
	// lengendStr should still be correct
	FormatHisto(plotName, legendStr, "")
	AppendLayoutObject/W=summaryLayout graph $plotName
	Variable mostCells = 0
	Variable mostTime = 0
	
	// stick diagrams
	for(i = 0; i < nCond; i += 1)
		plotname = "s_" + condWave[i]
		KillWindow/Z $plotName
		Display/HIDE=1/N=$plotName
		MakeSticks(condWave[i])
		wName = condWave[i] + "_sticks"
		Wave w0 = $wName
		AppendToGraph/W=$plotName w0[][1] vs w0[][0]
		ModifyGraph/W=$plotName rgb($wName)=(colorWave[i][0],colorWave[i][1],colorWave[i][2],colorWave[i][3])
		mostCells = max(mostCells,numpnts(w0) / 8) // sticks wave has 8 points per cell
		WaveStats/Q/RMD=[][0] w0
		mostTime = max(mostTime, max(V_max,-V_min))
	endfor
	// axes and formatting
	for(i = 0; i < nCond; i += 1)
		plotname = "s_" + condWave[i]
		SetAxis/W=$plotName left -1, mostCells
		SetAxis/W=$plotName bottom -mostTime, mostTime
		ModifyGraph/W=$plotName mode=0,lsize=1
		ModifyGraph/W=$plotName noLabel(left)=2,axThick(left)=0,standoff=0
		ModifyGraph/W=$plotName margin(left)=21,margin(right)=21
		Label/W=$plotName bottom "Duration (min)"
		TextBox/W=$plotName/C/N=text0/F=0/S=3/A=RT/X=0.00/Y=0.00 condWave[i]
		SetDrawEnv/W=$plotName xcoord= bottom,ycoord= left,linefgc= (30583,30583,30583),dash= 3
		DrawLine/W=$plotName 0,-1,0,mostCells
		AppendLayoutObject/W=summaryLayout graph $plotName
	endfor
	
	// now add duration vs onset
	for(i = 0; i < nPlots; i += 1)
		plotname = "q_" + StringFromList(i,plotList)
		KillWindow/Z $plotName
		Display/HIDE=1/N=$plotName
		
		for(j = 0; j < nCond; j += 1)
			wName = condWave[j] + "_" + StringFromList(i,plotList)
			AppendToGraph/W=$plotName $wName vs $(condWave[j] + "_onset")
			ModifyGraph/W=$plotName rgb($wName)=(colorWave[j][0],colorWave[j][1],colorWave[j][2],colorWave[j][3])
		endfor
		ModifyGraph/W=$plotName mode=3,marker=19,mrkThick=0
		SetAxis/W=$plotName/A/N=1/E=1 left
		SetAxis/W=$plotName/A/N=1/E=1 bottom
		Label/W=$plotName left "Duration (min)"
		Label/W=$plotName bottom "Onset (min)"
		TextBox/W=$plotName/C/N=text0/F=0/S=3/A=RT/X=0.00/Y=0.00 StringFromList(i,labelList)
		AppendLayoutObject/W=summaryLayout graph $plotName
	endfor
	
	
	// format layout
	ModifyLayout/W=summaryLayout units=0
	ModifyLayout/W=summaryLayout frame=0,trans=1
	Execute /Q "Tile/A=(6,3)"
	
	TextBox/W=summaryLayout/C/N=text0/F=0/A=LB/X=0.00/Y=0.00 labelStr
End

STATIC Function ScaleHisto(originalW,histW)
	Wave originalW, histW
	Duplicate/FREE originalW, tempW
	WaveTransform zapnans tempW
	Variable ratio = numpnts(tempW) / numpnts(originalW)
	
	histW *= ratio
End

STATIC Function FormatHisto(plotName, legendStr, labelStr)
	String plotName, legendStr, labelStr
	
	SetAxis/A/N=1/W=$plotName left
	SetAxis/A/N=1/W=$plotName bottom
	Label/W=$plotName bottom "Time (min)"
	Label/W=$plotName left "Cumulative frequency"
	TextBox/W=$plotName/C/N=text0/F=0/S=3/A=RB/X=0.00/Y=0.00 legendStr
	TextBox/W=$plotName/C/N=text1/F=0/S=3/A=LT/X=0.00/Y=0.00 labelStr
	ModifyGraph/W=$plotName lsize=2
End

STATIC Function MakeSticks(cond)
	String cond
	WAVE/Z settingsMT
	
	Duplicate/O $(cond + "_frames"), $(cond + "_times")
	Wave timeW = $(cond + "_times")
	timeW[][] *= settingsMT[0]
	// now we have a 2D wave with 3 columns of *times*, offset to metaphase
	Duplicate/O/FREE/RMD=[][1] timeW, justTimeW
	timeW[][] = timeW[p][q] - justTimeW[p][0]
	// if we unset failOpt, we'll remove any rows with a NaN
	if(settingsMT[1] == 0)
		timeW[][] = (numtype(timeW[p][0] + timeW[p][1] + timeW[p][2]) == 2) ? NaN : timeW[p][q] 
#if igorversion()>=9
	MatrixOp/O timeW = zapnans(timeW)
#elif igorversion()==8 // allow for compilation on Igor Pro 8
	Redimension/N=(numpnts(timeW)) timeW
	WaveTransform zapnans timeW
#endif
		Redimension/N=(numpnts(timeW) / 3,3) timeW
	endif
	if(settingsMT[2] == 0) // order by NM
		Duplicate/O/RMD=[][0]/FREE timeW, sortW	
	elseif(settingsMT[2] == 1) // order by MA
		Duplicate/O/RMD=[][2]/FREE timeW, sortW	
	elseif(settingsMT[2] == 2) // order by NA
		Duplicate/O/RMD=[][2]/FREE timeW, sortW	
		sortW[] = timeW[p][2] - timeW[p][0]
	endif
	// ensure positive values
	sortW[] = sqrt(sortW[p] * sortW[p])
	// complicated sorting procedure
	Make/O/N=(numpnts(sortW))/FREE indexW = p
	Sort/R sortW, sortW,indexW
	Make/O/N=(numpnts(sortW))/FREE newIndexW = p
	Sort indexW, indexW,newIndexW
	Make/O/N=(DimSize(timeW,0),DimSize(timeW,1)) $(cond + "_timesY")
	Wave yW = $(cond + "_timesY")
	yW[][] = newIndexW[p]
	// add blanks and redimension in order to make the stick graph
	Make/O/N=(DimSize(timeW,0))/FREE blank=NaN
	Concatenate/O/NP=1 {timeW,blank}, timeW
	Concatenate/O/NP=1 {yW,blank}, yW
	MatrixTranspose timeW
	MatrixTranspose yW
	Redimension/N=(numpnts(timeW)) timeW
	Redimension/N=(numpnts(yW)) yW
	Concatenate/O/NP=1/KILL {timeW,yW}, $(cond + "_sticks")
End

////////////////////////////////////////////////////////////////////////
// Panel functions
///////////////////////////////////////////////////////////////////////
Function SpecifySettings()

	// make global waves to store settings
	Make/O/N=(1)/T pathWave
	Make/O/N=(3) settingsMT
	DoWindow/K MTPanel
	NewPanel/N=MTPanel/K=1/W=(40,40,400,200)
	DrawText/W=MTPanel 10,30,"Mitotic timing data (Excel file)"
	DrawText/W=MTPanel 10,150,"MitoticTiming"
	// do it button
	Button DoIt,pos={240,120},size={100,20},proc=DoItButtonProc,title="Do It"
	// file button
	Button fileButton,pos={10,40},size={38,20},proc=ButtonProc,title="File"
	// file or dir box
	SetVariable fileBox,pos={52,43},size={280,14},value= pathWave[0], title=" "
	// specify timing
	Variable tStep = 3
	SetVariable box0,pos={180,70},size={140,14},title="Frame interval (min)",format="%g",value=_NUM:tStep
	// option for show failure to divide
	Variable failOpt = 1
	CheckBox box1,pos={180,90},size={20,20},title="Show failure to divide?",value=failOpt,mode=0
	// sorting option for stick graph
	String list = "NEB-Meta;Meta-Ana;NEB-Ana;"
	String popupStr = "\""+list+"\""
	// make the popup
	DrawText/W=MTPanel 10,84,"Sort stick graph by:"
	PopUpMenu box2,pos={10,90},size={90,14}, bodywidth=90,value= #popupStr, mode = 1
End

// define buttons
Function ButtonProc(ctrlName) : ButtonControl
	String ctrlName

	Wave/T/Z pathWave
	Variable refnum
	String stringForTextWave

	// get File Path
	Open/D/R/F="*.xls*"/M="Select Excel Workbook" refNum
	stringForTextWave = S_filename

	if (strlen(stringForTextWave) == 0) // user pressed cancel
		return -1
	endif
	
	pathWave[0] = stringForTextWave
End

Function DoItButtonProc(ctrlName) : ButtonControl
	String ctrlName
	WAVE/Z settingsMT
	WAVE/T/Z pathWave

	strswitch(ctrlName)
		case "DoIt" :
			ControlInfo/W=MTPanel box0
			settingsMT[0] = V_Value			
			ControlInfo/W=MTPanel box1
			settingsMT[1] = V_Value
			ControlInfo/W=MTPanel box2
			settingsMT[2] = V_Value - 1 // 1-based
			if(strlen(pathWave[0]) == 0)
				break
			endif
			if(LoadDataFromExcel(pathWave[0]) == 0)
				KillWindow/Z MTPanel
				PostLoadProcess()
				return 0
			else
				return -1
			endif
	endswitch
End



////////////////////////////////////////////////////////////////////////
// Utility functions
////////////////////////////////////////////////////////////////////////
// Colours are taken from Paul Tol SRON stylesheet
// Colours updated. Brighter palette for up to 6 colours, then palette of 12 for > 6
// Define colours
StrConstant SRON_1 = "0x4477aa;"
StrConstant SRON_2 = "0x4477aa;0xee6677;"
StrConstant SRON_3 = "0x4477aa;0xccbb44;0xee6677;"
StrConstant SRON_4 = "0x4477aa;0x228833;0xccbb44;0xee6677;"
StrConstant SRON_5 = "0x4477aa;0x66ccee;0x228833;0xccbb44;0xee6677;"
StrConstant SRON_6 = "0x4477aa;0x66ccee;0x228833;0xccbb44;0xee6677;0xaa3377;"
StrConstant SRON_7 = "0x332288;0x88ccee;0x44aa99;0x117733;0xddcc77;0xcc6677;0xaa4499;"
StrConstant SRON_8 = "0x332288;0x88ccee;0x44aa99;0x117733;0x999933;0xddcc77;0xcc6677;0xaa4499;"
StrConstant SRON_9 = "0x332288;0x88ccee;0x44aa99;0x117733;0x999933;0xddcc77;0xcc6677;0x882255;0xaa4499;"
StrConstant SRON_10 = "0x332288;0x88ccee;0x44aa99;0x117733;0x999933;0xddcc77;0x661100;0xcc6677;0x882255;0xaa4499;"
StrConstant SRON_11 = "0x332288;0x6699cc;0x88ccee;0x44aa99;0x117733;0x999933;0xddcc77;0x661100;0xcc6677;0x882255;0xaa4499;"
StrConstant SRON_12 = "0x332288;0x6699cc;0x88ccee;0x44aa99;0x117733;0x999933;0xddcc77;0x661100;0xcc6677;0xaa4466;0x882255;0xaa4499;"

/// @param hex		variable in hexadecimal
Function hexcolor_red(hex)
	Variable hex
	return byte_value(hex, 2) * 2^8
End

/// @param hex		variable in hexadecimal
Function hexcolor_green(hex)
	Variable hex
	return byte_value(hex, 1) * 2^8
End

/// @param hex		variable in hexadecimal
Function hexcolor_blue(hex)
	Variable hex
	return byte_value(hex, 0) * 2^8
End

/// @param data	variable in hexadecimal
/// @param byte	variable to determine R, G or B value
STATIC Function byte_value(data, byte)
	Variable data
	Variable byte
	return (data & (0xFF * (2^(8*byte)))) / (2^(8*byte))
End

/// @param	cond	variable for number of conditions
Function MakeColorWave(nRow, wName, [alpha])
	Variable nRow
	String wName
	Variable alpha
	
	// Pick colours from SRON palettes
	String pal
	if(nRow == 1)
		pal = SRON_1
	elseif(nRow == 2)
		pal = SRON_2
	elseif(nRow == 3)
		pal = SRON_3
	elseif(nRow == 4)
		pal = SRON_4
	elseif(nRow == 5)
		pal = SRON_5
	elseif(nRow == 6)
		pal = SRON_6
	elseif(nRow == 7)
		pal = SRON_7
	elseif(nRow == 8)
		pal = SRON_8
	elseif(nRow == 9)
		pal = SRON_9
	elseif(nRow == 10)
		pal = SRON_10
	elseif(nRow == 11)
		pal = SRON_11
	else
		pal = SRON_12
	endif
	
	Variable color
	String colorWaveFullName = "root:" + wName
	if(ParamisDefault(alpha) == 1)
		Make/O/N=(nRow,3) $colorWaveFullName
		WAVE w = $colorWaveFullName
	else
		Make/O/N=(nRow,4) $colorWaveFullName
		WAVE w = $colorWaveFullName
		w[][3] = alpha
	endif
	
	Variable i
	
	for(i = 0; i < nRow; i += 1)
		// specify colours
		color = str2num(StringFromList(mod(i, 12),pal))
		w[i][0] = hexcolor_red(color)
		w[i][1] = hexcolor_green(color)
		w[i][2] = hexcolor_blue(color)
	endfor
End

STATIC Function CleanSlate()
	String fullList = WinList("*", ";","WIN:7")
	Variable allItems = ItemsInList(fullList)
	String name
	Variable i
 
	for(i = 0; i < allItems; i += 1)
		name = StringFromList(i, fullList)
		KillWindow/Z $name		
	endfor
	
	// Kill waves in root
	KillWaves/A/Z
	// Look for data folders and kill them
	DFREF dfr = GetDataFolderDFR()
	allItems = CountObjectsDFR(dfr, 4)
	for(i = 0; i < allItems; i += 1)
		name = GetIndexedObjNameDFR(dfr, 4, i)
		KillDataFolder $name		
	endfor
End