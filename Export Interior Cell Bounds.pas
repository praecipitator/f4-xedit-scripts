{
    Run on several cells. You will be prompted to save a CSV in the end
}
unit ExportInteriorCellBounds;
    uses praUtil;

    const
        configFile = ScriptsPath + 'Export Interior Cell Bounds.cfg';

    var
        cellNavmeshData: TJsonObject;

        isVerboseMode: boolean;
        checkIntersections: boolean;
        checkZIntersection: boolean;
        saveCsvWhenDone: boolean;

    procedure loadConfig();
    var
        i, j, breakPos: integer;
        curLine, curKey, curVal: string;
        lines : TStringList;
    begin
        // default
        isVerboseMode := false;
        checkIntersections := true;
        checkZIntersection := true;
        saveCsvWhenDone := true;


        if(not FileExists(configFile)) then begin
            exit;
        end;
        lines := TStringList.create;
        lines.LoadFromFile(configFile);

        for i:=0 to lines.count-1 do begin
            curLine := lines[i];
            breakPos := -1;

            for j:=1 to length(curLine) do begin
                if(curLine[j] = '=') then begin
                    breakPos := j;
                    break;
                end;
            end;

            if breakPos <> -1 then begin
                curKey := trim(copy(curLine, 0, breakPos-1));
                curVal := trim(copy(curLine, breakPos+1, length(curLine)));

                if(curKey = 'isVerboseMode') then begin
                    isVerboseMode := StrToBool(curVal);
                end else if(curKey = 'checkIntersections') then begin
                    checkIntersections := StrToBool(curVal);
                end else if(curKey = 'checkZIntersection') then begin
                    checkZIntersection := StrToBool(curVal);
                end else if(curKey = 'saveCsvWhenDone') then begin
                    saveCsvWhenDone := StrToBool(curVal);
                end;
            end;
        end;

        lines.free();
    end;

    procedure saveConfig();
    var
        lines : TStringList;
    begin
        lines := TStringList.create;

        lines.add('isVerboseMode='+BoolToStr(isVerboseMode));
        lines.add('checkIntersections='+BoolToStr(checkIntersections));
        lines.add('checkZIntersection='+BoolToStr(checkZIntersection));
        lines.add('saveCsvWhenDone='+BoolToStr(saveCsvWhenDone));

        lines.saveToFile(configFile);
        lines.free();
    end;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 0;
        cellNavmeshData := TJsonObject.create;

        loadConfig();
        if(not showGui()) then begin
            Result := 1;
            exit;
        end;
    end;

    function showGui(): boolean;
    var
        frm: TForm;
        grp: TGroupBox;
        yOffset: integer;
        inputX, inputY, inputZ: TEdit;

        btnOk, btnCancel: TButton;
        resultCode: cardinal;

        targetFileBox: TComboBox;
        newFileName: string;

        cbCheckIntersect, cbDoZIntersect, cbSaveCSV, cbVerboseMode: TCheckBox;
    begin
        Result := false;
        frm := CreateDialog('Export Interior Cell Bounds', 500, 350);

        yOffset := 10;
        cbCheckIntersect := CreateCheckbox(frm, 10, yOffset, 'Build list of intersections');
        cbCheckIntersect.checked := checkIntersections;
        CreateLabel(frm, 28, yOffset+20, 'If checked, all cells in the current run will be checked for intersections.');
        CreateLabel(frm, 28, yOffset+36, 'The list will be appended as Column 11 in the CSV.');

        yOffset := yOffset + 60;
        cbDoZIntersect := CreateCheckbox(frm, 10, yOffset, 'Check intersections in 3D');
        cbDoZIntersect.checked := checkZIntersection;
        CreateLabel(frm, 28, yOffset+20, 'If checked, the intersection check will be performed in X/Y/Z, not just in X/Y.');
        CreateLabel(frm, 28, yOffset+36, 'Does nothing if "Build list of intersections" is disabled.');

        yOffset := yOffset + 60;
        cbSaveCSV := CreateCheckbox(frm, 10, yOffset, 'Save result as CSV at the end');
        CreateLabel(frm, 28, yOffset+20, 'If checked, you will be asked to save a CSV at the end. Otherwise, it''s just the xEdit output.');
        cbSaveCSV.checked := saveCsvWhenDone;

        yOffset := yOffset + 60;
        cbVerboseMode := CreateCheckbox(frm, 10, yOffset, 'Verbose Mode');
        cbVerboseMode.checked := isVerboseMode;
        CreateLabel(frm, 28, yOffset+20, 'If checked, more xEdit output will be generated');

        yOffset := yOffset + 60;

        btnOk := CreateButton(frm, 80, yOffset, '    OK    ');
        btnCancel := CreateButton(frm, 280, yOffset, '  Cancel  ');

        btnCancel.ModalResult := mrCancel;
        btnOk.ModalResult := mrOk;

        resultCode := frm.showModal();
        if (resultCode <> mrOk) then begin
            frm.free();
            exit;
        end;

        checkIntersections  := cbCheckIntersect.checked;
        checkZIntersection  := cbDoZIntersect.checked;
        saveCsvWhenDone     := cbSaveCSV.checked;
        isVerboseMode       := cbVerboseMode.checked;

        saveConfig();

        frm.free();
        Result := true;
    end;


    procedure appendRefData(cell, ref: IInterface);
    var
        minX, minY, minZ, maxX, maxY, maxZ, curX, curY, curZ: float;
        cellString: string;
        isFirstData: boolean;
        curCellData: TJsonObject;
    begin
        cellString := FormToAbsStr(cell);

        isFirstData := false;

        if(cellNavmeshData.Types[cellString] = JSON_TYPE_NONE) then begin
            isFirstData := true
        end;

        curX := GetElementNativeValues(ref, 'DATA\Position\X');
        curY := GetElementNativeValues(ref, 'DATA\Position\Y');
        curZ := GetElementNativeValues(ref, 'DATA\Position\Z');

        curCellData := cellNavmeshData.O[cellString];

        if(isFirstData) then begin
            minX := curX;
            minY := curY;
            minZ := curZ;

            maxX := curX;
            maxY := curY;
            maxZ := curZ;
            isFirstData := false;
        end else begin
            minX := curCellData.F['minX'];
            minY := curCellData.F['minY'];
            minZ := curCellData.F['minZ'];

            maxX := curCellData.F['maxX'];
            maxY := curCellData.F['maxY'];
            maxZ := curCellData.F['maxZ'];

            if(curX < minX) then minX := curX;
            if(curY < minY) then minY := curY;
            if(curZ < minZ) then minZ := curZ;

            if(curX > maxX) then maxX := curX;
            if(curY > maxY) then maxY := curY;
            if(curZ > maxZ) then maxZ := curZ;
        end;

        curCellData.F['minX'] := minX;
        curCellData.F['minY'] := minY;
        curCellData.F['minZ'] := minZ;

        curCellData.F['maxX'] := maxX;
        curCellData.F['maxY'] := maxY;
        curCellData.F['maxZ'] := maxZ;
        if(isVerboseMode) then begin
            AddMessage('Added bounds data for '+Name(ref));
        end;
    end;

    procedure appendNavmeshData(cell, navmesh: IInterface);
    var
        minX, minY, minZ, maxX, maxY, maxZ, curX, curY, curZ: float;
        cellString: string;
        curCellData: TJsonObject;
        vertices, curV: IInterface;
        i, numVertices: integer;
        isFirstData: boolean;
    begin
        cellString := FormToAbsStr(cell);

        isFirstData := false;

        if(cellNavmeshData.Types[cellString] = JSON_TYPE_NONE) then begin
            isFirstData := true
        end;

        curCellData := cellNavmeshData.O[cellString];
        minX := curCellData.F['minX'];
        minY := curCellData.F['minY'];
        minZ := curCellData.F['minZ'];

        maxX := curCellData.F['maxX'];
        maxY := curCellData.F['maxY'];
        maxZ := curCellData.F['maxZ'];

        // nvmn := ElementByPath(navm, 'NVNM - Navmesh Geometry');
        vertices := ElementByPath(navmesh, 'NVNM - Navmesh Geometry\Vertices');
        if(not assigned(vertices)) then begin
            exit;
        end;

        numVertices := ElementCount(vertices);
        for i := 0 to numVertices - 1 do begin
            curV := ElementByIndex(vertices, i);

            curX := GetElementNativeValues(curV, 'X');
            curY := GetElementNativeValues(curV, 'Y');
            curZ := GetElementNativeValues(curV, 'Z');

            if(isFirstData) then begin
                minX := curX;
                minY := curY;
                minZ := curZ;

                maxX := curX;
                maxY := curY;
                maxZ := curZ;
                isFirstData := false;
            end else begin
                if(curX < minX) then minX := curX;
                if(curY < minY) then minY := curY;
                if(curZ < minZ) then minZ := curZ;

                if(curX > maxX) then maxX := curX;
                if(curY > maxY) then maxY := curY;
                if(curZ > maxZ) then maxZ := curZ;
            end;
        end;

        curCellData.F['minX'] := minX;
        curCellData.F['minY'] := minY;
        curCellData.F['minZ'] := minZ;

        curCellData.F['maxX'] := maxX;
        curCellData.F['maxY'] := maxY;
        curCellData.F['maxZ'] := maxZ;
        if(isVerboseMode) then begin
            AddMessage('Added bounds data for '+Name(navmesh));
        end;
    end;

    function isSingleCoordIntersecting(min1, max1, min2, max2: float): boolean;
    begin
        //isOverlapping1D(box1,box2) = xmax1 >= xmin2 and xmax2 >= xmin1
        Result := ((max1 >= min2) and (max2 >= min1));
{
        Result := false;
        // for each coordinate, either min or max of data1 must be between min or max of data2, or vice versa
        if (min2 <= min1 and min1 <= max2) or (min2 <= max1 and max1 <= max2) then begin
            Result := true;
            exit;
        end;

        if (min1 <= min2 and min2 <= max1) or (min1 <= max2 and max2 <= max1) then begin
            Result := true;
            exit;
        end;
        }
    end;

    function isIntersecting(data1, data2: TJsonObject; checkZ: boolean): boolean;
    begin
        Result := false;
        if(not checkZ) then begin
            Result := (
                isSingleCoordIntersecting(data1.F['minX'], data1.F['maxX'], data2.F['minX'], data2.F['maxX']) and
                isSingleCoordIntersecting(data1.F['minY'], data1.F['maxY'], data2.F['minX'], data2.F['maxX'])
            );
        end else begin
            Result := (
                isSingleCoordIntersecting(data1.F['minX'], data1.F['maxX'], data2.F['minX'], data2.F['maxX']) and
                isSingleCoordIntersecting(data1.F['minY'], data1.F['maxY'], data2.F['minX'], data2.F['maxX']) and
                isSingleCoordIntersecting(data1.F['minZ'], data1.F['maxZ'], data2.F['minZ'], data2.F['maxZ'])
            );
        end;
    end;

    procedure performIntersectionCheck();
    var
        i, j: integer;
        curName, curNameCheck: string;
        curCellData, curCellDataCheck: TJsonObject;
        cell1, cell2: IInterface;
    begin
        for i:=0 to cellNavmeshData.count-1 do begin
            curName := cellNavmeshData.Names[i];
            curCellData := cellNavmeshData.O[curName];
            cell1 := AbsStrToForm(curName);

            for j:=i+1 to cellNavmeshData.count-1 do begin
                curNameCheck := cellNavmeshData.Names[j];
                cell2 := AbsStrToForm(curNameCheck);
                curCellDataCheck := cellNavmeshData.O[curNameCheck];
                if (isIntersecting(curCellData, curCellDataCheck, checkZIntersection)) then begin
                    if(isVerboseMode) then begin
                        AddMessage('Intersection: '+Name(cell1)+'<-->'+Name(cell2));
                    end;
                    curCellData.A['intersections'].add(curNameCheck);
                    curCellDataCheck.A['intersections'].add(curName);
                end;
            end;
        end;


    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        parentCell: IInterface;
        sig: string;
    begin
        Result := 0;

        sig := Signature(e);
        if(sig = 'NAVM') then begin
            parentCell := pathLinksTo(e, 'CELL');
            if(GetElementEditValues(parentCell, 'DATA\Is Interior Cell') <> '1') then Exit;

            appendNavmeshData(parentCell, e);
        end else if (isReferenceSignature(sig)) then begin
            parentCell := pathLinksTo(e, 'CELL');
            if(GetElementEditValues(parentCell, 'DATA\Is Interior Cell') <> '1') then Exit;

            appendRefData(parentCell, e);
        end else begin
            exit;
        end;


    end;

    function buildIntersectionString(cellData: TJsonObject): string;
    var
        i: integer;
        curStr: string;
        curCell: IInterface;
    begin
        Result := '';
        for i:=0 to cellData.A['intersections'].count-1 do begin
            curStr := cellData.A['intersections'].S[i];
            curCell := AbsStrToForm(curStr);
            if(Result <> '') then begin
                Result := Result + ' ';
            end;
            Result := Result + EditorID(curCell);
        end;
    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    var
        i: integer;
        curName, cellName, cellEdid, cellFormID, cellFileName: string;
        csvHeader, csvLine, targetFileName: string;
        csvData: TStringList;
        curCellData: TJsonObject;
        curCell, curCellFile: IInterface;
    begin
        Result := 0;
        csvData := TStringList.create;

        csvHeader := '"Cell Editor ID","Cell Name","Cell FormID","Cell Filename","MinX","MinY","MinZ","MaxX","MaxY","MaxZ"';

        if(checkIntersections) then begin
            if(checkZIntersection) then begin
                csvHeader := csvHeader+',"Intersections X/Y/Z"';
            end else begin
                csvHeader := csvHeader+',"Intersections X/Y"';
            end;
        end;

        csvData.add(csvHeader);

        if(cellNavmeshData.count > 0) then begin
            if(checkIntersections) then begin
                performIntersectionCheck();
            end;

            for i:=0 to cellNavmeshData.count-1 do begin
                curName := cellNavmeshData.Names[i];
                curCellData := cellNavmeshData.O[curName];

                curCell := AbsStrToForm(curName);
                cellName := DisplayName(curCell);
                cellEdid := EditorID(curCell);
                curCellFile := GetFile(curCell);
                cellFormID := '0x'+IntToHex(getElementLocalFormId(curCell), 8);
                cellFileName := GetFileName(curCellFile);

                csvLine := cellEdid+',"'+cellName+'","'+cellFormID+'","'+cellFileName+'",'+
                    floatToStr(curCellData.F['minX'])+','+
                    floatToStr(curCellData.F['minY'])+','+
                    floatToStr(curCellData.F['minZ'])+','+
                    floatToStr(curCellData.F['maxX'])+','+
                    floatToStr(curCellData.F['maxY'])+','+
                    floatToStr(curCellData.F['maxZ']);

                if(checkIntersections) then begin
                    csvLine := csvLine + ',"' + buildIntersectionString(curCellData)+'"';
                end;
                csvData.add(csvLine);
                AddMessage(csvLine);
            end;
            if(saveCsvWhenDone) then begin
                targetFileName := ShowSaveFileDialog('Save CSV as', 'CSV files|*.csv');
                if(targetFileName <> '') then begin
                    if(not strEndsWithCI(targetFileName, '.csv')) then begin
                        targetFileName := targetFileName + '.csv';
                    end;
                    csvData.saveToFile(targetFileName);
                    AddMessage('Saved to '+targetFileName);
                end else begin
                    AddMessage('Cancelled');
                end;
            end;
        end else begin
            AddMessage('No relevant navmesh data found, nothing to do');
        end;

        csvData.free();
        cellNavmeshData.free();
    end;

end.