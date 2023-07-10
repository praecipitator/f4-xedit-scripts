{
    Run on several cells. You will be prompted to save a CSV in the end
}
unit userscript;
    uses praUtil;

    var
        cellNavmeshData: TJsonObject;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 0;
        cellNavmeshData := TJsonObject.create;
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
        AddMessage('Added bounds data for '+Name(navmesh));
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        parentCell: IInterface;
    begin
        Result := 0;

        // comment this out if you don't want those messages
        if(Signature(e) <> 'NAVM') then begin
            exit;
        end;

        parentCell := pathLinksTo(e, 'CELL');
        if(GetElementEditValues(parentCell, 'DATA\Is Interior Cell') <> '1') then Exit;

        //AddMessage('Processing: ' + FullPath(e));
        // processing code goes here
        appendNavmeshData(parentCell, e);
    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    var
        i: integer;
        curName, cellName, cellEdid, cellFormID, cellFileName: string;
        csvLine, targetFileName: string;
        csvData: TStringList;
        curCellData: TJsonObject;
        curCell, curCellFile: IInterface;
    begin
        Result := 0;
        csvData := TStringList.create;
        csvData.add('"Cell Editor ID","Cell Name","Cell FormID","Cell Filename","MinX","MinY","MinZ","MaxX","MaxY","MaxZ"');
        
        if(cellNavmeshData.count > 0) then begin

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
                csvData.add(csvLine);
                AddMessage(csvLine);
            end;
            
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
        end else begin
            AddMessage('No relevant navmesh data found, nothing to do');
        end;

        csvData.free();
        cellNavmeshData.free();
    end;

end.