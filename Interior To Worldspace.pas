{
    Run on a cell
}
unit InteriorToWorldspace;
    uses praUtil;

    var
        targetCell, targetFile: IInterface;
        offsetX, offsetY, offsetZ: float;

        // stats
        numNavmeshes, numRefs: integer;

        // new bounds
        minX, minY, minZ, maxX, maxY, maxZ: float;
        isFirstRef: boolean;

        targetWsName, targetWsEdid: string;

        cellCache: TStringList;

    function getElementToEdit(e: IInterface): IInterface;
    var
        elemFile: IInterface;
    begin
        elemFile := GetFile(e);
        if(isSameFile(elemFile, targetFile)) then begin
            Result := e;
            exit;
        end;

        // addRequiredMastersSilent(e, targetFile);
        Result := createElementOverride(e, targetFile);
    end;

    function processNavm(navm: IInterface): IInterface;
    var
        navmToEdit, vertices, curV: IInterface;
        numVertices, i: integer;
        newX, newY, newZ: float;

    begin
        // hard
        navmToEdit := getElementToEdit(navm);

        vertices := ElementByPath(navmToEdit, 'NVNM - Navmesh Geometry\Vertices');
        if(not assigned(vertices)) then begin
            exit;
        end;

        numNavmeshes := numNavmeshes + 1;

        numVertices := ElementCount(vertices);
        for i := 0 to numVertices - 1 do begin
            curV := ElementByIndex(vertices, i);

            newX := GetElementNativeValues(curV, 'X')+offsetX;
            newY := GetElementNativeValues(curV, 'Y')+offsetY;
            newZ := GetElementNativeValues(curV, 'Z')+offsetZ;
{
            if(isFirstRef) then begin
                minX := newX;
                minY := newY;
                minZ := newZ;

                maxX := newX;
                maxY := newY;
                maxZ := newZ;
                isFirstRef := false;
            end else begin
                if(newX < minX) then minX := newX;
                if(newY < minY) then minY := newY;
                if(newZ < minZ) then minZ := newZ;

                if(newX > maxX) then maxX := newX;
                if(newY > maxY) then maxY := newY;
                if(newZ > maxZ) then maxZ := newZ;
            end;
}
            SetElementNativeValues(curV, 'X', newX);
            SetElementNativeValues(curV, 'Y', newY);
            SetElementNativeValues(curV, 'Z', newZ);
        end;

        Result := navmToEdit;
    end;

    procedure processLoadDoor(door: IInterface);
    var
        doorToEdit: IInterface;
        newX, newY, newZ: float;
    begin
        doorToEdit := getElementToEdit(door);

        newX := GetElementNativeValues(door, 'XTEL\Position/Rotation\Position\X')+offsetX;
        newY := GetElementNativeValues(door, 'XTEL\Position/Rotation\Position\Y')+offsetY;
        newZ := GetElementNativeValues(door, 'XTEL\Position/Rotation\Position\Z')+offsetZ;

        SetElementNativeValues(doorToEdit, 'XTEL\Position/Rotation\Position\X', newX);
        SetElementNativeValues(doorToEdit, 'XTEL\Position/Rotation\Position\Y', newY);
        SetElementNativeValues(doorToEdit, 'XTEL\Position/Rotation\Position\Z', newZ);
    end;

    function processRef(ref: IInterface): IInterface;
    var
        refToEdit: IInterface;
        newX, newY, newZ: float;
        otherDoor: IInterface;
    begin
        numRefs := numRefs + 1;
        // easy
        refToEdit := getElementToEdit(ref);

        newX := GetElementNativeValues(ref, 'DATA\Position\X')+offsetX;
        newY := GetElementNativeValues(ref, 'DATA\Position\Y')+offsetY;
        newZ := GetElementNativeValues(ref, 'DATA\Position\Z')+offsetZ;
{
        if(isFirstRef) then begin
            minX := newX;
            minY := newY;
            minZ := newZ;

            maxX := newX;
            maxY := newY;
            maxZ := newZ;
            isFirstRef := false;
        end else begin
            if(newX < minX) then minX := newX;
            if(newY < minY) then minY := newY;
            if(newZ < minZ) then minZ := newZ;

            if(newX > maxX) then maxX := newX;
            if(newY > maxY) then maxY := newY;
            if(newZ > maxZ) then maxZ := newZ;
        end;
}
        SetElementNativeValues(refToEdit, 'DATA\Position\X', newX);
        SetElementNativeValues(refToEdit, 'DATA\Position\Y', newY);
        SetElementNativeValues(refToEdit, 'DATA\Position\Z', newZ);

        // MORE: if this is a loaddoor, also process the other door's doormarker
        otherDoor := pathLinksTo(refToEdit, 'XTEL\Door');
        if(assigned(otherDoor)) then begin
            processLoadDoor(otherDoor);
        end;

        Result := refToEdit;
    end;

    procedure fillWsList(wsList: TStringList);
    var
        i, iFiles: integer;
        curFile, wsGroup, curWs: IInterface;
        edid: string;
    begin
        for iFiles := 0 to FileCount-1 do begin
            curFile := FileByIndex(iFiles);

            if(assigned(curFile)) then begin
                wsGroup := GroupBySignature(curFile, 'WRLD');

                for i:=0 to ElementCount(wsGroup)-1 do begin
                    curWs := ElementByIndex(wsGroup, i);
                    if(assigned(curWs)) then begin
                        edid := EditorID(curWs);
                        if(edid <> '') and (wsList.indexOf(edid) < 0) then begin
                            wsList.Add(edid);
                        end;
                    end;
                end;
            end;
        end;
    end;

    function showGui(cell: IInterface): boolean;
    var
        frm: TForm;
        grp: TGroupBox;
        yOffset, yOffsetBox: integer;
        inputX, inputY, inputZ, inputName, inputEdid: TEdit;

        btnOk, btnCancel: TButton;
        resultCode: cardinal;

        targetFileBox: TComboBox;
        newFileName, newWsEdid: string;
        inputBox: TComboBox;


    begin
        if(not assigned(targetFile)) then begin
            targetFile := GetFile(cell);
        end;

        newWsEdid := EditorID(cell)+'_WS';

        yOffset := 0;
        Result := false;
        frm := CreateDialog('Interior To Worldspace', 500, 400);

        yOffset := yOffset + 8;
        CreateLabel(frm, 10, yOffset, 'Current Cell: '+Name(cell));
        yOffset := yOffset + 20;

        CreateLabel(frm, 10, yOffset + 3, 'Worldspace Name: ');
        inputName := CreateInput(frm, 120, yOffset, DisplayName(cell));
        inputName.Width := 330;

        yOffset := yOffset + 24;
        CreateLabel(frm, 10, yOffset + 3, 'Worldspace EDID: ');

        inputBox := CreateComboBox(frm, 120, yOffset, 330, nil);
        inputBox.Items.Add(newWsEdid);
        fillWsList(inputBox.Items);
        inputBox.ItemIndex := 0;
        yOffset := yOffset + 24;
        CreateLabel(frm, 10, yOffset, 'If you select an existing Worldspace, references will be added to it.');

        //inputEdid := CreateInput(frm, 120, yOffset, newWsEdid);
        //inputEdid.Width := 330;



        yOffset := yOffset + 34;
        //CreateLabel(frm, 10, 10, 'Offset Amounts:');
        grp := CreateGroup(frm, 10, yOffset, 480, 100, 'Offset Amounts (optional)');
        //yOffset := yOffset + 28;
        yOffsetBox := 8;
        CreateLabel(grp, 20, 16+yOffsetBox, 'X:');
        inputX := CreateInput(grp, 40, 14+yOffsetBox, '0.0');
        inputX.Width := 400;

        yOffsetBox := yOffsetBox + 24;
        CreateLabel(grp, 20, 16+yOffsetBox, 'Y:');
        inputY := CreateInput(grp, 40, 14+yOffsetBox, '0.0');
        inputY.Width := 400;

        yOffsetBox := yOffsetBox + 24;
        CreateLabel(grp, 20, 16+yOffsetBox, 'Z:');
        inputZ := CreateInput(grp, 40, 14+yOffsetBox, '0.0');
        inputZ.Width := 400;

        yOffset := yOffset + 120;
        CreateLabel(frm, 40, yOffset+2, 'Target File:');
        targetFileBox := CreateFileSelectDropdown(frm, 120, yOffset, 200, targetFile, true);
        yOffset := yOffset + 40;

        btnOk := CreateButton(frm, 80, yOffset, '    OK    ');
        btnCancel := CreateButton(frm, 280, yOffset, '  Cancel  ');

        btnCancel.ModalResult := mrCancel;
        btnOk.ModalResult := mrOk;

        resultCode := frm.showModal();
        if (resultCode <> mrOk) then begin
            frm.free();
            exit;
        end;

        offsetX := StrToFloat(inputX.text);
        offsetY := StrToFloat(inputY.text);
        offsetZ := StrToFloat(inputZ.text);

        targetWsName := trim(inputName.Text);
        targetWsEdid := trim(inputBox.Text);

        if(targetFileBox.ItemIndex = 0) then begin
            // add new file

            if(not InputQuery('Interior To Worldspace', 'Enter New File Name (with or without extension)', newFileName)) then begin
                frm.free();
                exit;
            end;
            newFileName := trim(newFileName);
            if(newFileName = '') then begin
                frm.free();
                exit;
            end;

            if (not strEndsWithCI(newFileName, '.esp')) and (not strEndsWithCI(newFileName, '.esl')) and (not strEndsWithCI(newFileName, '.esm')) then begin
                newFileName := newFileName+'.esp';
            end;

            targetFile := AddNewFileName(newFileName);
            if(not assigned(targetFile)) then begin
                frm.free();
                exit;
            end;
        end else begin
            newFileName := targetFileBox.Items[targetFileBox.ItemIndex];
            targetFile := FindFile(newFileName);

            if(not assigned(targetFile)) then begin
                AddMessage('ERROR');
                frm.free();
                exit;
            end;
        end;

        frm.free();
        Result := true;
    end;

    // Called before processing
    // You can remove it if script doesn't require initialization code
    function Initialize: integer;
    begin
        Result := 0;
        isFirstRef := true;

        numNavmeshes := 0;
        numRefs := 0;

        // new bounds
        minX := 0;
        minY := 0;
        minZ := 0;
        maxX := 0;
        maxY := 0;
        maxZ := 0;
    end;

    function getRefsInGroup(childGroup: IInterface): TStringList;
    var
        i: integer;
        ref: IInterface;
        sig: string;
    begin
        Result := TStringList.create;
        for i := 0 to ElementCount(childGroup)-1 do begin
            ref := ElementByIndex(childGroup, i);
            sig := Signature(ref);
            if (isReferenceSignature(sig)) or (sig = 'NAVM') then begin
                // AddMessage('Would process '+Name(ref));
                Result.AddObject(Name(ref), ref);
            end;
        end;
    end;

    procedure iterateCellSubGroup(childGroup: IInterface; cell: IInterface);
    var
        i: integer;
        ref: IInterface;
        sig: string;
    begin
        for i := 0 to ElementCount(childGroup)-1 do begin
            ref := ElementByIndex(childGroup, i);
            sig := Signature(ref);
            if (isReferenceSignature(sig)) or (sig = 'NAVM') then begin
                AddMessage('Would process '+Name(ref));
            end;
        end;
    end;

    function AddNewRecordToGroup(const g: IInterface; const s: String): IInterface;
    begin
        Result := Add(g, s, True);
        if not Assigned(Result) then
            Result := Add(g, s, True); // tries twice because
    end;

    function createWorldspace(edid, cellName: string): IInterface;
    var
        cellGroup: IInterface;
    begin
        cellGroup := GroupBySignature(targetFile, 'WRLD');
        if(not assigned(cellGroup)) then begin
            cellGroup := Add(targetFile, 'WRLD', True);
        end;

        Result := AddNewRecordToGroup(cellGroup, 'WRLD');

        SetElementEditValues(Result, 'EDID', edid);
        SetElementEditValues(Result, 'FULL', cellName);
    end;

    procedure applyCellSettingsToWs(cell, newWs: IInterface);
    var
        newWsFlags, cellFlags: cardinal;
        hasWater, noLodWater, cantTravel, showSky: boolean;
        waterType: IInterface;
    begin
        cellFlags := GetElementNativeValues(cell, 'DATA - Flags');
        // now, create new worldspace
        // always:
        // - "DNAM\Default Land Height" = 0
        // - Data - Flags 1 (Small World)
        // - Data - Flags 5 (No Land)
        // - Data - Flags 8 (No Grass)
        newWsFlags := 1 + (1 shl 4) + (1 shl 7);

        // add the DNAM
        Add(newWs, 'DNAM', true);

        AddMessage('CellFlags '+IntToStr(cellFlags));
        AddMessage('newWsFlags '+IntToStr(newWsFlags));

        hasWater   := (cellFlags and (1 shl 1)) <> 0;
        cantTravel := (cellFlags and (1 shl 2)) <> 0;
        noLodWater := (cellFlags and (1 shl 3)) <> 0;
        showSky    := (cellFlags and (1 shl 7)) <> 0;

        // sky: XCCM? but where to put it?

        // - no LOD water: DATA - Flags 4 -> Data - Flags 4
        if(noLodWater) then begin
            newWsFlags := (newWsFlags or (1 shl 3));
        end;

        // - can't travel from here: cell: DATA - Flags 3  -> Data - Flags 2
        if(cantTravel) then begin
            newWsFlags := (newWsFlags or (1 shl 1));
        end;

        // - show sky: cell: DATA - Flags 8 -> NOT Data - Flags 6
        if(not showSky) then begin
            newWsFlags := (newWsFlags or (1 shl 5));
        end;

        SetElementNativeValues(newWs, 'DATA - Flags', newWsFlags);

        // - Has Water: DATA - Flags 2  -> nothing, set "DNAM\Default Water Height" to -500024.0000 or such
        if(hasWater) then begin
            // - Water Height: XCLW: if has water is enabled, set this to Default Water height?
            waterType := GetElementNativeValues(cell, 'XCLW');
            if(assigned(waterType)) then begin
                SetElementNativeValues(newWs, 'DNAM\Default Water Height', waterType);
                SetElementNativeValues(newWs, 'NAM4', waterType);
            end;
            // also set the water type
            // XCWT -> NAM2 and NAM3
            setPathLinksTo(newWs, 'NAM2', pathLinksTo(cell, 'XCWT'));
            setPathLinksTo(newWs, 'NAM3', pathLinksTo(cell, 'XCWT'));
        end else begin
            SetElementNativeValues(newWs, 'DNAM\Default Water Height', -500024.0);
        end;

        // - Music Type: XCMO -> ZNAM
        setPathLinksTo(newWs, 'ZNAM', pathLinksTo(cell, 'XCMO'));
        // - Location: XLCN -> XLCN
        setPathLinksTo(newWs, 'XLCN', pathLinksTo(cell, 'XLCN'));
        // - EncounterZone: XEZN -> XEZN
        setPathLinksTo(newWs, 'XEZN', pathLinksTo(cell, 'XEZN'));


        // we can port over:
        // // - Name: FULL -> FULL



        // we can't:
        // - Directional Ambient Lighting??? XCLL?
        // - most stuff from Interior Data
        // - lighting template: LTMP

        // per-cell:
        // - player followers can't travel: DATA - Flags 14
        // - ImageSpace -> can be done to individual cells: XCIM
        // - Default Acoustic Space: XCAS

        // newWs := createWorldsapce
    end;

    function getGridCoord(num: integer): integer;
    var
        res: integer;
    begin
        if(num < 0) then begin
            Result := getGridCoord(num * -1) * -1 - 1;
        end else begin
            Result := num shr 12;
        end;
    end;

    procedure transferReference(ref, toCell: IInterface);
    var
        processedRef: IInterface;
    begin
        if(Signature(ref) = 'NAVM') then begin
            processedRef := processNavm(ref);
        end else begin
            processedRef := processRef(ref);
        end;
        // relink to the new cell
        setPathLinksTo(processedRef, 'CELL', toCell);
        //processNavm processRef
    end;

    procedure processPermSubgroup(theCell, grp, newWs: IInterface);
    var
        cnt, i: integer;
        ref: IInterface;
        sig: string;

        persCell: IInterface;
        refs: TStringList;
    begin


        if(not assigned(grp)) then begin
            AddMessage('No childgroup??');
            exit;
        end;

        refs := getRefsInGroup(grp);


        cnt := refs.count;

        if(cnt <= 0) then begin
            AddMessage('No permanent refs?');
            exit;
        end;

        persCell := Add(newWs, 'CELL[P]', true);

        for i := 0 to cnt-1 do begin
            ref := ObjectToElement(refs.Objects[i]);
            sig := Signature(ref);
            //if (isReferenceSignature(sig)) or (sig = 'NAVM') then begin
                // AddMessage('Would process '+Name(ref));
                transferReference(ref, persCell);
            //end;
        end;
        
        refs.free();
    end;

    function getWorldspaceCell(ws: IInterface; gridX, gridY: integer): IInterface;
    var
        i, x, y: integer;
        cell: IInterface;
        blockidx, subblockidx, cellidx: integer;
        wrldgrup, block, subblock: IInterface;
    begin
        wrldgrup := ChildGroup(ws);
        for blockidx := 0 to ElementCount(wrldgrup)-1 do begin
            block := ElementByIndex(wrldgrup, blockidx);
            // traverse SubBlocks
            for subblockidx := 0 to ElementCount(block)-1 do begin
                subblock := ElementByIndex(block, subblockidx);
                // traverse Cells
                for cellidx := 0 to ElementCount(subblock)-1 do begin
                    cell := ElementByIndex(subblock, cellidx);
                    x := getElementNativeValues(cell, 'XCLC\X');
                    y := getElementNativeValues(cell, 'XCLC\Y');
                    if(x = gridX) and (y = gridY) then begin
                        Result := cell;
                        exit;
                    end;
                end;
            end;
        end;

        Result := Add(ws, 'CELL['+IntToStr(gridX)+','+IntToStr(gridY)+']', true);
    end;

    procedure processTempSubgroup(cell, grp, newWs: IInterface);
    var
        cnt, i, gridX, gridY, x, y: integer;
        ref: IInterface;
        sig: string;

        curCell: IInterface;
        refs: TStringList;
    begin

        if(not assigned(grp)) then exit;
        
        refs := getRefsInGroup(grp);

        cnt := refs.count;
        if(cnt <= 0) then exit;

        for i := 0 to cnt-1 do begin
            ref := ObjectToElement(refs.Objects[i]);
            // sig := Signature(ref);
            //if (isReferenceSignature(sig)) or (sig = 'NAVM') then begin
                x := getElementNativeValues(ref, 'DATA\Position\X');
                y := getElementNativeValues(ref, 'DATA\Position\Y');

                gridX := getGridCoord(x + offsetX);
                gridY := getGridCoord(y + offsetY);

                curCell := getWorldspaceCell(newWs, gridX, gridY);
                // AddMessage('Would process '+Name(ref));
                transferReference(ref, curCell);
            //end;
        end;
        
        refs.free();
    end;

    procedure processCell(cell: IInterface);
    var
        newWs, persCell, gridCell, permGroup, tempGroup: IInterface;
    begin

        //a cell is 4096 units across
        // first, show UI
        if(not showGui(cell)) then exit;

        //cellCache := TStringList.create;
        addRequiredMastersSilent(cell, targetFile);

        AddMessage('ws='+targetWsName+', '+targetWsEdid);
        newWs := GetFormByEdid(targetWsEdid);
        if(assigned(newWs)) then begin
            if(Signature(newWs) <> 'WRLD') then begin
                AddMessage('"'+targetWsEdid+'" is not a worldspace! It''s a '+Signature(newWs));
                exit;
            end;
        end else begin
            newWs := createWorldspace(targetWsEdid, targetWsname);
            applyCellSettingsToWs(cell, newWs);
        end;
 {
        AddMessage('PERM');
        iterateCellSubGroup(FindChildGroup(ChildGroup(cell), 8, cell), cell);
        AddMessage('TEMP');
        iterateCellSubGroup(FindChildGroup(ChildGroup(cell), 9, cell), cell);

}
        // okay what the hell? I can call it from here, but not from there??
        // iterateCellSubGroup(FindChildGroup(ChildGroup(cell), 8, cell), cell);
        // it seems that this function is cursed, and the whole FindChildGroup shebang only works from within this function, and not from anywhere else.
        permGroup := FindChildGroup(ChildGroup(cell), 8, cell);
        tempGroup := FindChildGroup(ChildGroup(cell), 9, cell);
        AddMessage('PermGroup? '+BoolToStr(Assigned(permGroup)));
        AddMessage('Tempgroup? '+BoolToStr(Assigned(tempGroup)));

        //

        processPermSubgroup(cell, permGroup, newWs);
        processTempSubgroup(cell, tempGroup, newWs);
        exit;

       // cellCache.free();

        // okay, how TF do i put a ref into this WS now?
        // persCell := Add(newWs, 'CELL[P]', true);
        // gridCell := Add(newWs, 'CELL[1,2]', true);
        exit;



        // then, transfer the refs
        // 9 is temporary
        // 8 seems to be persistent
        iterateCellSubGroup(FindChildGroup(ChildGroup(cell), 8, cell), cell);
        iterateCellSubGroup(FindChildGroup(ChildGroup(cell), 9, cell), cell);
    end;

    // called for every record selected in xEdit
    function Process(e: IInterface): integer;
    var
        parentCell: IInterface;
        curSig: string;
    begin
        Result := 0;

        curSig := Signature(e);
        if(curSig <> 'CELL') then exit;
        if(GetElementEditValues(e, 'DATA\Is Interior Cell') <> '1') then exit;

        processCell(e);
        exit;

        // comment this out if you don't want those messages
        // AddMessage('Processing: ' + FullPath(e));
        if (not isReferenceSignature(curSig)) and (curSig <> 'NAVM') then exit;

        if (not assigned(targetCell)) then begin

            parentCell := pathLinksTo(e, 'CELL');
            if(not assigned(parentCell)) then exit;
            if(GetElementEditValues(parentCell, 'DATA\Is Interior Cell') <> '1') then exit;

            if(not showGui(GetFile(parentCell))) then begin
                Result := 1;
                exit;
            end;

            targetCell := parentCell;
            AddMessage('Processing cell '+Name(targetCell));
        end;
        // otherwise continue and process this ref
        // failsafe
        parentCell := pathLinksTo(e, 'CELL');
        if(not isSameForm(parentCell, targetCell)) then exit;


        AddMessage('Processing '+Name(e));
        if(curSig <> 'NAVM') then begin
            processRef(e);
        end else begin
            processNavm(e);
        end;
    end;

    // Called after processing
    // You can remove it if script doesn't require finalization code
    function Finalize: integer;
    begin
        Result := 0;

        {
        AddMessage('Processed '+IntToStr(numRefs)+' references and '+IntToStr(numNavmeshes)+' navmeshes.');
        AddMessage('New Cell Bounds are:');
        AddMessage('Xmin: '+FloatToStr(minX)+'; Xmax: '+FloatToStr(maxX));
        AddMessage('Ymin: '+FloatToStr(minY)+'; Ymax: '+FloatToStr(maxY));
        AddMessage('Zmin: '+FloatToStr(minZ)+'; Zmax: '+FloatToStr(maxZ));
        numNavmeshes := 0;
        numRefs := 0;

        // new bounds
        minX := 0;
        minY := 0;
        minZ := 0;
        maxX := 0;
        maxY := 0;
        maxZ := 0;}
    end;

end.