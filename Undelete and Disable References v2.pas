{
    Undelete and disable references and navmeshes.
    Based on the original xEdit script, with more options and features added.

    Without any other options, references will be set to initially disabled, to have the player as enable parent with opposite state, and will have their persistent flag removed, if possible.
    "If possible" means: always for master records, but for overrides, this will restore the master's state.

    If you want navmesh processing to work, you must disable the xEdit setting "Simple records LAND, NAVI, NAVM, CELL, WRLD".
    It's found in the ≡ menu in the top left of xEdit. Or press Ctrl+O.

    If you let the script process navmeshes, you must refinalize the navmeshes in the affected cells in the CreationKit afterwards.
    A list of cells will be output at the end.

    Required Scripts:
        - praUtil.pas
        - CobbLibrary.pas
        - XeditSimpleMath.pas

    Explanation of the options in the UI:
        - Process Navmeshes:
                            If enabled, deleted navmeshes will be undeleted, and reduced to 3 vertices at the intermediate point,
                            with one triange with the edge length of 10.
        - Set Z Coordinate:
                            You can make the script set the Z coordinate to -30000, subtract -30000, or do nothing.
        - Reapply to Undeleted:
                            The script will process references and navmeshes which have been undeleted already.
                            Useful if you want to move undeleted refs onto a layer or into a cell,
                            or regenerate the dummy precombine after regenerating real previsibines.
        - Reapply to 'Initially Disabled'
                            This affects the "Reapply to Undeleted" option: if both are enabled, the "Initially Disabled" option alone will be enough
                            for a ref to be considered undeleted.
                            WARNING: this option can and will catch references like workshop borders, or refs initially disabled for quest reasons, etc
        - Move deleted to layer:
                            Specify the layer's Name/EditorID (it's the same thing) in the field below. Undeleted references will be moved to that layer.
                            If the layer doesn't exist, it will be created.
        - Move deleted to cell:
                            Specify the cell's EditorID in the field below. Undeleted references will be moved into that cell.
                            If the cell doesn't exist, the script will exit.
        - Create dummy precombine:
                            This affects the "Move deleted to cell" option:
                            A fake precombine will be created in that cell, and all relevant static references will be added to that.
}
unit UndeleteStuff;
    uses praUtil;
    const
        configFile = ScriptsPath + 'Undelete and Disable References v2.cfg';

    var
        UndeletedCount: integer;
        settingProcessNavmeshes, settingMoveToLayer, settingMoveToCell, settingReapply, settingDummyPrecomb, settingDisabledIsEnough: boolean;
        settingZCoordMode: integer; // 0 => nothing, 1 => set to -30k, 2 => subtract -30k
        settingTargetLayerName, settingTargetCellEdid: string;
        targetLayer, targetCell: IInterface;
        cellsToRefinalize: TStringList;

    procedure loadConfig();
    var
        i, j, breakPos: integer;
        curLine, curKey, curVal: string;
        lines : TStringList;
    begin
        // default
        // navmesh settings
        settingProcessNavmeshes := true;

        // ref settings
        settingMoveToLayer := false;
        settingMoveToCell := false;

        settingTargetCellEdid := '';
        settingDummyPrecomb := false;
        settingZCoordMode := 1;
        settingTargetLayerName := 'deleted';

        // other settings
        settingReapply := false;
        settingDisabledIsEnough := false;


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

                if(curKey = 'TargetLayer') then begin
                    settingTargetLayerName := curVal;
                end else if(curKey = 'TargetCell') then begin
                    settingTargetCellEdid := curVal;
                end else if(curKey = 'ProcessNavmeshes') then begin
                    settingProcessNavmeshes := StrToBool(curVal);
                end else if(curKey = 'MoveToLayer') then begin
                    settingMoveToLayer := StrToBool(curVal);
                end else if(curKey = 'MoveToCell') then begin
                    settingMoveToCell := StrToBool(curVal);
                end else if(curKey = 'ReapplyState') then begin
                    settingReapply := StrToBool(curVal);
                end else if(curKey = 'DisabledIsEnough') then begin
                    settingDisabledIsEnough := StrToBool(curVal);
                end else if(curKey = 'DummyPrecombine') then begin
                    settingDummyPrecomb := StrToBool(curVal);
                end else if(curKey = 'ZCoordMode') then begin
                    settingZCoordMode := StrToInt(curVal);
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
        lines.add('TargetLayer='+settingTargetLayerName);
        lines.add('TargetCell='+settingTargetCellEdid);
        lines.add('ProcessNavmeshes='+BoolToStr(settingProcessNavmeshes));
        lines.add('MoveToLayer='+BoolToStr(settingMoveToLayer));
        lines.add('MoveToCell='+BoolToStr(settingMoveToCell));
        lines.add('ReapplyState='+BoolToStr(settingReapply));
        lines.add('DummyPrecombine='+BoolToStr(settingDummyPrecomb));
        lines.add('DisabledIsEnough='+BoolToStr(settingDisabledIsEnough));
        lines.add('ZCoordMode='+IntToStr(settingZCoordMode));

        lines.saveToFile(configFile);
        lines.free();
    end;


    function showConfigDialog(): boolean;
    var
        frm: TForm;
        cbProcessNavmeshes, cbMoveToLayer, cbMoveToCell, cbDoReapply, cbDummyPrecomb, cbDisabledEnough: TCheckBox;
        rgMoveMode: TRadioGroup;
        items: TStringList;
        editLayer, editCell: TEdit;
        resultCode: cardinal;
        btnOkay, btnCancel: TButton;
        yOffset: integer;
    begin
        loadConfig();
        Result := false;
        frm := CreateDialog('Undelete and Disable Settings', 350, 350);

        cbProcessNavmeshes := CreateCheckbox(frm, 10, 10, 'Process Navmeshes');
        if(settingProcessNavmeshes) then begin
            cbProcessNavmeshes.checked := true;
        end;

        items := TStringList.create();
        items.add('Do not change');
        items.add('Set to -30000');
        items.add('Subtract -30000');
        rgMoveMode := CreateRadioGroup(frm, 10, 35, 300, 80, 'Set Z coordinate', items);
        rgMoveMode.ItemIndex := settingZCoordMode;

        yOffset := 125;
        //settingReapply, settingDummyPrecomb
        cbDoReapply := CreateCheckbox(frm, 10, yOffset, 'Reapply to undeleted');
        cbDoReapply.checked := settingReapply;

        //
        cbDisabledEnough := CreateCheckbox(frm, 160, yOffset, 'Reapply to ''Initially Disabled''');
        cbDisabledEnough.checked := settingDisabledIsEnough;


        yOffset := 150;

        cbMoveToLayer := CreateCheckbox(frm, 10, yOffset, 'Move deleted to layer');
        cbMoveToLayer.checked := settingMoveToLayer;

        CreateLabel(frm, 10, yOffset + 23, 'Layer:');
        editLayer := CreateInput(frm, 66, yOffset + 20, settingTargetLayerName);
        editLayer.width := 244;

        yOffset := yOffset + 60;

        cbMoveToCell := CreateCheckbox(frm, 10, yOffset, 'Move deleted to cell');
        cbMoveToCell.checked := settingMoveToCell;

        cbDummyPrecomb := CreateCheckbox(frm, 160, yOffset, 'Create dummy precombine');
        cbDummyPrecomb.checked := settingDummyPrecomb;

        CreateLabel(frm, 10, yOffset + 23, 'Cell EDID:');
        editCell := CreateInput(frm, 66, yOffset + 20, settingTargetCellEdid);
        editCell.width := 244;

        yOffset := yOffset + 50;

        btnOkay := CreateButton(frm, 90, yOffset, '  OK  ');
        btnOkay.ModalResult := mrYes;
        btnOkay.Default := true;

        btnCancel := CreateButton(frm, 180, yOffset, 'Cancel');
        btnCancel.ModalResult := mrCancel;

        resultCode := frm.showModal();

        if(resultCode = mrYes) then begin
            settingProcessNavmeshes := cbProcessNavmeshes.checked;
            settingMoveToLayer := cbMoveToLayer.checked;
            settingMoveToCell := cbMoveToCell.checked;

            settingZCoordMode := rgMoveMode.ItemIndex;
            settingTargetLayerName := editLayer.text;
            settingTargetCellEdid := editCell.text;

            settingDummyPrecomb := cbDummyPrecomb.checked;
            settingReapply := cbDoReapply.checked;
            settingDisabledIsEnough := cbDisabledEnough.checked;


            Result := true;
            if(settingMoveToCell) then begin
                targetCell := findInteriorCellByEdid(settingTargetCellEdid);
                if(not assigned(targetCell)) then begin
                    AddMessage('Failed to find cell "'+settingTargetCellEdid+'"');
                    Result := false;
                end;
            end;

            saveConfig();
        end;

        items.free();
        frm.free();
    end;

    procedure removeAllChildren(e: IInterface);
    begin
        // try this
        while(ElementCount(e) > 0) do begin
            //AddMessage('removing something');
            RemoveElement(e, 0);
        end;
    end;

    function hasAnyChildren(e: IInterface): boolean;
    begin
        Result := false;
        if (not assigned(e)) then exit;

        Result := ElementCount(e) > 0;
    end;

    function getOverriddenForm(e, targetFile: IInterface): IInterface;
    var
        eFile: IInterface;
    begin
        Result := e;
        if(not assigned(e)) then exit;

        eFile := GetFile(MasterOrSelf(e));
        if(not FilesEqual(eFile, targetFile)) then begin
            Result := getOrCreateElementOverride(e, targetFile);
        end;

    end;

    function AddGroupBySignature(const f: IwbFile; const s: String): IInterface;
    begin
        Result := GroupBySignature(f, s);
        if not Assigned(Result) then
            Result := Add(f, s, True);
    end;

    function getLayer(inFile: IInterface; layerName: string): IInterface;
    var
        curMaster, myLayrGroup, foundLayer: IInterface;
        i: integer;
    begin
        // getOrCreateElementOverride
        //foundLayer := FindLayerByEdid(layerName);
        foundLayer := FindObjectByEdidAndSignature(layerName, 'LAYR');
        if(assigned(foundLayer)) then begin
            Result := foundLayer;
            exit;
        end;

        Result := nil;
        myLayrGroup := AddGroupBySignature(inFile, 'LAYR');

        AddMessage('Creating layer '+layerName);
        // create new
        foundLayer := Add(myLayrGroup, 'LAYR', true);//ensurePath(myLayrGroup, 'LAYR');
        setElementEditValues(foundLayer, 'EDID', layerName);


        Result := foundLayer;
    end;

    procedure registerNavmesh(navm: IInterface);
    var
        parentCell: IInterface;
        stringKey: string;
    begin
        parentCell := MasterOrSelf(pathLinksTo(navm, 'Cell'));
        stringKey := FormToAbsStr(parentCell);


        if(cellsToRefinalize.indexOf(stringKey) >= 0) then begin
            exit;
        end;

        // stringKey := EditorID(parentCell)+' "'+DisplayName(parentCell)+'" [CELL:'+IntToHex(FormID(parentCell), 8)+']';
        cellsToRefinalize.addObject(stringKey, parentCell);

        // AddMessage(stringKey);
        // cellsToRefinalize
    end;


    procedure processNavm(navm: IInterface);
    var
        i, numVertices: integer;
        meanX, meanY, meanZ, p1x, p2x, p3x, p1y, p2y, p3y: float;
        vertices, triangles, curV, curTri, nvmn, grid, gridArrays, cellWhat: IInterface;

    begin

        if(not settingProcessNavmeshes) then begin
            // AddMessage('Skipping deleted navmesh: ');
            AddMessage('Skipping deleted navmesh: ' + Name(navm));
            exit;
        end;

        registerNavmesh(navm);

        AddMessage('Undeleting: ' + Name(navm));
        // leave as-is:
        //  Pathing Cell
        // we will add:
        //  - Exactly 3 vertices
        //  - 1 Triangle
        //  - Navmesh Grid
        //      - Max X Distance = MaxX - MinX
        //      - Max Y Distance = MaxY - MinY

        nvmn := ElementByPath(navm, 'NVNM - Navmesh Geometry');
        vertices := ElementByPath(nvmn, 'Vertices');
        if(not assigned(vertices)) then begin
            exit;
        end;

        triangles := ElementByPath(nvmn, 'Triangles');
        if(not assigned(triangles)) then begin
            exit;
        end;

        // calculate average coords
        meanX := 0;
        meanY := 0;
        meanZ := 0;
        numVertices := ElementCount(vertices);
        for i := 0 to numVertices - 1 do begin
            curV := ElementByIndex(vertices, i);
            meanX := meanX + GetElementNativeValues(curV, 'X');
            meanY := meanY + GetElementNativeValues(curV, 'Y');
            meanZ := meanZ + GetElementNativeValues(curV, 'Z');
        end;

        meanX := meanX / numVertices;
        meanY := meanY / numVertices;
        meanZ := meanZ / numVertices;

        // 0 -> leave it as-is
        if (settingZCoordMode = 1) then begin
            // 1 -> set to -30k
            meanZ := -30000;
        end else if(settingZCoordMode = 2) then begin
            // 2 -> subtract -30k
            meanZ := meanZ - 30000;
        end;

        // point1

        p1x := meanX;
        p1y := meanY;

        p2x := meanX + 10;
        p2y := meanY;

        p3x := meanX;
        p3y := meanY + 10;

        // clear the stuff
        removeAllChildren(vertices);
        removeAllChildren(triangles);
        removeAllChildren(ElementByPath(nvmn, 'Edge Links'));
        removeAllChildren(ElementByPath(nvmn, 'Door Triangles'));
        removeAllChildren(ElementByPath(nvmn, 'Unknown 5'));
        removeAllChildren(ElementByPath(nvmn, 'Unknown 6'));
        removeAllChildren(ElementByPath(nvmn, 'Waypoints'));
        removeAllChildren(ElementByPath(nvmn, 'MNAM - PreCut Map Entries'));

        // write vertices
        curV := ElementAssign(vertices, HighInteger, nil, false);
        SetElementNativeValues(curV, 'X', p1x);
        SetElementNativeValues(curV, 'Y', p1y);
        SetElementNativeValues(curV, 'Z', meanZ);

        curV := ElementAssign(vertices, HighInteger, nil, false);
        SetElementNativeValues(curV, 'X', p2x);
        SetElementNativeValues(curV, 'Y', p2y);
        SetElementNativeValues(curV, 'Z', meanZ);

        curV := ElementAssign(vertices, HighInteger, nil, false);
        SetElementNativeValues(curV, 'X', p3x);
        SetElementNativeValues(curV, 'Y', p3y);
        SetElementNativeValues(curV, 'Z', meanZ);

        // the tri
        curTri := ElementAssign(triangles, HighInteger, nil, false);
        SetElementNativeValues(curTri, 'Vertex 0', 0);
        SetElementNativeValues(curTri, 'Vertex 1', 1);
        SetElementNativeValues(curTri, 'Vertex 2', 2);

        SetElementEditValues(curTri, 'Edge 0-1', 'None');
        SetElementEditValues(curTri, 'Edge 1-2', 'None');
        SetElementEditValues(curTri, 'Edge 2-0', 'None');

        SetElementEditValues(curTri, 'Height', 'Default');
        // flags -> found?
        SetElementNativeValues(curTri, 'Flags', 2048); // flag #12, 1<<11

        // grid stuff
        grid := ElementByPath(nvmn, 'Navmesh Grid');
        SetElementNativeValues(grid, 'Navmesh Grid Size', 1);
        SetElementNativeValues(grid, 'Max X Distance', 10);
        SetElementNativeValues(grid, 'Max Y Distance', 10);

        SetElementNativeValues(grid, 'MinX', p1x);
        SetElementNativeValues(grid, 'MaxX', p2x);

        SetElementNativeValues(grid, 'MinY', p1y);
        SetElementNativeValues(grid, 'MaxY', p3y);

        SetElementNativeValues(grid, 'MinZ', meanZ);
        SetElementEditValues(grid, 'MaxZ', 'Default');


        gridArrays := ElementByPath(grid, 'NavMesh Grid Arrays');
        removeAllChildren(gridArrays);
        cellWhat := ElementAssign(gridArrays, HighInteger, nil, false);
        ElementAssign(cellWhat, HighInteger, nil, false);

        if(settingMoveToCell) and (assigned(targetCell)) then begin
            // let's try to also move the navmesh to the cell
            setPathLinksTo(navm, 'Cell', targetCell);
        end;

        Inc(UndeletedCount);
    end;

    function Initialize: integer;
    begin
        Result := 0;
        if(PRA_UTIL_VERSION < 12.0) then begin
            AddMessage('This requires praUtil.pas version 12.0 or higher.');
            Result := 1;
            exit;
        end;

        if(not showConfigDialog()) then begin
            Result := 1;
            exit;
        end;

        cellsToRefinalize := TStringList.create();
    end;

    function isNavmeshPseudoDeleted(navm: IInterface): boolean;
    var
        nvmn, vertices, triangles: IInterface;
        v1, v2, v3: IInterface;
        x1, x2, x3, y1, y2, y3, z1, z2, z3: float;
    begin

        AddMessage('checking navm undeletion');
        // check that we have exactly 3 vertices, one triangle, and nothing else.
        Result := false;
        nvmn := ElementByPath(navm, 'NVNM - Navmesh Geometry');
        vertices := ElementByPath(nvmn, 'Vertices');
        if(not assigned(vertices)) then begin
            exit;
        end;

        triangles := ElementByPath(nvmn, 'Triangles');
        if(not assigned(triangles)) then begin
            exit;
        end;

        if(ElementCount(vertices) <> 3) then exit;
        if(ElementCount(triangles) <> 1) then exit;

        if(hasAnyChildren(ElementByPath(nvmn, 'Edge Links'))) then exit;
        if(hasAnyChildren(ElementByPath(nvmn, 'Door Triangles'))) then exit;
        if(hasAnyChildren(ElementByPath(nvmn, 'Unknown 5'))) then exit;
        if(hasAnyChildren(ElementByPath(nvmn, 'Unknown 6'))) then exit;
        if(hasAnyChildren(ElementByPath(nvmn, 'Waypoints'))) then exit;
        if(hasAnyChildren(ElementByPath(nvmn, 'MNAM - PreCut Map Entries'))) then exit;

        // also check if the 3 vertices have the same Z and X/Y are within 10 of each other
        v1 := ElementByIndex(vertices, 0);
        v2 := ElementByIndex(vertices, 1);
        v3 := ElementByIndex(vertices, 2);

        x1 := GetElementNativeValues(v1, 'X');
        y1 := GetElementNativeValues(v1, 'Y');
        z1 := GetElementNativeValues(v1, 'Z');

        x2 := GetElementNativeValues(v2, 'X');
        y2 := GetElementNativeValues(v2, 'Y');
        z2 := GetElementNativeValues(v2, 'Z');

        x3 := GetElementNativeValues(v3, 'X');
        y3 := GetElementNativeValues(v3, 'Y');
        z3 := GetElementNativeValues(v3, 'Z');

        if(not floatEquals(z1, z2)) or (not floatEquals(z1, z3)) then exit;

        // I think I had a higher number here than 10 before, or maybe the ck updates the vertex coordinates, so let's say 20
        if(not floatEqualsWithTolerance(x1, x2, 20.0)) or (not floatEqualsWithTolerance(x1, x3, 20.0)) then exit;
        if(not floatEqualsWithTolerance(y1, y2, 20.0)) or (not floatEqualsWithTolerance(y1, y3, 20.0)) then exit;

        Result := true;
    end;

    function isReferencePseudoDeleted(e: IInterface): boolean;
    begin
        if(settingDisabledIsEnough) then begin
            if(GetIsInitiallyDisabled(e)) then begin
                Result := true;
                exit;
            end;
        end;

        // this checks for actual deleted, or is initially disabled && is enable state opposite of player
        if(isConsideredDeleted(e)) then begin
            Result := true;
            exit;
        end;

        // on the target layer?
        if(settingMoveToLayer) and (assigned(targetLayer)) then begin
            if(isSameForm(targetLayer, pathLinksTo(e, 'XLYR'))) then begin
                Result := true;
                exit;
            end;
        end;

        // or maybe in the target cell?
        if (settingMoveToCell) and (assigned(targetCell)) then begin
            if(isSameForm(targetCell, pathLinksTo(e, 'Cell'))) then begin
                Result := true;
                exit;
            end;
        end;

        Result := false;
    end;

    function getTodayTimestamp(): string;
    var
        YY,MM,DD: cardinal;
    begin
        DeCodeDate (Date,YY,MM,DD);

        Result := encodeHexTimestampString(dateToTimestamp(DD, MM, YY));
    end;

    procedure addToDummyPrecomb(ref, cell: IInterface);
    var
        visi, pcmb, newDate, newDateFoo, meshStr: string;
        xcri, meshes, references, curElem, testRef, meshEntry, refEntry: IInterface;
        i: integer;
    begin
        // maybe prepare the cell
        // we need
        // set VISI and PCMB
        visi := GetElementEditValues(cell, 'VISI');
        pcmb := GetElementEditValues(cell, 'PCMB');

        if(visi = '') or (pcmb = '') then begin
            // set them to today
            newDate := getTodayTimestamp();

            SetElementEditValues(cell, 'VISI', newDate);
            SetElementEditValues(cell, 'PCMB', newDate);
            //Add(ref, 'VISI', true);
            //Add(ref, 'PCMB', true);
        end;

        xcri := ensurePath(cell, 'XCRI');
        references := elementByPath(xcri, 'References');
        meshes := elementByPath(xcri, 'Meshes');
        // try finding
        for i:=0 to ElementCount(references)-1 do begin
            curElem := ElementByIndex(references, i);
            testRef := pathLinksTo(curElem, 'Reference');
            if(isSameForm(ref, testRef)) then exit; // nothing to do
        end;

        if(ElementCount(meshes) = 0) then begin
            // now again, trial and error, how does this thing want stuff to get appended?
            meshEntry := ElementAssign(meshes, HighInteger, nil, False);
        end else begin
            meshEntry := ElementByIndex(meshes, 0);
        end;

        meshStr := GetEditValue(meshEntry);


        refEntry := ElementAssign(references, HighInteger, nil, False);
        setPathLinksTo(refEntry, 'Reference', ref);
        SetElementEditValues(refEntry, 'Combined Mesh', meshStr);
    end;

    procedure SetInteriorCellPersistency(ref: IInterface; newPersistence: boolean; newCell: IInterface);
    var
        isPersistent, isSameCell: boolean;
        cell, permGroup, tempGroup, prevCell: IInterface;
    begin
        isPersistent := GetIsPersistent(ref);
        cell := pathLinksTo(ref, 'Cell');
        isSameCell := isSameForm(newCell, pathLinksTo(ref, 'Cell'));

        if(isPersistent = newPersistence) and (isSameCell) then exit; // all is well

        if(not isSameCell) then begin
            // easy
            SetIsPersistent(ref, newPersistence);
            setPathLinksTo(ref, 'Cell', newCell);
            exit;
        end;

        // same cell, but not same persistence
        permGroup := FindChildGroup(ChildGroup(cell), 8, cell);
        tempGroup := FindChildGroup(ChildGroup(cell), 9, cell);

        if(newPersistence) then begin
            // remove from temp, add to perm
            SetIsPersistent(ref, newPersistence);
            RemoveElement(tempGroup, ref);
            AddElement(permGroup, ref);
        end else begin
            // remove from perm, add to temp
            SetIsPersistent(ref, newPersistence);
            RemoveElement(permGroup, ref);
            AddElement(tempGroup, ref);
        end;
    end;

    procedure processReference(e: IInterface; isReapplying: boolean);
    var
        baseForm: IInterface;
        baseFormSig: string;
        prevOverride, xesp: IInterface;
        prevZpos: integer;
        isRefMaster, inDummyPrecomb, canDummyPrecomb, newPersistence: boolean;
    begin
        if(settingMoveToLayer) and (not assigned(targetLayer)) then begin
            targetLayer := getLayer(GetFile(e), settingTargetLayerName);
        end;

        inDummyPrecomb := false;
        prevOverride := nil;
        isRefMaster := isMaster(e);
        if(not isRefMaster) then begin
            prevOverride := getWinningOverrideBefore(e, GetFile(e));
        end;

        if(settingZCoordMode = 1) then begin
            SetElementNativeValues(e, 'DATA\Position\Z', -30000);
        end else if(settingZCoordMode = 2) then begin

            if (not isReapplying) then begin
                prevZpos := GetElementNativeValues(e, 'DATA\Position\Z');
                SetElementNativeValues(e, 'DATA\Position\Z', prevZpos - 30000);
            end else begin
                if(assigned(prevOverride)) then begin
                    prevZpos := GetElementNativeValues(prevOverride, 'DATA\Position\Z');
                    {
                    SetElementNativeValues(e, 'DATA\Rotation\X', getElementNativeValues(prevOverride, 'DATA\Rotation\X'));
                    SetElementNativeValues(e, 'DATA\Rotation\Y', getElementNativeValues(prevOverride, 'DATA\Rotation\Y'));
                    SetElementNativeValues(e, 'DATA\Rotation\Z', getElementNativeValues(prevOverride, 'DATA\Rotation\Z'));

                    SetElementNativeValues(e, 'DATA\Position\X', getElementNativeValues(prevOverride, 'DATA\Position\X'));
                    SetElementNativeValues(e, 'DATA\Position\Y', getElementNativeValues(prevOverride, 'DATA\Position\Y'));
                    }
                    SetElementNativeValues(e, 'DATA\Position\Z', prevZpos - 30000);
                end;
            end;
        end else if(settingZCoordMode = 0) then begin
            if(assigned(prevOverride)) then begin
                // take the original value
                SetElementNativeValues(e, 'DATA\Position\Z', GetElementNativeValues(prevOverride, 'DATA\Position\Z'));

            end;
        end;

        RemoveElement(e, 'Enable Parent');
        RemoveElement(e, 'XTEL');
        // ... remove anything else here
        // linked refs maybe
        RemoveElement(e, 'Linked References');
        // just in case
        RemoveElement(e, 'XLRL');
        RemoveElement(e, 'XLRT');





        //if(shouldChangePersist) then begin
            // maybe unpersist
        if(isRefMaster) then begin
            // yes
            newPersistence := false;
            // SetIsPersistent(e, False);
        end else if (assigned(prevOverride)) then begin
            // apply the state of the prev one, just to be safe
            newPersistence := GetIsPersistent(prevOverride);
            //SetIsPersistent(e, GetIsPersistent(prevOverride));
        end;
        //end;

        // set to disabled
        SetIsInitiallyDisabled(e, True);

        // set layer
        if(settingMoveToLayer) and (assigned(targetLayer)) then begin
            // targetLayer := getLayer(settingTargetLayerName);
            setPathLinksTo(e, 'XLYR', targetLayer);
        end;

        baseForm := pathLinksTo(e, 'NAME');
        baseFormSig := Signature(baseForm);

        if(settingMoveToCell) and (assigned(targetCell)) then begin
            //newPersistence
            SetInteriorCellPersistency(e, newPersistence, targetCell);
            // setPathLinksTo(e, 'CELL', targetCell);

            if(settingDummyPrecomb) then begin
                // probably only safe for statics
                if(baseFormSig = 'STAT') or (baseFormSig = 'SCOL') then begin
                    inDummyPrecomb := true;
                    addToDummyPrecomb(e, targetCell);
                end;
            end;
        end else begin
            SetIsPersistent(e, newPersistence);
        end;

        if (not inDummyPrecomb) then begin
            // add enabled opposite of player (true - silent)
            xesp := Add(e, 'XESP', True);
            if Assigned(xesp) then begin
                SetElementNativeValues(xesp, 'Reference', $14); // Player ref
                SetElementNativeValues(xesp, 'Flags', 1);  // opposite of parent flag
            end;
        end else begin
            // remove the XESP
            RemoveElement(e, 'XESP');
        end;

        Inc(UndeletedCount);
    end;

    function Process(e: IInterface): integer;
    var
        Sig: string;
    begin
        Result := 0;

{
    settingReapply := false;
    settingDummyPrecomb := false;
}

        if (not IsEditable(e)) then Exit;
        Sig := Signature(e);

        if (not GetIsDeleted(e)) then begin
            if(settingReapply) then begin
                if(Sig = 'NAVM') then begin
                    if(isNavmeshPseudoDeleted(e)) then begin
                        AddMessage('Reapplying: ' + Name(e));
                        processNavm(e);
                    end;
                end else begin
                    if(isReferencePseudoDeleted(e)) then begin
                        AddMessage('Reapplying: ' + Name(e));
                        processReference(e, true);
                    end;
                end;


            end;
            Exit;
        end;


        if
            (Sig <> 'REFR') and
            (Sig <> 'PGRE') and
            (Sig <> 'PMIS') and
            (Sig <> 'ACHR') and
            (Sig <> 'ACRE') and
            (Sig <> 'NAVM') and
            (Sig <> 'PARW') and // Skyrim
            (Sig <> 'PBAR') and // Skyrim
            (Sig <> 'PBEA') and // Skyrim
            (Sig <> 'PCON') and // Skyrim
            (Sig <> 'PFLA') and // Skyrim
            (Sig <> 'PHZD')     // Skyrim
        then Exit;

        AddMessage('Undeleting: ' + Name(e));

        // undelete
        // can't remember why setting it to true first, but I think there was a reason for it
        SetIsDeleted(e, True);
        SetIsDeleted(e, False);

        if Sig = 'NAVM' then begin
            processNavm(e);
            Exit;
        end;

        processReference(e, false);
    end;

    function Finalize: integer;
    var
        i: integer;
        curCell: IInterface;
        cellText: string;
    begin
        AddMessage('Undeleted Records: ' + IntToStr(UndeletedCount));

        if(cellsToRefinalize.count > 0) then begin
            AddMessage('=== IMPORTANT ===');
            AddMessage('Navmeshes have been updated.');

            AddMessage('Navmeshes must be finalized in the following cells.');
            AddMessage('-----------------');
            for i:=0 to cellsToRefinalize.count-1 do begin
                curCell := ObjectToElement(cellsToRefinalize.Objects[i]);
                cellText := ' - ['+IntToHex(FormID(curCell), 8) +'] ' + EditorID(curCell)+' "'+DisplayName(curCell)+'"';

                AddMessage(cellText);
            end;
            if(settingZCoordMode = 0) then begin
                AddMessage('-----------------');
                AddMessage('Also, since "Set Z Coordinate" was set to "Do Not Change", make sure to check if the autogenerated tiny navmesh triangles aren''t placed within other navmeshes.');
            end;
            AddMessage('=================');

        end;
        cellsToRefinalize.free();
    end;

end.