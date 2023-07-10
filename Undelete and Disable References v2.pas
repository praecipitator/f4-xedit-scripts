{
  Undelete and disable references, almost the copy of internal
  xEdit code.
  Does not require applied filter.
}
unit UndeleteStuff;
    uses praUtil;
    const
        configFile = ProgramPath + 'Edit Scripts\Undelete and Disable References v2.cfg';

    var
        UndeletedCount: integer;
        settingProcessNavmeshes, settingMoveToLayer: boolean;
        settingZCoordMode: integer; // 0 => nothing, 1 => set to -30k, 2 => subtract -30k
        settingTargetLayerName: string;
        targetLayer: IInterface;

    procedure loadConfig();
    var
        i, j, breakPos: integer;
        curLine, curKey, curVal: string;
        lines : TStringList;
    begin
        // default
        settingProcessNavmeshes := true;
        settingMoveToLayer := false;
        settingZCoordMode := 1;
        settingTargetLayerName := 'deleted';


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
                end else if(curKey = 'ProcessNavmeshes') then begin
                    settingProcessNavmeshes := StrToBool(curVal);
                end else if(curKey = 'MoveToLayer') then begin
                    settingMoveToLayer := StrToBool(curVal);
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
        lines.add('ProcessNavmeshes='+BoolToStr(settingProcessNavmeshes));
        lines.add('MoveToLayer='+BoolToStr(settingMoveToLayer));
        lines.add('ZCoordMode='+IntToStr(settingZCoordMode));

        lines.saveToFile(configFile);
        lines.free();
    end;


    function showConfigDialog(): boolean;
    var
        frm: TForm;
        cbProcessNavmeshes, cbMoveToLayer: TCheckBox;
        rgMoveMode: TRadioGroup;
        items: TStringList;
        editLayer: TEdit;
        resultCode: cardinal;
        btnOkay, btnCancel: TButton;

    begin
        loadConfig();
        Result := false;
        frm := CreateDialog('Undelete and Disable Settings', 350, 250);

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

        cbMoveToLayer := CreateCheckbox(frm, 10, 120, 'Move deleted to layer');
        if(settingMoveToLayer) then begin
            cbMoveToLayer.checked := true;
        end;

        CreateLabel(frm, 10, 142, 'Layer:');
        editLayer := CreateInput(frm, 50, 140, settingTargetLayerName);
        editLayer.width := 250;

        btnOkay := CreateButton(frm, 90, 170, '  OK  ');
        btnOkay.ModalResult := mrYes;
        btnOkay.Default := true;

        btnCancel := CreateButton(frm, 180, 170, 'Cancel');
        btnCancel.ModalResult := mrCancel;

        resultCode := frm.showModal();

        if(resultCode = mrYes) then begin
            settingProcessNavmeshes := cbProcessNavmeshes.checked;
            settingMoveToLayer := cbMoveToLayer.checked;

            settingZCoordMode := rgMoveMode.ItemIndex;
            settingTargetLayerName := editLayer.text;
            saveConfig();
            Result := true;
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

    function getLayer(inFile: IInterface; layerName: string; checkMasters: boolean): IInterface;
    var
        curMaster, myLayrGroup, foundLayer: IInterface;
        i: integer;
    begin
        myLayrGroup := AddGroupBySignature(inFile, 'LAYR');
        foundLayer := MainRecordByEditorID(myLayrGroup, layerName);
        Result := nil;

        if(assigned(foundLayer)) then begin
            Result := getOverriddenForm(foundLayer, inFile);
            //Result := foundLayer;
            exit;
        end;


        if (checkMasters) then begin
            for i:=0 to MasterCount(inFile)-1 do begin

                curMaster := MasterByIndex(inFile, i);

                foundLayer := MainRecordByEditorID(GroupBySignature(curMaster, 'LAYR'), layerName);

                if (assigned(foundLayer)) then begin
                    Result := getOverriddenForm(foundLayer, inFile);
                    exit;
                end;

            end;
        end;

        // create new
        foundLayer := Add(myLayrGroup, 'LAYR', true);//ensurePath(myLayrGroup, 'LAYR');
        setElementEditValues(foundLayer, 'EDID', layerName);


        Result := foundLayer;
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

        // point1

        p1x := meanX;
        p1y := meanY;

        p2x := meanX + 10;
        p2y := meanY;

        p3x := meanX;
        p3y := meanY + 10;

        triangles := ElementByPath(nvmn, 'Triangles');
        if(not assigned(triangles)) then begin
            exit;
        end;
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

        Inc(UndeletedCount);
    end;

    function Initialize: integer;
    begin
        Result := 0;
        if(not showConfigDialog()) then begin
            Result := 1;
            exit;
        end;
    end;

    function Process(e: IInterface): integer;
    var
        Sig: string;
        xesp: IInterface;
        prevZpos: integer;
    begin
        Result := 0;

        if(settingMoveToLayer) then begin
            targetLayer := getLayer(GetFile(e), settingTargetLayerName, true);
        end;

        if (not IsEditable(e)) then Exit;

        if (not GetIsDeleted(e)) then begin
            Exit;
        end;

        Sig := Signature(e);

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
        SetIsDeleted(e, True);
        SetIsDeleted(e, False);

        if Sig = 'NAVM' then begin
            processNavm(e);
            // Inc(NAVMCount);
            Exit;
        end;

        {
        // set persistence flag depending on game <- BUT WHY?!
        if (wbGameMode = gmFO3) or (wbGameMode = gmFNV) or (wbGameMode = gmTES5) and ((Sig = 'ACHR') or (Sig = 'ACRE')) then
            SetIsPersistent(e, True)
        else if wbGameMode = gmTES4 then
            SetIsPersistent(e, False);
        }

        if(settingZCoordMode = 1) then begin
            SetElementNativeValues(e, 'DATA\Position\Z', 30000);
        end else if(settingZCoordMode = 2) then begin
            prevZpos := GetElementNativeValues(e, 'DATA\Position\Z');
            SetElementNativeValues(e, 'DATA\Position\Z', prevZpos - 30000);
        end;

        RemoveElement(e, 'Enable Parent');
        RemoveElement(e, 'XTEL');
        // ... remove anything else here
        // linked refs maybe
        RemoveElement(e, 'Linked References');


        // set to disabled
        SetIsInitiallyDisabled(e, True);

        // set layer
        if(settingMoveToLayer) then begin
            // targetLayer := getLayer(settingTargetLayerName);
            setPathLinksTo(e, 'XLYR', targetLayer);
        end;

        // add enabled opposite of player (true - silent)
        xesp := Add(e, 'XESP', True);
        if Assigned(xesp) then begin
            SetElementNativeValues(xesp, 'Reference', $14); // Player ref
            SetElementNativeValues(xesp, 'Flags', 1);  // opposite of parent flag
        end;

        Inc(UndeletedCount);
    end;

    function Finalize: integer;
    begin
        AddMessage('Undeleted Records: ' + IntToStr(UndeletedCount));
    end;

end.