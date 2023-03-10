{
  Undelete and disable references, almost the copy of internal
  xEdit code.
  Does not require applied filter.
}
unit UndeleteStuff;

    var
        UndeletedCount, NAVMCount: integer;

    procedure removeAllChildren(e: IInterface);
    begin
        // try this
        while(ElementCount(e) > 0) do begin
            //AddMessage('removing something');
            RemoveElement(e, 0);
        end;
    end;

    procedure processNavm(navm: IInterface);
    var
        i, numVertices: integer;
        meanX, meanY, meanZ, p1x, p2x, p3x, p1y, p2y, p3y: float;
        vertices, triangles, curV, curTri, nvmn, grid, gridArrays, cellWhat: IInterface;

    begin
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
            AddMessage('AAAA');
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
            AddMessage('AAAA');
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

    function Process(e: IInterface): integer;
    var
        Sig: string;
        xesp: IInterface;
        prevZpos: integer;
    begin
        Result := 0;

        if not (IsEditable(e) and GetIsDeleted(e)) then Exit;

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

        // set persistence flag depending on game <- BUT WHY?!
        if (wbGameMode = gmFO3) or (wbGameMode = gmFNV) or (wbGameMode = gmTES5) and ((Sig = 'ACHR') or (Sig = 'ACRE')) then
            SetIsPersistent(e, True)
        else if wbGameMode = gmTES4 then
            SetIsPersistent(e, False);

        // place it below the ground <- why not if persistent?
        // if not GetIsPersistent(e) then begin
            prevZpos := GetElementNativeValues(e, 'DATA\Position\Z');
            // SetElementNativeValues(e, 'DATA\Position\Z', prevZpos - 30000);
            SetElementNativeValues(e, 'DATA\Position\Z', 30000);
        // end;

        RemoveElement(e, 'Enable Parent');
        RemoveElement(e, 'XTEL');
        // ... remove anything else here
        // linked refs maybe
        RemoveElement(e, 'Linked References');


        // set to disabled
        SetIsInitiallyDisabled(e, True);

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